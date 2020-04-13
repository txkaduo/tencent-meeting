module Tencent.Meeting.REST where

-- {{{1
import           ClassyPrelude
import           Control.Lens hiding ((.=))
import           Control.Lens.TH (makeLenses)
import           Control.Monad.Logger
import           Data.Aeson as A
import           Data.Aeson.TH                 (deriveJSON, fieldLabelModifier, defaultOptions)
import           Data.Aeson.Types              (camelTo2)
import qualified Data.ByteString as B
import           Data.Byteable (toBytes)
import           Data.Default (Default(..))
import           Data.Time.Clock.POSIX
import           System.Random (randomIO)
import qualified Data.ByteString.Lazy as LB
import qualified Crypto.Hash as CH
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base64 as B64
import qualified Network.HTTP.Client    as HC
import           Network.HTTP.Types (Status(..), hContentType)
import           Network.Wreq.Types (FormValue(..), FormParam(..))

import           Tencent.Meeting.Types
import           Tencent.Meeting.Error
-- }}}1


type HttpCallMonad m = (MonadIO m, MonadLogger m, MonadReader HC.Manager m)

type RestApiMonad m a = (HttpCallMonad m) => ReaderT (AppId, SecretId, SecretKey) m (RestResponse a)

logSourceName :: Text
logSourceName = "tx-meeting"

apiHost :: ByteString
apiHost = "api.meeting.qq.com"


apiBaseUrlPath :: ByteString
apiBaseUrlPath = "/v1/"


data RestPublicParams = RestPublicParams
  { _rpAppId     :: AppId
  , _rpSecretId  :: SecretId
  , _rpSdkId     :: Maybe SdkId
  , _rpAction    :: Maybe Text
  , _rpRegion    :: Maybe Text
  , _rpTimestamp :: Timestamp
  , _rpNonce     :: Word32
  , _rpVersion   :: Maybe Text
  , _rpToken     :: Maybe Token
  }
$(makeLenses 'RestPublicParams)

newRestPublicParams :: MonadIO m => AppId -> SecretId -> m RestPublicParams
newRestPublicParams app_id secret_id = do
  ts <- liftIO $ timestampFromPOSIXTime <$> getPOSIXTime
  nonce <- liftIO randomIO
  pure $ RestPublicParams app_id secret_id Nothing Nothing Nothing ts nonce Nothing Nothing


restHttpHeaders :: RestPublicParams -> [(Text, Text)]
restHttpHeaders (RestPublicParams {..}) = catMaybes
  [ ("X-TC-Action", ) <$> _rpAction
  , ("X-TC-Region", ) <$> _rpRegion
  , pure ("X-TC-Key", unSecretId _rpSecretId)
  , pure ("X-TC-Timestamp", tshow (unTimestamp _rpTimestamp))
  , pure ("X-TC-Nonce", tshow _rpNonce)
  , ("X-TC-Version", ) <$> _rpVersion
  , ("X-TC-Token", ) . unToken <$> _rpToken
  , pure ("AppId", unAppId _rpAppId)
  , ("SdkId", ) . unSdkId  <$> _rpSdkId
  ]


restHttpHeadersToSign :: RestPublicParams -> [(Text, Text)]
restHttpHeadersToSign (RestPublicParams {..}) =
  [ ("X-TC-Key", unSecretId _rpSecretId)
  , ("X-TC-Timestamp", tshow (unTimestamp _rpTimestamp))
  , ("X-TC-Nonce", tshow _rpNonce)
  ]


-- | 生成签名串
-- 文档没明确讲，但实测表明 query string 是要算在 URI 那一段里的
restStringToSign :: RestPublicParams -> HttpMethod -> ByteString -> ByteString -> Maybe LB.ByteString -> ByteString
-- {{{1
restStringToSign public_params http_method url_path query_string m_body =
  intercalate "\n"
    [ case http_method of
        HttpGet -> "GET"
        HttpPost -> "POST"
    , encodeUtf8 $ intercalate "&" $ map header_line $ sortWith fst sign_headers
    , url_path <> query_string'
    , toStrict $ fromMaybe mempty m_body
    ]
  where header_line (x, y) = x <> "=" <> y
        sign_headers = restHttpHeadersToSign public_params
        qmark = fromIntegral (fromEnum '?')
        query_string' = if not (null query_string)
                           then if B.head query_string == qmark
                                   then query_string
                                   else B.cons qmark query_string
                           else query_string
-- }}}1


restSignature :: SecretKey -> RestPublicParams -> HttpMethod -> ByteString -> ByteString -> Maybe LB.ByteString -> Text
-- {{{1
restSignature (SecretKey secret_key) public_params http_method url_path query_string m_body =
  decodeUtf8 $ B64.encode $ B16.encode $ toBytes hmac
  where
    string_to_sign = restStringToSign public_params http_method url_path query_string m_body

    hmac :: CH.HMAC CH.SHA256
    hmac = CH.hmac (encodeUtf8 secret_key) string_to_sign
-- }}}1


restApiCallReal :: FromJSON a
                => ByteString
                -> [(ByteString, Maybe ByteString)] -- ^ query string
                -> Maybe LB.ByteString  -- ^ Nothing: http get; Just _: http post
                -> RestApiMonad m a
-- {{{1
restApiCallReal url_sub_path qs_items m_body = do
  manager <- lift ask
  (app_id, secret_id, secret_key) <- ask
  def_public_params <- newRestPublicParams app_id secret_id
  let public_params = def_public_params
  let public_headers = restHttpHeaders public_params
  let sign = restSignature secret_key public_params http_method url_path (HC.queryString req0) m_body
  let extra_headers = (hContentType, "application/json") : map ((fromString . unpack) *** encodeUtf8) (("X-TC-Signature", sign) : public_headers)
  let req = add_body $
            req0
              { HC.host = apiHost
              , HC.secure = True
              , HC.port = 443
              , HC.path = url_path
              , HC.method = bool "POST" "GET" (isNothing m_body)
              , HC.requestHeaders = extra_headers
              }

  $logDebugS logSourceName $ "req=" <> tshow req

  resp <- liftIO $ HC.httpLbs req manager
  let status = HC.responseStatus resp
  let lbs = HC.responseBody resp

  case lookup hContentType (HC.responseHeaders resp) of
    Just "application/json" -> return ()
    Nothing -> return ()
    Just ct -> do
      $logErrorS logSourceName $ "Got non json response: " <> decodeUtf8 ct
      throwIO $ SomeRestApiException url_sub_path http_method (Just status) (Just lbs) $
                  userError $ "Unexpected response content-type: " <> unpack (decodeUtf8 ct)

  case statusCode status of
    200 -> do
      case eitherDecode lbs of
        Right x -> pure $ Right x
        Left err -> do
          $logErrorS logSourceName $ "Failed to parse JSON body: " <> fromString err
          throwIO $ RestApiJsonError url_sub_path http_method status lbs err

    _ -> do
      $logErrorS logSourceName $ "Got non-200 response: " <> tshow status

      case eitherDecode lbs of
        Right x -> pure $ Left x
        Left err -> do
          $logErrorS logSourceName $ "Failed to parse JSON body: " <> fromString err
          throwIO $ RestApiJsonError url_sub_path http_method status lbs err

  where http_method = bool HttpPost HttpGet (isNothing m_body)

        url_path = apiBaseUrlPath <> url_sub_path

        req0 = add_qs $ HC.defaultRequest

        add_qs = if null qs_items
                    then id
                    else HC.setQueryString qs_items

        add_body = case m_body of
                     Nothing -> id
                     Just lbs -> \ x -> x { HC.requestBody = HC.RequestBodyLBS lbs }
-- }}}1


restApiCall :: (FromJSON a, ToJSON b)
            => ByteString
            -> [FormParam] -- ^ query string
            -> Maybe b
            -> RestApiMonad m a
restApiCall url_sub_path get_params m_body = do
  restApiCallReal url_sub_path qs_items m_body'
  where from_form_item (k := v) = (k, Just (renderFormValue v))
        qs_items = map from_form_item get_params
        m_body' = fmap A.encode m_body


-- | 多个接口有用到类似以下结构的报文，但又不完全一样
data CreateMeetingInfo = CreateMeetingInfo
  { _cmiSubject      :: Text
  , _cmiMeetingId    :: MeetingId
  , _cmiMeetingCode  :: MeetingCode
  , _cmiPassword     :: Maybe Text
  , _cmiHosts        :: [UserObj]
  , _cmiParticipants :: Maybe [UserObj]
  , _cmiStartTime    :: Timestamp
  , _cmiEndTime      :: Timestamp
  , _cmiJoinUrl      :: Text
  , _cmiSettings     :: MeetingSettings
  }
$(makeLenses ''CreateMeetingInfo)
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }) ''CreateMeetingInfo)


-- | 这个结构的报文用在多个接口的返回，创建会议，查询用户的会议列表等等
data MeetingInfoList a = MeetingInfoList
  { _milMeetingNumber   :: Int
  , _milMeetingInfoList :: [a]
  }
$(makeLenses ''MeetingInfoList)
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }) ''MeetingInfoList)

data CreateMeetingOptions = CreateMeetingOptions
  { _cmoHosts    :: [UserObj]
  , _cmoInvitees :: [UserObj]
  , _cmoPassword :: Maybe Text
  , _cmoSettings :: MeetingSettings
  }
$(makeLenses ''CreateMeetingOptions)

instance Default CreateMeetingOptions where
  def = CreateMeetingOptions [] [] Nothing def


-- | 创建会议
restCreateMeeting :: MeetingType
                  -> UserId
                  -> DeviceType
                  -> Text -- ^ subject
                  -> (Timestamp, Timestamp)
                  -> CreateMeetingOptions
                  -> RestApiMonad m (MeetingInfoList CreateMeetingInfo)
-- {{{1
restCreateMeeting meeting_type user_id dev_type subject (start_time, end_time) opts = do
  restApiCall "meetings" [] (Just body)
  where
    optional_list lst = do
      guard $ not $ null lst
      Just lst

    body = object $ catMaybes
      [ pure $ "userid"     .= user_id
      , pure $ "instanceid" .= dev_type
      , pure $ "subject"    .= subject
      , pure $ "type"       .= meeting_type
      , pure $ "start_time" .= start_time
      , pure $ "end_time"   .= end_time
      , pure $ "settings"   .= (opts ^. cmoSettings)
      , ("password" .=) <$> opts ^. cmoPassword
      , ("hosts" .=) <$> optional_list (opts ^. cmoHosts)
      , ("invitees" .=) <$> optional_list (opts ^. cmoInvitees)
      ]
-- }}}1


-- | 取消会议
restCancelMeeting :: MeetingId -> UserId -> DeviceType -> CancelReasonCode -> Maybe Text -> RestApiMonad m ()
restCancelMeeting meeting_id user_id dev_type code m_msg = do
  restApiCall url_sub_path [] (Just body)
  where
    url_sub_path = "meetings/" <> renderFormValue meeting_id <> "/cancel"
    body = object $ catMaybes
      [ pure $ "userid"     .= user_id
      , pure $ "instanceid" .= dev_type
      , pure $ "reason_code" .= code
      , ("reason_detail" .=) <$> m_msg
      ]


data UpdateMeetingOpts = UpdateMeetingOpts
  { _umoHosts     :: Maybe [UserObj]
  , _umoInvitees  :: Maybe [UserObj]
  , _umoStartTime :: Maybe Timestamp
  , _umoEndTime   :: Maybe Timestamp
  , _umoPassword  :: Maybe Text
  , _umoSettings  :: Maybe MeetingSettings
  }
$(makeLenses ''UpdateMeetingOpts)
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }) ''UpdateMeetingOpts)

instance Default UpdateMeetingOpts where
  def = UpdateMeetingOpts Nothing Nothing Nothing Nothing Nothing Nothing


data UpdateMeetingInfo = UpdateMeetingInfo
  { _umiMeetingId    :: MeetingId
  , _umiMeetingCode  :: MeetingCode
  }
$(makeLenses ''UpdateMeetingInfo)
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }) ''UpdateMeetingInfo)

-- | 修改会议
restUpdateMeeting :: MeetingId
                  -> UserId
                  -> DeviceType
                  -> Text -- ^ subject
                  -> UpdateMeetingOpts
                  -> RestApiMonad m (MeetingInfoList UpdateMeetingInfo)
-- {{{1
restUpdateMeeting meeting_id user_id dev_type subject (UpdateMeetingOpts {..}) = do
  restApiCall url_sub_path qs_items (Just body)
  where
    url_sub_path = "meetings/" <> renderFormValue meeting_id
    qs_items = []

    body = object $ catMaybes
      [ pure $ "userid"     .= user_id
      , pure $ "instanceid" .= dev_type
      , pure $ "subject" .= subject
      , ("hosts" .=) <$> _umoHosts
      , ("invitees" .=) <$> _umoInvitees
      , ("start_time" .=) <$> _umoStartTime
      , ("end_time" .=) <$> _umoEndTime
      , ("password" .=) <$> _umoPassword
      , ("settings" .=) <$> _umoSettings
      ]
-- }}}1


data QueryMeetingInfo = QueryMeetingInfo
  { _qmiSubject      :: Text
  , _qmiMeetingId    :: MeetingId
  , _qmiMeetingCode  :: MeetingCode
  , _qmiPassword     :: Maybe Text
  , _qmiHosts        :: [UserObj]
  , _qmiParticipants :: Maybe [UserObj]
  , _qmiStartTime    :: Timestamp
  , _qmiEndTime      :: Timestamp
  }
$(makeLenses ''QueryMeetingInfo)
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }) ''QueryMeetingInfo)

-- | 通过会议 ID 查询
resetQueryMeetingById :: MeetingId -> UserId -> DeviceType -> RestApiMonad m (MeetingInfoList QueryMeetingInfo)
resetQueryMeetingById meeting_id user_id dev_type = do
  restApiCall url_sub_path qs_items (Nothing :: Maybe Value)
  where
    url_sub_path = "meetings/" <> renderFormValue meeting_id
    qs_items = [ "userid" := user_id
               , "instanceid" := dev_type
               ]


-- | 通过会议 Code 查询
resetQueryMeetingByCode :: MeetingCode -> UserId -> DeviceType -> RestApiMonad m (MeetingInfoList QueryMeetingInfo)
resetQueryMeetingByCode meeting_code user_id dev_type = do
  restApiCall url_sub_path qs_items (Nothing :: Maybe Value)
  where
    url_sub_path = "meetings"
    qs_items = [ "userid" := user_id
               , "instanceid" := dev_type
               , "meetingCode" := meeting_code
               ]


data Participant = Participant
  { _pUserid   :: UserId
  , _pJoinTime :: Timestamp
  , _pLeftTime :: ZeroOrTimestamp
  -- ^ 0 stands for Nothing
  }
$(makeLenses ''Participant)
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }) ''Participant)

data QueryParticipantsOutput = QueryParticipantsOutput
  { _qpMeetingId         :: MeetingId
  , _qpMeetingCode       :: MeetingCode
  , _qpSubject           :: Text
  , _qpScheduleStartTime :: Timestamp
  , _qpScheduleEndTime   :: Timestamp
  , _qpParticipants      :: [Participant]
  }
$(makeLenses ''QueryParticipantsOutput)
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3 }) ''QueryParticipantsOutput)

-- | 获取参会成员列表
restQueryMeetingParticipants :: MeetingId -> UserId -> RestApiMonad m QueryParticipantsOutput
restQueryMeetingParticipants meeting_id user_id = do
  restApiCall url_sub_path qs_items (Nothing :: Maybe Value)
  where
    url_sub_path = "meetings/" <> renderFormValue meeting_id <> "/participants"
    qs_items = [ "userid" := user_id ]


-- | 查询用户的会议列表
restQueryMeetingsOfUser :: UserId -> DeviceType -> RestApiMonad m (MeetingInfoList QueryMeetingInfo)
restQueryMeetingsOfUser user_id dev_type = do
  restApiCall url_sub_path qs_items (Nothing :: Maybe Value)
  where
    url_sub_path = "meetings"
    qs_items = [ "userid" := user_id
               , "instanceid" := dev_type
               ]


-- vim: set foldmethod=marker:
