{-# OPTIONS_GHC -pgmP cc -optP -E -optP -undef -optP -std=c89 #-}
module Tencent.Meeting.Types where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Reader (reader)
import           Control.Lens.TH (makeLenses)
import           Data.Aeson as A
import           Data.Aeson.TH                 (deriveJSON, fieldLabelModifier, defaultOptions)
import           Data.Aeson.Types              (camelTo2)
import           Data.Time.Clock.POSIX
import           Database.Persist.Sql  (PersistField (..), PersistFieldSql (..))
import           Data.Default (Default(..))
import           Text.Blaze.Html       (ToMarkup (..))
import           Text.Shakespeare.I18N (ToMessage (..))
import           Safe (toEnumMay)
import           Network.Wreq.Types (FormValue(..))
-- }}}1


#define NEWTYPE_TEXT_DERIVING \
  deriving (Show, Eq, Ord, Typeable, ToMessage, ToMarkup \
           , PersistField, PersistFieldSql \
           , ToJSON, FromJSON \
           , FormValue \
           )

#define NEWTYPE_DEF(t1, t2) newtype t1 = t1 { un ## t1 :: t2 }

#define NEWTYPE_DEF_TEXT(t1) NEWTYPE_DEF(t1, Text) NEWTYPE_TEXT_DERIVING

#define INSTANCES_BY_SHOW_READ(t) \
instance ParamValue t where { toParamValue = tshow . un ## t } ; \
instance ToJSON t where { toJSON = toJSON . un ## t }; \
instance FromJSON t where { \
  parseJSON (A.String s) = maybe mzero (return . t) $ readMay s ;\
  parseJSON v = fmap t $ parseJSON v; \
                          }


data ErrorInfo = ErrorInfo
  { _errInfoErrorCode :: Int
  , _errInfoMessage   :: Text
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }) ''ErrorInfo)
$(makeLenses ''ErrorInfo)

data ErrorResponse = ErrorResponse
  { _errRespErrorInfo :: ErrorInfo
  }

$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 8 }) ''ErrorResponse)
$(makeLenses ''ErrorResponse)


type RestResponse a = Either ErrorResponse a


NEWTYPE_DEF_TEXT(AppId)
NEWTYPE_DEF_TEXT(SdkId)
NEWTYPE_DEF_TEXT(SecretId)
NEWTYPE_DEF_TEXT(SecretKey)
NEWTYPE_DEF_TEXT(Token)

NEWTYPE_DEF_TEXT(MeetingId)
NEWTYPE_DEF_TEXT(MeetingCode)
NEWTYPE_DEF_TEXT(UserId)


-- | 用户终端设备类型. (aka. instanceid)
data DeviceType = DeviceTypePc
                | DeviceTypeMac
                | DeviceTypeAndroid
                | DeviceTypeIos
                | DeviceTypeWeb
                | DeviceTypeIpad
                | DeviceTypeAndroidPad
                | DeviceTypeMiniProgram
                deriving (Show, Eq, Ord, Bounded)

-- {{{1 instances
instance Enum DeviceType where
  fromEnum DeviceTypePc          = 1
  fromEnum DeviceTypeMac         = 2
  fromEnum DeviceTypeAndroid     = 3
  fromEnum DeviceTypeIos         = 4
  fromEnum DeviceTypeWeb         = 5
  fromEnum DeviceTypeIpad        = 6
  fromEnum DeviceTypeAndroidPad  = 7
  fromEnum DeviceTypeMiniProgram = 8

  toEnum 1 = DeviceTypePc
  toEnum 2 = DeviceTypeMac
  toEnum 3 = DeviceTypeAndroid
  toEnum 4 = DeviceTypeIos
  toEnum 5 = DeviceTypeWeb
  toEnum 6 = DeviceTypeIpad
  toEnum 7 = DeviceTypeAndroidPad
  toEnum 8 = DeviceTypeMiniProgram
  toEnum x = error $ "cannot convert to DeviceType: " <> show x

instance ToJSON DeviceType where
  toJSON = toJSON . fromEnum

instance FromJSON DeviceType where
  parseJSON = fmap toEnumMay . parseJSON >=> maybe mzero pure

instance FormValue DeviceType where
  renderFormValue = renderFormValue . fromEnum
-- }}}1


data MeetingType = BookedMeeting  -- ^ 预约会议
                 | QuickMeeting   -- ^ 快速会议
                 deriving (Show, Eq, Ord, Bounded)

-- {{{1 instances
instance Enum MeetingType where
  fromEnum BookedMeeting = 0
  fromEnum QuickMeeting  = 1

  toEnum 0 = BookedMeeting
  toEnum 1 = QuickMeeting
  toEnum x = error $ "cannot convert to MeetingType: " <> show x

instance ToJSON MeetingType where
  toJSON = toJSON . fromEnum

instance FromJSON MeetingType where
  parseJSON = fmap toEnumMay . parseJSON >=> maybe mzero pure

instance FormValue MeetingType where
  renderFormValue = renderFormValue . fromEnum
-- }}}1


-- | 取消会议时的原因代码
NEWTYPE_DEF(CancelReasonCode, Int)
  deriving (Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           , FromJSON, ToJSON, FormValue
           )


NEWTYPE_DEF(Timestamp, Int64)
  deriving (Eq, Ord, Typeable, ToMarkup
           , PersistField, PersistFieldSql
           )

-- {{{1
instance ToJSON Timestamp where toJSON = toJSON . tshow . unTimestamp

instance FromJSON Timestamp where
  parseJSON (A.String s) = maybe mzero (return . Timestamp) $ readMay s
  parseJSON v            = fmap Timestamp $ parseJSON v

instance Show Timestamp where show = showTimeStamp

instance FormValue Timestamp where
  renderFormValue = renderFormValue . unTimestamp
-- }}}1


timestampFromPOSIXTime :: POSIXTime -> Timestamp
timestampFromPOSIXTime = Timestamp . round


timestampToPOSIXTime :: Timestamp -> POSIXTime
timestampToPOSIXTime = fromIntegral . unTimestamp


showTimeStamp :: Timestamp -> String
showTimeStamp ts = show (posixSecondsToUTCTime ept)
  where ept = timestampToPOSIXTime ts


-- | 有时候报文用零代表无此时间之意
newtype ZeroOrTimestamp = ZeroOrTimestamp { unZeroOrTimestamp :: Maybe Timestamp }

-- {{{1 instances
instance ToJSON ZeroOrTimestamp where
  toJSON (ZeroOrTimestamp Nothing) = toJSON (Nothing :: Maybe Int64)
  toJSON (ZeroOrTimestamp (Just t)) = toJSON t

instance FromJSON ZeroOrTimestamp where
  parseJSON v = do
    as_int <|> fmap ZeroOrTimestamp (parseJSON v)
    where as_int = do
            i <- parseJSON v
            if i == 0
               then pure (ZeroOrTimestamp Nothing)
               else pure (ZeroOrTimestamp $ Just $ Timestamp i)
-- }}}1


data UserObj = UserObj
  { _userObjUserid       :: UserId
  , _userObjIsAnonymous  :: Bool
  , _userObjNickname     :: Maybe Text
  , _userObjProfilePhoto :: Maybe Text
  }

-- {{{1
$(makeLenses ''UserObj)

instance Default (Reader UserId UserObj) where
  def = reader $ \ user_id -> UserObj user_id False Nothing Nothing

instance ToJSON UserObj where
  toJSON (UserObj {..}) = object $ catMaybes $
    [ pure $ "userid" .= _userObjUserid
    , pure $ "is_anonymous" .= _userObjIsAnonymous
    , ("nick_name" .=) <$> _userObjNickname
    , ("profile_photo" .=) <$> _userObjProfilePhoto
    ]

instance FromJSON UserObj where
  parseJSON = withObject "UserObj" $ \ o -> do
                UserObj <$> o .: "userid"
                        <*> o .:? "is_anonymous" .!= True
                        <*> o .:? "nick_name"
                        <*> o .:? "profile_photo"
-- }}}1


-- | 会议媒体参数配置对象
-- 这个结构的报文出现在多处，同时出现在输入和输出
-- 在不同接口或输入输出的不同处，字段可选的标识不同. 关键是可选的字段缺失时被如何理解并无说明
-- 实测表明，全部当成可选，从 haskell 世界看是没问题的
data MeetingSettings = MeetingSettings
  { _msMuteEnableJoin   :: Maybe Bool
  , _msAllowUnmuteSelf  :: Maybe Bool
  , _msMuteAll          :: Maybe Bool
  , _msHostVideo        :: Maybe Bool
  , _msParticipantVideo :: Maybe Bool
  , _msEnableRecord     :: Maybe Bool
  , _msPlayIvrOnLeave   :: Maybe Bool
  , _msPlayIvrOnJoin    :: Maybe Bool
  , _msLiveUrl          :: Maybe Bool
  }

$(makeLenses ''MeetingSettings)
-- 注意: Nothing的字段不要encode
$(deriveJSON (defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3, omitNothingFields = True }) ''MeetingSettings)

instance Default MeetingSettings where
  def = MeetingSettings Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing


data HttpMethod = HttpGet
                | HttpPost
                deriving (Show, Eq, Ord, Bounded)


-- vim: set foldmethod=marker:
