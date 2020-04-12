module Tencent.Meeting.Error where

import           ClassyPrelude
import qualified Data.ByteString.Lazy as LB
import           Data.Typeable
import           Network.HTTP.Types (Status(..))

import           Tencent.Meeting.Types


data SomeTencentMeetingException = forall e. Exception e => SomeTencentMeetingException e

instance Show SomeTencentMeetingException where
  show (SomeTencentMeetingException e) = show e

instance Exception SomeTencentMeetingException

tencentMeetingExceptionToException :: Exception e => e -> SomeException
tencentMeetingExceptionToException = toException . SomeTencentMeetingException

tencentMeetingExceptionFromException :: Exception e => SomeException -> Maybe e
tencentMeetingExceptionFromException x = do
  SomeTencentMeetingException a <- fromException x
  cast a


showRestApiError :: ByteString -> HttpMethod -> Maybe Status -> Maybe LB.ByteString -> String -> String
showRestApiError url_sub_path http_method m_status m_body err_msg =
  "when " <> http_method_name <> " to " <> unpack (decodeUtf8 url_sub_path) <> ": " <> pack err_msg
      <> maybe "" ( \ x -> "(" <> show x <> ")" ) m_status
      <> show_body
  where http_method_name = case http_method of
                             HttpGet -> "GET"
                             HttpPost -> "POST"

        show_body = fromMaybe "" $ flip fmap m_body $ \ lbs -> "\nresponse body was:\n" <> unpack (decodeUtf8 lbs)


data SomeRestApiException = forall e. Exception e
                              => SomeRestApiException
                                  ByteString  -- ^ url sub path
                                  HttpMethod
                                  (Maybe Status)  -- ^ http status of response
                                  (Maybe LB.ByteString) -- ^ body of response
                                  e

instance Show SomeRestApiException where
  show (SomeRestApiException url_sub_path http_method m_status m_body e) =
    showRestApiError url_sub_path http_method m_status m_body (show e)


instance Exception SomeRestApiException where
  toException = tencentMeetingExceptionToException
  fromException = tencentMeetingExceptionFromException


data RestApiJsonError = RestApiJsonError
                          ByteString  -- ^ url sub path
                          HttpMethod
                          Status
                          LB.ByteString
                          String

instance Show RestApiJsonError where
  show (RestApiJsonError url_sub_path http_method status body e) =
    showRestApiError url_sub_path http_method (Just status) (Just body) e

instance Exception RestApiJsonError where
  toException exc@(RestApiJsonError url_sub_path http_method status body _) =
    toException $ SomeRestApiException url_sub_path http_method (Just status) (Just body) exc

  fromException x = do
    SomeRestApiException url_sub_path http_method m_status m_body exc <- fromException x
    json_exc@(RestApiJsonError url_sub_path2 http_method2 status2 body2 _) <- cast exc
    guard $ url_sub_path == url_sub_path2
    guard $ http_method == http_method2
    guard $ m_status == Just status2
    guard $ m_body == Just body2
    pure json_exc


-- vim: set foldmethod=marker:
