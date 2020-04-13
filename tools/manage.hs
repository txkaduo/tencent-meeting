module Main where

-- {{{1 imports
import           ClassyPrelude
import           Control.Monad.Logger
import qualified Data.Aeson.Encode.Pretty as AP
import           Data.Default (def)
import           Data.Time
import           Data.Time.Clock.POSIX
import qualified Network.HTTP.Client.TLS  as HC
import           Options.Applicative
import           System.Log.FastLogger (LoggerSet, newStderrLoggerSet, pushLogStr)
import           Text.Show.Unicode (ushow)
import qualified Safe

import Tencent.Meeting
-- }}}1


type TimestampSpec = Either TimeOfDay LocalTime


readMeetingType :: ReadM MeetingType
-- {{{1
readMeetingType = do
  t <- str
  case readMay t of
    Just i -> maybe mzero pure (Safe.toEnumMay i)
    Nothing -> do
      case t of
        "booked" -> pure BookedMeeting
        "quick" -> pure QuickMeeting
        _       -> fail $ "unknown meeting type: " <> t
-- }}}1


readDeviceType :: ReadM DeviceType
-- {{{1
readDeviceType = do
  t <- str
  case readMay t of
    Just i -> maybe mzero pure (Safe.toEnumMay i)
    Nothing -> do
      case t of
        "pc"           -> pure DeviceTypePc
        "mac"          -> pure DeviceTypeMac
        "android"      -> pure DeviceTypeAndroid
        "ios"          -> pure DeviceTypeIos
        "web"          -> pure DeviceTypeWeb
        "ipad"         -> pure DeviceTypeIpad
        "android-pad"  -> pure DeviceTypeAndroidPad
        "mini-program" -> pure DeviceTypeMiniProgram
        _              -> fail $ "unknown device type: " <> t
-- }}}1


readTimeOfDay :: ReadM TimeOfDay
readTimeOfDay = str >>= parseTimeM True defaultTimeLocale "%R"

readLocalTime :: ReadM LocalTime
readLocalTime = str >>= parseTimeM True defaultTimeLocale "%F %R"

readTimestampSpec :: ReadM TimestampSpec
readTimestampSpec = fmap Left readTimeOfDay <|> fmap Right readLocalTime

timestampFromSpec :: MonadIO m => TimestampSpec -> m Timestamp
-- {{{1
timestampFromSpec spec = do
  tz <- liftIO getCurrentTimeZone
  fmap (timestampFromPOSIXTime . utcTimeToPOSIXSeconds . localTimeToUTC tz) $
    case spec of
      Right lt -> pure lt
      Left tod -> do
        now <- liftIO getCurrentTime
        let today = localDay $ utcToLocalTime tz now
        pure $ LocalTime today tod
-- }}}1


data ManageCmd = Create UserId Text MeetingType DeviceType TimestampSpec TimestampSpec
               | QueryById MeetingId UserId DeviceType
               | QueryByCode MeetingCode UserId DeviceType
               | Participants MeetingId UserId
               deriving (Show)

manageCmdParser :: Parser ManageCmd
-- {{{1
manageCmdParser = subparser $
  command "create"
    (info (helper <*> (pure Create
                        <*> fmap (UserId . fromString) (argument str (metavar "USER_ID"))
                        <*> fmap fromString (argument str (metavar "SUBJECT"))
                        <*> argument readMeetingType (metavar "MEETING_TYPE")
                        <*> argument readDeviceType (metavar "DEVICE_TYPE")
                        <*> argument readTimestampSpec (metavar "START_TIME")
                        <*> argument readTimestampSpec (metavar "END_TIME")
                      )
          )
          (progDesc "创建会议")
    )
  <> command "query-by-id"
    (info (helper <*> (pure QueryById
                        <*> fmap (MeetingId . fromString) (argument str (metavar "MEETING_ID"))
                        <*> fmap (UserId . fromString) (argument str (metavar "USER_ID"))
                        <*> argument readDeviceType (metavar "DEVICE_TYPE")
                      )
          )
          (progDesc "通过会议 ID 查询")
    )
  <> command "query-by-code"
    (info (helper <*> (pure QueryByCode
                        <*> fmap (MeetingCode . fromString) (argument str (metavar "MEETING_CODE"))
                        <*> fmap (UserId . fromString) (argument str (metavar "USER_ID"))
                        <*> argument readDeviceType (metavar "DEVICE_TYPE")
                      )
          )
          (progDesc "通过会议 Code 查询")
    )
  <> command "participants"
    (info (helper <*> (pure Participants
                        <*> fmap (MeetingId . fromString) (argument str (metavar "MEETING_ID"))
                        <*> fmap (UserId . fromString) (argument str (metavar "USER_ID"))
                      )
          )
          (progDesc "获取参会成员列表")
    )
-- }}}1


data Options = Options
  { optVerbose   :: Int
  , optAppId     :: AppId
  , optSecretId  :: SecretId
  , optSecretKey :: SecretKey
  , optCommand   :: ManageCmd
  }


optionsParse :: Parser Options
-- {{{1
optionsParse = Options
                <$> (option auto
                        $ long "verbose" <> short 'v' <> value 1
                        <> metavar "LEVEL"
                        <> help "Verbose Level (0 - 3)")
                <*> fmap (AppId . fromString) (strOption (long "app-id" <> short 'i' <> metavar "APP_ID"))
                <*> fmap (SecretId . fromString) (strOption (long "secret-id" <> short 's' <> metavar "SECRET_ID"))
                <*> fmap (SecretKey . fromString) (strOption (long "secret-key" <> short 'k' <> metavar "SECRET_KEY"))
                <*> manageCmdParser
-- }}}1


start :: (HttpCallMonad m) => Options -> m ()
-- {{{1
start (Options {..}) = flip runReaderT (optAppId, optSecretId, optSecretKey) $ do
  case optCommand of
    Create user_id password meeting_type dev_type start_time0 end_time0 -> do
      let opts = def
      start_time <- timestampFromSpec start_time0
      end_time <- timestampFromSpec end_time0
      restCreateMeeting meeting_type user_id dev_type password (start_time, end_time) opts
        >>= print_json_resp

    QueryById meeting_id user_id dev_type -> do
      resetQueryMeetingById meeting_id user_id dev_type >>= print_json_resp

    QueryByCode meeting_code user_id dev_type -> do
      resetQueryMeetingByCode meeting_code user_id dev_type >>= print_json_resp

    Participants meeting_id user_id -> do
      restQueryMeetingParticipants meeting_id user_id >>= print_json_resp

  where
    print_json_resp (Left e) = do
      $logError $ "Call failed: " <> toStrict (decodeUtf8 (AP.encodePretty e))

    print_json_resp (Right a) = do
      putStrLn $ toStrict $ decodeUtf8 $ AP.encodePretty a
-- }}}1


start' :: Options -> IO ()
-- {{{1
start' opts = do
  manager <- HC.newTlsManager
  logger_set <- newStderrLoggerSet 0
  runLoggingT
      (flip runReaderT manager $ start opts)
      (appLogger logger_set (optVerbose opts))
-- }}}1


appLogger :: LoggerSet -> Int -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
-- {{{1
appLogger logger_set verbose loc src level ls = do
    let should_log = case level of
                        LevelOther {}   -> True
                        _               -> level `elem` lv_by_v verbose

    if should_log
        then pushLogStr logger_set $ defaultLogStr loc src level ls
        else return ()
    where
        lv_by_v lv
            | lv <= 0   = [ LevelError]
            | lv == 1   = [ LevelError, LevelWarn ]
            | lv == 2   = [ LevelError, LevelWarn, LevelInfo ]
            | otherwise = [ LevelError, LevelWarn, LevelInfo, LevelDebug ]
-- }}}1


main :: IO ()
-- {{{1
main = execParser opts >>= start'
  where
    opts = info (helper <*> optionsParse)
            ( fullDesc
                <> progDesc (unlines
                    [ "执行一些腾讯会议管理查询操作"
                    ])
                <> header "Tencent Meeting Comand Line Management"
                )
-- }}}1


utshow :: Show a => a -> Text
utshow = fromString . ushow


-- vim: set foldmethod=marker:
