{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Main where
import Protolude hiding (log)
import           Codec.Picture
import           Codec.Picture.Extra    (below)
import           Control.Concurrent     (threadDelay)
import Control.Exception (catch)
import           Control.Monad          (forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson             (FromJSON (..), withObject, (.:), (.:?))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Either            (isLeft, rights)
import           Data.Semigroup         ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T
import Data.Yaml
import Network.HTTP.Client (HttpException(..))
import           Test.WebDriver
import           Test.WebDriver.JSON    (ignoreReturn)
import           System.Process
import System.IO (hClose, hIsClosed)

import Manta

data WindowCoords = WindowCoords
  {
    wCoordsTotalWidth     :: !Double
  , wCoordsTotalHeight    :: !Double
  , wCoordsViewportWidth  :: !Double
  , wCoordsViewportHeight :: !Double
  } deriving (Show)

instance FromJSON WindowCoords where
  parseJSON = withObject "coords" $ \o -> WindowCoords
    <$> o .: "offsetWidth"
    <*> o .: "scrollHeight"
    <*> o .: "clientWidth"
    <*> o .: "innerHeight"

windowCoordsJS :: Text
windowCoordsJS = "return {\
  \'offsetWidth': document.body.scrollWidth,\
  \'clientWidth': document.body.clientWidth,\
  \'scrollHeight': document.body.parentNode.scrollHeight,\
  \'innerHeight': window.innerHeight}"

windowScrollToJS :: Double -> Text
windowScrollToJS y = "window.scrollTo(0, " <> T.pack (show y) <> ")"

firefoxConfig, chromeConf :: WDConfig
firefoxConfig = defaultConfig
chromeConf = useBrowser chrome defaultConfig

log :: Text -> IO ()
log = print

data SeleniumServerOptions = SeleniumServerOptions
    { ssOptionJarFile   :: FilePath
    , ssOptionDebug     :: Bool
    } deriving (Show)

withSeleniumServer :: SeleniumServerOptions -> IO () -> IO ()
withSeleniumServer opts action = do
    let ps = shell $ "xvfb-run -a -e /dev/stdout -s \"-screen 0 1920x1080x24\" java -jar " <> toS (ssOptionJarFile opts)
    log "spawned process."
    (Nothing, Just out, Nothing, ph) <- createProcess ps {std_out = CreatePipe}
    _ <- waitForServer ph out
    completedNormally <- catch (action >> return True) (\e -> do
            print (e :: HttpException)
            hClose out
            return False)
    if completedNormally then do
        log "spawned process completed normally. shutting down."
        terminateProcess ph
        outOpen <- not <$> hIsClosed out
        when outOpen (hClose out)
        void $ interruptProcessGroupOf ph
        void $ waitForProcess ph
        return ()
        else do
            outOpen <- not <$> hIsClosed out
            when outOpen (BS.hGetNonBlocking out 1024 >>= print)
            log "spawnd process did not complete normally."
    return ()
  where
      waitForServer :: ProcessHandle -> Handle -> IO ()
      waitForServer ph h = waitForServer'
        where
            waitForServer' = do
                -- Read any outstanding input.
                bs <- BS.hGetNonBlocking h (64 * 1024)
                unless (BS.null bs) (BS.putStr bs)
                -- Check on the process.
                s <- getProcessExitCode ph
                case s of
                    Just ec -> void $ log (show ec)
                    Nothing -> do
                        let started = BS.isInfixOf "Selenium Server is up and running" bs
                        unless started waitForServer'


-- kit@sawmill:/home/code/virtualenvs/twisted$ xvfb-run java -jar selenium-server-standalone-2.53.0.jar
defaultSeleniumOptions :: SeleniumServerOptions
defaultSeleniumOptions = SeleniumServerOptions "/home/code/virtualenvs/twisted/selenium-server-standalone-2.53.0.jar" True

data PageConfig = PageConfig
    { pageConfigUrl :: Text
    , pageConfigJs :: Maybe Text
    , pageConfigFile :: Text
    , pageConfigMantaPath :: Maybe Text
    } deriving (Show, Generic)

instance FromJSON PageConfig where
    parseJSON = withObject "PageConfig" $ \o -> do
        pageConfigUrl <- o .: "url"
        pageConfigJs <- o .:? "js"
        pageConfigFile <- o .: "file"
        pageConfigMantaPath <- o .:? "manta"
        return PageConfig{..}

data Configuration = Configuration
    { configSelenium :: SeleniumServerOptions
    , configPages :: [PageConfig]
    } deriving (Show, Generic)

instance FromJSON Configuration where
    parseJSON = withObject "Configuration" $ \o -> do
        jarFile <- o .: "jarFile"
        debug <- o .:? "debug"
        pages <- o .: "pages"
        pageConfs <- mapM parseJSON pages
        let serverOptions = SeleniumServerOptions
                            { ssOptionJarFile=jarFile
                            , ssOptionDebug= maybe False identity debug
                            }
        return Configuration{configSelenium=serverOptions, configPages=pageConfs}

{--
-- using WebDriver from ghci
import Test.WebDriver.Session (getSession)
session <- runSession defaultConfig getSession
runWD session $ openPage ""
runWD session $ closeSession
--}
main :: IO ()
main = do
    [confFile] <- getArgs
    print confFile
    bs <- encodeUtf8 <$> readFile confFile
    let yaml = decodeEither' bs
    case yaml of
        Left e -> log (show e)
        Right config -> do
            log "launching selenium"
            withSeleniumServer defaultSeleniumOptions $ do
                print config
                env <- Manta.defEnv
                forM_ (configPages config) $ \pconf -> do
                    print pconf
                    log ("grabScreener: " <> pageConfigUrl pconf)
                    mfile <- grabScreener (pageConfigUrl pconf)
                                 (toS (pageConfigFile pconf))
                                 (pageConfigJs pconf) `catch` (\e->do; print (e::SomeException); return Nothing)
                    case mfile of
                        Nothing -> print ("no screenshot created" :: Text)
                        Just filepath -> do
                            let mpath = pageConfigMantaPath pconf
                            case mpath of
                                Nothing -> print ("no manta path specified" :: Text)
                                Just path -> do
                                    Manta.runMantaClientNoLogging env $ Manta.putFile filepath (toS path)
                                    return ()

grabScreener :: Text -> FilePath -> Maybe Text -> IO (Maybe FilePath)
grabScreener url filename extraJs =  runSession firefoxConfig . finallyClose $ do
  liftIO $ log "Getting page"
  setPageLoadTimeout (200 * 1000)
  openPage (strConv Strict url)
  liftIO $ log "Got page"

  _ <- case extraJs of
      Nothing -> return ()
      Just js -> do
          liftIO $ log "executing some js"
          ignoreReturn $ executeJS [] js
          --"$('#folioBar, #bottom-bar-container, #ad-colB-1').remove()"
  liftIO $ log "taking screenshot"
  firefoxScreenShot filename
  liftIO $ log "saving screenshot"
  return $ Just filename

firefoxScreenShot :: FilePath -> WD ()
firefoxScreenShot filename = do
  img <- screenshot
  let img' = decodePng (BSL.toStrict img)
  case img' of
    Left str -> liftIO $ print str
    Right img'' -> liftIO $ do
      r <- writeDynamicPng filename img''
      log $ if isLeft r then "error" else "success"
  return ()

chromeScreenShot :: FilePath -> WD ()
chromeScreenShot filename = do
  liftIO $ threadDelay 3000000
  ignoreReturn $ executeJS [] "document.documentElement.style.overflowX = 'hidden'"
  coords <- executeJS [] windowCoordsJS :: WD WindowCoords
  liftIO $ log ("coords: " <> show coords)
  let yCoords = [0, (wCoordsViewportHeight coords) .. (wCoordsTotalHeight coords)]
  imgs <- forM yCoords $ \y -> do
      ignoreReturn $ executeJS [] (windowScrollToJS y)
      liftIO $ do
        threadDelay 1000000
        putStrLn $ T.unpack (windowScrollToJS y)
      img <- screenshot
      liftIO $ log "got screenshot"
      return $ decodePng (BSL.toStrict img)
  if any isLeft imgs then
    liftIO $ log "Error decoding one of the imges."
    else liftIO $ do
      let imgs' = rights imgs
          imgs'' = map convertRGB8 imgs'
          combined = below imgs''
      writePng filename combined
      return ()
  return ()
