{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant (Handler, (:<|>)((:<|>)), Server, ServerT, serve)
import Servant.Utils.Enter ((:~>)(NT), enter)

import Servant.RawM (serveDirectoryWebApp)

import Api (Api, port)

data Config = Config
  { configInt1 :: Int
  , configInt2 :: Int
  , configDir :: FilePath
  } deriving Show

config :: Config
config = Config {configInt1 = 3, configInt2 = 4, configDir = "./example/files"}

serverRoot :: ServerT Api (ReaderT Config IO)
serverRoot = getOtherEndpoint1 :<|> rawEndpoint :<|> getOtherEndpoint2

getOtherEndpoint1 :: ReaderT Config IO Int
getOtherEndpoint1 = do
  (Config int1 _ _) <- ask
  pure int1

rawEndpoint :: ReaderT Config IO Application
rawEndpoint = do
  (Config _ _ dir) <- ask
  serveDirectoryWebApp dir

getOtherEndpoint2 :: ReaderT Config IO Int
getOtherEndpoint2 = do
  (Config _ int2 _) <- ask
  pure int2

app :: Config -> Application
app conf = serve (Proxy :: Proxy Api) apiServer
  where
    apiServer :: Server Api
    apiServer = enter naturalTrans serverRoot

    naturalTrans :: ReaderT Config IO :~> Handler
    naturalTrans = NT transformation

    transformation :: ReaderT Config IO a -> Handler a
    transformation readerT = liftIO $ runReaderT readerT conf

-- | Run the WAI 'Application' using 'run' on the port defined by 'port'.
main :: IO ()
main = do
  putStrLn $ "example RawM server running on port " <> show port
  run port $ app config
