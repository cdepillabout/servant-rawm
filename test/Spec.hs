{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Concurrent
       (ThreadId, forkIO, killThread, threadDelay)
import Control.Exception (Exception, SomeException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import Data.ByteString.Lazy (ByteString)
import Data.Either (isLeft)
import Data.Monoid ((<>))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Type.Equality ((:~:)(Refl))
import Data.Typeable (Typeable)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Types (methodGet)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant ((:>), Handler, ServerT, hoistServer, serve)
import Servant.Client
       (Client, ClientM, Response, client, mkClientEnv, parseBaseUrl, responseBody, runClientM)
import Servant.Client.Core (Request, appendToPath, requestMethod)
import Test.Hspec.Wai (get, shouldRespondWith, with)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.Hspec (afterAll, beforeAll, it, shouldBe, testSpec)
import Test.Tasty.HUnit ((@?=), assertFailure, testCase)

import Servant.RawM (RawM, serveDirectoryWebApp)

main :: IO ()
main = do
  tests <- testsIO
  defaultMain tests

testsIO :: IO TestTree
testsIO = do
  clientTests <- clientTestsIO
  serverTests <- serverTestsIO
  pure $
    testGroup
      "tests"
      [ instanceTests
      , clientTests
      , serverTests
      ]

-------------
-- Helpers --
-------------

type Selector e = e -> Bool

anyException :: Selector SomeException
anyException _ = True

-- | Extra HUnit assertion to make sure an expression throws an exception.
assertThrows
  :: forall e a.
     (Exception e, Typeable e)
  => IO a -> Selector e -> IO ()
assertThrows ioAction selector = do
  didCatch <- catch (ioAction *> pure False) (pure . selector)
  case didCatch of
    False ->
      assertFailure "expecting an exception, but no exception occurred"
    True -> pure ()

-- | Infix version of 'assertThrows'.
(@!)
  :: (Exception e, Typeable e)
  => IO a -> Selector e -> IO ()
(@!) = assertThrows

infix 1 @!

------------------------------
-- HasServer instance tests --
------------------------------

checkRawMServer :: ServerT RawM m :~: m Application
checkRawMServer = Refl

checkRawMClient
  :: Client m RawM :~: ((Request -> Request) -> m Response)
checkRawMClient = Refl

instanceTests :: TestTree
instanceTests =
  testGroup
    "instances"
    [ testCase "HasServer" $ checkRawMServer @?= Refl
    , testCase "HasClient" $ checkRawMClient @?= Refl
    ]

--------------------------------
-- Real Server Tests (Server) --
--------------------------------

type Api = "test" :> RawM

server :: ServerT Api (ReaderT FilePath IO)
server = fileServer

fileServer :: ReaderT FilePath IO Application
fileServer = do
  path <- ask
  serveDirectoryWebApp path

app :: Application
app =
  let proxy = Proxy :: Proxy Api
  in
  serve proxy $ hoistServer proxy trans server
  where
    trans :: forall a. ReaderT FilePath IO a -> Handler a
    trans readerT = liftIO $ runReaderT readerT "example/files"

serverTestsIO :: IO TestTree
serverTestsIO =
  testSpec "server" $
    with (pure app) $ do
      it "correctly serves files" $
        get "/test/bar.txt" `shouldRespondWith` "This is bar.txt.\n"
      it "returns 404 for non-existent files" $
        get "/test/non-existent-file.txt" `shouldRespondWith` 404

--------------------------------
-- Real Server Tests (Client) --
--------------------------------

getFile'
  :: (Request -> Request)
  -> ClientM Response
getFile' = client (Proxy :: Proxy Api)

getFile :: Text -> ClientM ByteString
getFile filePath = do
  resp <-
    getFile' $ \req -> appendToPath filePath req { requestMethod = methodGet }
  pure $ responseBody resp

clientTestsIO :: IO TestTree
clientTestsIO = do
  manager <- newManager defaultManagerSettings
  baseUrl <- parseBaseUrl $ "http://localhost:" <> show port <> "/"
  let clientEnv = mkClientEnv manager baseUrl
  testSpec "client" . beforeAll runServer . afterAll killServer $ do
    it "correctly gets files" $ \_ -> do
      eitherRes <- runClientM (getFile "bar.txt") clientEnv
      eitherRes `shouldBe` Right "This is bar.txt.\n"
    it "returns ServantErr for non-existent files" $ \_ -> do
      eitherRes <- runClientM (getFile "non-existent-file.txt") clientEnv
      isLeft eitherRes `shouldBe` True

runServer :: IO ThreadId
runServer = do
  threadId <- forkIO (run port app)
  threadDelay $ 250 * 1000
  pure threadId

killServer :: ThreadId -> IO ()
killServer = killThread

port :: Int
port = 51135
