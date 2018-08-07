{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad (replicateM)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader
import qualified Data.ByteString.Char8 as BC
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text.Lazy as TL
import qualified Database.Redis as R
import qualified Network.URI as U (URI, parseURI)
import qualified System.Random as SR
import           Web.Scotty

alphaNum :: String
alphaNum = ['A'..'Z'] ++ ['0'..'9']

newtype Shawty = Shawty { unShawty :: BC.ByteString}
newtype URI = URI { unURI :: BC.ByteString}

randomElement :: String -> IO Char
randomElement xs = do
  let maxIndex :: Int
      maxIndex = length xs - 1
  -- Right of arrow is IO Int, so randomDigit is Int
  randomDigit <- SR.randomRIO (0, maxIndex) :: IO Int
  return (xs !! randomDigit)

shortyGen :: IO String
shortyGen =
  -- -- for testing existing shorty:
  -- return "777"
  replicateM 7 (randomElement alphaNum)

saveURI :: R.Connection
        -> Shawty
        -> URI
        -> IO (Either R.Reply R.Status)
saveURI conn shortURI uri =
  R.runRedis conn $ R.set (unShawty shortURI) (unURI uri)

getURI  :: R.Connection
        -> Shawty
        -> IO (Either R.Reply (Maybe URI))
getURI conn shortURI =
  (fmap . fmap . fmap) URI $
    R.runRedis conn $ R.get (unShawty shortURI)

linkShorty :: String -> String
linkShorty shorty =
  concat [ "<a href=\""
         , shorty
         , "\">Copy and paste your short URL</a>"
         ]

shortyCreated :: Show a => a -> String -> TL.Text
shortyCreated resp shawty =
  TL.concat [ TL.pack (show resp)
            , " shorty is: ", TL.pack (linkShorty shawty)
            ]

shortyAintUri :: TL.Text -> TL.Text
shortyAintUri uri =
  TL.concat [ uri
            , " wasn't a url, did you forget http://?"
            ]

shortyAlreadyExists :: TL.Text
shortyAlreadyExists = "Generated shorty already existed, failed. Refresh to try again."

shortyFound :: TL.Text -> TL.Text
shortyFound tbs =
  TL.concat ["<a href=\"", tbs, "\">", tbs, "</a>"]

app :: ReaderT R.Connection ScottyM ()
app = do
  rConn <- ask
  let handler rConn = do
        get "/" $ do
          uri <- param "uri"
          let parsedUri :: Maybe U.URI
              parsedUri = U.parseURI (TL.unpack uri)
          case parsedUri of
            Just _  -> do
              shawty <- liftIO shortyGen
              let shorty = Shawty $ BC.pack shawty
                  uri' = URI $ encodeUtf8 (TL.toStrict uri)
              oldUri <- liftIO $ getURI rConn shorty
              case oldUri of
                Right (Just _) -> text shortyAlreadyExists
                _ -> do
                  resp <- liftIO (saveURI rConn shorty uri')
                  html (shortyCreated resp shawty)
            Nothing -> text (shortyAintUri uri)
        get "/:short" $ do
          short <- Shawty <$> param "short"
          either <- liftIO (getURI rConn short)
          case either of
            Left reply -> text (TL.pack (show reply))
            Right mbURI -> case mbURI of
              Nothing -> text "uri not found"
              Just uri -> html (shortyFound tbs)
                where tbs :: TL.Text
                      tbs = TL.fromStrict (decodeUtf8 (unURI uri))
  lift $ handler rConn

main :: IO ()
main = do
  rConn <- R.connect R.defaultConnectInfo
  let app' = runReaderT app rConn
  scotty 3000 app'

-- assignment: rewrite the app using ReaderT
-- https://www.fpcomplete.com/blog/2017/06/readert-design-pattern
