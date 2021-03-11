{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types
import Control.Monad (forever)
import Control.Concurrent
import Control.Concurrent.STM
import Data.Ord
import qualified Data.Binary as B
import qualified Data.Aeson as J
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Control.Exception

type ScoreTable = M.Map String Word
deriving instance J.FromJSON a => J.FromJSON (Down a)
deriving instance J.ToJSON a => J.ToJSON (Down a)
deriving instance B.Binary a => B.Binary (Down a)

app :: TVar ScoreTable -> TQueue ScoreTable -> Application
app score q req respond = case (requestMethod req, pathInfo req) of
  ("GET", ["score"]) -> do
    resp <- J.encode <$> readTVarIO score
    respond $ responseLBS status200 [("Content-Type", "application/json")] resp
  ("PUT", ["score"]) ->
    (J.decode <$> consumeRequestBodyStrict req) >>= \case
      Just newScore -> do
        atomically $ do
          writeTVar score newScore
          writeTQueue q newScore
        respond $ responseLBS status200 [("Content-Type", "text/plain")] "OK"
      _ -> respond $ responseLBS status400 [("Content-Type", "text/plain")] "Bad Request"
  ("POST", ["score"]) -> do
    oldscore <- readTVarIO score
    mNewScore <- fmap (M.fromListWith max) . J.decode <$> consumeRequestBodyStrict req
    case mNewScore of
      Just newScore -> do
        let combinedScore = M.unionWith max oldscore newScore
        atomically $ do
          writeTVar score combinedScore
          writeTQueue q combinedScore
        respond $ responseLBS status200 [("Content-Type", "text/plain")] "OK"
      _ -> respond $ responseLBS status400 [("Content-Type", "text/plain")] "Bad Request"
  _ -> respond $ responseLBS status404 [("Content-Type", "text/plain")] "Not Found"

main :: IO ()
main = do
  score <- loadScore
  tvScore <- newTVarIO score
  saveQueue <- newTQueueIO
  _ <- forkIO . forever $ saveScore saveQueue
  run 3000 $ app tvScore saveQueue

scoreFile :: FilePath
scoreFile = "score.dat"

saveScore :: TQueue ScoreTable -> IO ()
saveScore scoreQueue = do
  score <- atomically $ readTQueue scoreQueue
  BS.writeFile scoreFile $ B.encode score

loadScore :: IO ScoreTable
loadScore = do
  f <- BS.readFile scoreFile
  case B.decodeOrFail f of
    Left _ -> return M.empty
    Right (_, _, result) -> return result
  `catch` (\e -> print (e :: SomeException) >> return M.empty)
