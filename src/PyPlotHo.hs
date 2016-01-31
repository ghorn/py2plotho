{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}

module Main
       ( main
       , PlotThing(..), PlotOtherThing(..) -- export to silence dumb warnings
       ) where

import GHC.Generics ( Generic )

import Control.Concurrent as CC
import Control.Concurrent.Chan as C
import Control.Monad ( forever )
import Data.Aeson ( FromJSON, eitherDecodeStrict )
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map as M
import qualified Data.Set as S
import PlotHo
import qualified System.ZMQ4 as ZMQ

url :: FilePath
url = "ipc:///tmp/py2plotho"

data PlotThing =
  PlotThing
  { value :: Double
  , count :: Int
  } deriving Generic
instance FromJSON PlotThing
instance Lookup PlotThing

data PlotOtherThing =
  PlotOtherThing
  { otherValue :: Double
  , otherCount :: Int
  } deriving Generic
instance FromJSON PlotOtherThing
instance Lookup PlotOtherThing

data Channel a = Channel XAxisType (a -> Bool) String

data Channel' where
  Channel' :: (FromJSON a, Lookup a) => Channel a -> Channel'

main :: IO ()
main = ZMQ.withContext $ \context -> ZMQ.withSocket context ZMQ.Sub $ \socket -> do
  ZMQ.connect socket url
  let channels =
        [ Channel' (Channel XAxisTime (\x -> 0 == count x) "topic0")
        , Channel' (Channel XAxisCount (\x -> 0 == otherCount x) "topic1")
        ]

  let topics = map (\(Channel' (Channel _ _ topic)) -> topic) channels
  mapM_ (\topic -> ZMQ.subscribe socket (BS8.pack topic)) topics
  topicMap <- sequenceA (M.fromSet (const C.newChan) (S.fromList topics))
    :: IO (M.Map String (C.Chan BS8.ByteString))

  _ <- CC.forkIO $ forever $ do
    topic':msg <- ZMQ.receiveMulti socket :: IO [BS8.ByteString]
    case M.lookup (BS8.unpack topic') topicMap of
      Just chan -> C.writeChan chan (BS8.concat msg)
      Nothing -> error $ "got unrecognized topic " ++ show topic'

  let listener :: FromJSON a => Channel a -> (a -> Bool -> IO ()) -> IO ()
      listener (Channel _ toReset topic) newMessage = do
        let chan = case M.lookup topic topicMap of
              Just r -> r
              Nothing -> error "impossible happened: couldn't lookup channel from topic map"
        forever $ do
          binaryMessage <- C.readChan chan
          case eitherDecodeStrict binaryMessage of
            Left err -> putStrLn $ "error deserializing message!!\n" ++ err
            Right msg -> newMessage msg (toReset msg)

  runPlotter $ do
    let add (Channel' c@(Channel xaxisType _ topic)) = addHistoryChannel topic xaxisType (listener c)
    mapM_ add channels
