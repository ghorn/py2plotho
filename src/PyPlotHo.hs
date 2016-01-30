{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DeriveGeneric #-}

module Main
       ( main
       , PlotThing(..) -- export to silence dumb warnings
       ) where

import GHC.Generics ( Generic )

import Control.Monad ( forever )
import Data.Aeson ( FromJSON, eitherDecodeStrict )
import qualified Data.ByteString.Char8 as BS8
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

main :: IO ()
main = ZMQ.withContext $ \c -> ZMQ.withSocket c ZMQ.Sub $ \socket -> do
  ZMQ.connect socket url
  ZMQ.subscribe socket (BS8.pack "")

  let listener :: (PlotThing -> Bool -> IO ()) -> IO ()
      listener newMessage = forever $ do
        binaryMessage <- ZMQ.receive socket :: IO BS8.ByteString
        case eitherDecodeStrict binaryMessage of
          Left err ->
            putStrLn $ "error deserializing message!!\n" ++ err
          Right msg -> do
            let reset = 0 == count msg
            print reset
            newMessage msg reset

  runPlotter $ addHistoryChannel "hello py2plotho" XAxisCount listener
