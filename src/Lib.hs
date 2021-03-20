{-# LANGUAGE OverloadedStrings #-}
module Lib where

import           Network.AMQP
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.ByteString.Lazy.Char8    as BL

hikeExchange :: T.Text
hikeExchange = "hike_exchange"

createExchange :: Channel -> IO ()
createExchange chan = declareExchange
  chan
  newExchange { exchangeName = hikeExchange, exchangeType = "topic" }

processMsg :: (Message, Envelope) -> IO ()
processMsg (msg, env) =
  putStrLn ("received message: " <> BL.unpack (msgBody msg)) >> ackEnv env
