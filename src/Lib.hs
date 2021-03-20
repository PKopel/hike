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

sendMsg :: Channel -> T.Text -> String -> IO ()
sendMsg chan key msg = do
  putStrLn $ "sending message: " <> msg
  publishMsg
    chan
    hikeExchange
    key
    newMsg { msgBody = BL.pack msg, msgDeliveryMode = Just Persistent }
  return ()
