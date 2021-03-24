{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Network.AMQP
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.ByteString.Lazy.Char8    as BL
import           Lib

main :: IO ()
main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn
  let nameQ = "admin_queue"
  createExchange chan
  declareQueue chan newQueue { queueName = nameQ }
  bindQueue chan nameQ hikeExchange "order.*"
  bindQueue chan nameQ hikeExchange "ack.*"
  queue <- consumeMsgs chan nameQ Ack processMsg
  sendMessage chan
  cancelConsumer chan queue
  closeConnection conn

sendMessage :: Channel -> IO ()
sendMessage chan = putStrLn "Receiver: " >> getLine >>= \case
  ""    -> return ()
  other -> do
    putStrLn "message:"
    body <- getLine
    let msg      = "admin: " <> body
        receiver = T.pack $ case other of
          "all" -> "team.supplier"
          _     -> other
    sendMsg chan receiver msg
    sendMessage chan
