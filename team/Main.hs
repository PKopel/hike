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
  name <- putStrLn "team name:" >> getLine
  let nameT = T.pack name
      nameQ = nameT <> "_queue"
  createExchange chan
  declareQueue chan newQueue { queueName = nameQ }
  bindQueue chan nameQ hikeExchange "team.#"
  bindQueue chan nameQ hikeExchange ("ack." <> nameT)
  queue <- consumeMsgs chan nameQ Ack processMsg
  placeOrder name chan
  cancelConsumer chan queue
  closeConnection conn

placeOrder :: String -> Channel -> IO ()
placeOrder name chan = putStrLn "Place order: " >> getLine >>= \case
  ""    -> return ()
  order -> do
    let msg = order <> " from " <> name
    sendMsg chan ("order." <> T.pack order) msg
    placeOrder name chan
