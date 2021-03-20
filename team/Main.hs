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
  createExchange chan
  declareQueue chan newQueue { queueName = nameT }
  bindQueue chan nameT hikeExchange "team.#"
  bindQueue chan nameT hikeExchange ("ack." <> nameT)
  queue <- consumeMsgs chan nameT Ack processMsg
  placeOrder name chan
  cancelConsumer chan queue
  closeConnection conn

placeOrder :: String -> Channel -> IO ()
placeOrder name chan = putStrLn "Place order: " >> getLine >>= \case
  ""    -> return ()
  order -> do
    let msg = order <> " from " <> name
    putStrLn $ "sending message: " <> msg
    publishMsg
      chan
      hikeExchange
      ("order." <> T.pack order)
      newMsg { msgBody = BL.pack msg, msgDeliveryMode = Just Persistent }
    placeOrder name chan
