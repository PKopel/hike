{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.AMQP
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.ByteString.Lazy.Char8    as BL
import           Control.Monad                  ( (>=>) )
import           Control.Concurrent.MVar
import           Lib

main :: IO ()
main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn
  name <- putStrLn "supplier name:" >> getLine
  let nameT = T.pack name
  oidMV <- newMVar 0
  createExchange chan
  declareQueue chan newQueue { queueName = nameT }
  bindQueue chan nameT hikeExchange "#.supplier"
  queue <- consumeMsgs chan nameT Ack processMsg
  putStrLn "Enter list of available supplies: "
  queues <- TIO.getLine >>= createQueues name oidMV chan . T.words
  getLine -- press enter to stop
  mapM_ (cancelConsumer chan) (queue : queues)
  closeConnection conn

createQueues :: String -> MVar Int -> Channel -> [T.Text] -> IO [ConsumerTag]
createQueues supp oidMV chan = mapM (declare >=> bind)
 where
  declare name = declareQueue chan newQueue { queueName = name }
  bind (name, _, _) = do
    bindQueue chan name hikeExchange ("order." <> name)
    consumeMsgs chan name Ack $ processOrder supp oidMV chan

processOrder :: String -> MVar Int -> Channel -> (Message, Envelope) -> IO ()
processOrder supp oidMV chan (msg, env) = do
  oid <- takeMVar oidMV
  let body    = BL.unpack (msgBody msg)
      product = head $ words body
      team    = last $ words body
      resp    = supp <> ": order " <> show oid <> ", " <> product <> " ready"
  putStrLn ("received order: " <> body <> "; id: " <> show oid)
  ackEnv env
  putStrLn $ "sending message: " <> resp
  publishMsg
    chan
    hikeExchange
    ("ack." <> T.pack team)
    newMsg { msgBody = BL.pack resp, msgDeliveryMode = Just Persistent }
  putMVar oidMV (oid + 1)
