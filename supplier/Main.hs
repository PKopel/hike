{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.AMQP
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.ByteString.Lazy.Char8    as BL
import           Control.Monad                  ( (>=>) )
import           Control.Concurrent.MVar

consExchange :: T.Text
consExchange = "consumerExchange"

adminExchange :: T.Text
adminExchange = "adminExchange"

main :: IO ()
main = do
  conn  <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan  <- openChannel conn
  name  <- putStrLn "supplier name:" >> getLine
  oidMV <- newMVar 0
  declareExchange
    chan
    newExchange { exchangeName = consExchange, exchangeType = "direct" }
  putStrLn "Enter list of available supplies: "
  queues <- TIO.getLine >>= createQueues oidMV chan . T.words
  getLine
  mapM_ (cancelConsumer chan) queues
  closeConnection conn

createQueues :: MVar Int -> Channel -> [T.Text] -> IO [ConsumerTag]
createQueues oidMV chan = mapM (declare >=> bind >=> consume)
 where
  declare name = declareQueue chan newQueue { queueName = name }
  bind (name, _, _) = bindQueue chan name consExchange name >> return name
  consume name = consumeMsgs chan name Ack $ processOrder oidMV

processOrder :: MVar Int -> (Message, Envelope) -> IO ()
processOrder oidMV (msg, env) = do
  oid <- takeMVar oidMV
  putStrLn
    ("received order: " <> BL.unpack (msgBody msg) <> "; id: " <> show oid)
  ackEnv env
  putMVar oidMV (oid + 1)
