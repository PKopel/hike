{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.AMQP
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.ByteString.Lazy.Char8    as BL
import           Control.Monad                  ( (>=>) )

consExchange :: T.Text
consExchange = "consumerExchange"

adminExchange :: T.Text
adminExchange = "adminExchange"

main :: IO ()
main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn
  name <- putStrLn "supplier name:" >> getLine
  declareExchange
    chan
    newExchange { exchangeName = consExchange, exchangeType = "direct" }
  putStrLn "Enter list of available supplies: "
  queues <- TIO.getLine >>= createQueues chan . T.words
  getLine
  mapM_ (cancelConsumer chan) queues
  closeConnection conn

createQueues :: Channel -> [T.Text] -> IO [ConsumerTag]
createQueues chan = mapM (declare >=> bind >=> consume)
 where
  declare name = declareQueue chan newQueue { queueName = name }
  bind (name, _, _) = bindQueue chan name consExchange name >> return name
  consume name = consumeMsgs chan name Ack processOrder

processOrder :: (Message, Envelope) -> IO ()
processOrder (msg, env) =
  putStrLn ("received message: " <> BL.unpack (msgBody msg)) >> ackEnv env
