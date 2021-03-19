{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.AMQP
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO

myExchange :: T.Text
myExchange = "myExchange"

myQueue :: T.Text
myQueue = "myQueue"

main :: IO ()
main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  ch   <- openChannel conn
  declareExchange
    ch
    newExchange { exchangeName = myExchange, exchangeType = "direct" }
  declareQueue ch newQueue { queueName = myQueue }
  putStrLn "Enter list of available supplies: "
  mapM_ (bindQueue ch myQueue myExchange) . T.words <$> TIO.getLine
  getLine
  closeConnection conn
