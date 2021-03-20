{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import           Network.AMQP
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.ByteString.Lazy.Char8    as BL

consExchange :: T.Text
consExchange = "consumerExchange"

adminExchange :: T.Text
adminExchange = "adminExchange"

main :: IO ()
main = do
  conn <- openConnection "127.0.0.1" "/" "guest" "guest"
  chan <- openChannel conn
  name <- putStrLn "team name:" >> getLine
  declareExchange
    chan
    newExchange { exchangeName = consExchange, exchangeType = "direct" }
  placeOrder name chan

placeOrder :: String -> Channel -> IO ()
placeOrder name chan = putStrLn "Place order: " >> getLine >>= \case
  ""    -> return ()
  order -> do
    publishMsg
      chan
      consExchange
      (T.pack order)
      newMsg { msgBody         = BL.pack (order <> " from " <> name)
             , msgDeliveryMode = Just Persistent
             }
    placeOrder name chan
