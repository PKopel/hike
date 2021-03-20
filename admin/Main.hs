{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Network.AMQP
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.ByteString.Lazy.Char8    as BL

adminExchange :: T.Text
adminExchange = "adminExchange"

main :: IO ()
main = getLine >>= putStrLn
