module Main where

main :: IO ()
main = placeOrder

placeOrder :: IO ()
placeOrder = do
  putStrLn "Place order: "
  order <- getLine
  placeOrder
