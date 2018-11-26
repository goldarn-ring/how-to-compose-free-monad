module Main where

import qualified Friends
import qualified Ninja
import qualified NinjaFriends

main :: IO ()
main = do
  putStrLn "---------------- Friends.main ----------------"
  Friends.main
  putStrLn "\n---------------- Ninja.main ----------------"
  Ninja.main
  putStrLn "\n---------------- NinjaFriends.main ----------------"
  NinjaFriends.main
