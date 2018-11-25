module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Friends as Friends
import Ninja as Ninja
import NinjaFriends as NinjaFriends

main :: Effect Unit
main = do
  log "---------------- Friends.main ----------------"
  Friends.main
  log "\n---------------- Ninja.main ----------------"
  Ninja.main
  log "\n---------------- NinjaFriends.main ----------------"
  NinjaFriends.main
