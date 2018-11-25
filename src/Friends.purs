module Friends (main, Friends(..), wai, sugoi, tanoshii, tokuiNandane, runFriendsJp, runFriendsEn, goFriendsJp, goFriendsEn) where 

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Free (Free, foldFree, liftF)

data Friends a
  = Wai a
  | Sugoi a
  | Tanoshii a
  | TokuiNandane String a

wai :: Free Friends Unit
wai = liftF $ Wai unit

sugoi :: Free Friends Unit
sugoi = liftF $ Sugoi unit

tanoshii :: Free Friends Unit
tanoshii = liftF $ Tanoshii unit

tokuiNandane :: String -> Free Friends Unit
tokuiNandane skill  = liftF $ TokuiNandane skill unit

runFriendsJp :: forall a. Free Friends a -> Effect a
runFriendsJp f = foldFree goFriendsJp f

goFriendsJp :: forall a. Friends a -> Effect a
goFriendsJp (Wai next) = do
  log "わーい！"
  pure next
goFriendsJp (Sugoi next) = do
  log "すごーい！"
  pure next
goFriendsJp (Tanoshii next) = do
  log "たーのしー！"
  pure next
goFriendsJp (TokuiNandane skill next) = do
  log ("君は" <> skill <> "が得意なフレンズなんだね！")
  pure next

runFriendsEn :: forall a. Free Friends a -> Effect a
runFriendsEn f = foldFree goFriendsEn f

goFriendsEn :: forall a. Friends a -> Effect a
goFriendsEn (Wai next) = do
  log "Wow!"
  pure next
goFriendsEn (Sugoi next) = do
  log "Great!"
  pure next
goFriendsEn (Tanoshii next) = do
  log "Wonderful!"
  pure next
goFriendsEn (TokuiNandane skill next) = do
  log ("You are friends that good at " <> skill <> "!")
  pure next

friends :: String -> Free Friends Unit
friends skill = do
  wai
  sugoi
  tanoshii
  tokuiNandane skill

main :: Effect Unit
main = do
  log "◆フレンズ（日本語）◆"
  runFriendsJp $ friends "クソコード"
  log "\n◆Friends (English)◆"
  runFriendsEn $ friends "Crappy Code"

