module NinjaFriends (main) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Functor.Coproduct (Coproduct (..))
import Data.Functor.Coproduct.Inject (class Inject, inj)
import Data.Either (Either(..))
import Control.Monad.Free (Free, foldFree, liftF)

import Friends (Friends(..), goFriendsJp, goFriendsEn)
import Ninja (Ninja(..), goNinjaJp, goNinjaEn)


liftF' :: forall f g a. Inject f g => f a -> Free g a
liftF' f = liftF $ inj f


wai :: forall f. Inject Friends f => Free f Unit
wai = liftF' $ Wai unit

sugoi :: forall f. Inject Friends f => Free f Unit
sugoi = liftF' $ Sugoi unit

tanoshii :: forall f. Inject Friends f => Free f Unit
tanoshii = liftF' $ Tanoshii unit

tokuiNandane :: forall f. Inject Friends f => String -> Free f Unit
tokuiNandane skill  = liftF' $ TokuiNandane skill unit



domo :: forall f. Inject Ninja f => String -> Free f Unit
domo name = liftF' $ Domo name unit

yeeart :: forall f. Inject Ninja f => Free f Unit
yeeart = liftF' $ Yeeart unit

aieee :: forall f. Inject Ninja f => Free f Unit
aieee = liftF' $ Aieee unit

sayonara :: forall f. Inject Ninja f => String -> Free f Unit
sayonara name  = liftF' $ Sayonara name unit



type NinjaFriends = Coproduct Ninja Friends

or :: forall f g h a. (f a -> h a) -> (g a -> h a) -> (Coproduct f g a -> h a)
or fh gh = case _ of
  (Coproduct (Left left)) -> fh left
  (Coproduct (Right right)) -> gh right

runNinjaFriendsJp :: forall a. Free NinjaFriends a -> Effect a
runNinjaFriendsJp f = foldFree goNinjaFriendsJp f

goNinjaFriendsJp :: forall a. NinjaFriends a -> Effect a
goNinjaFriendsJp x = (goNinjaJp `or` goFriendsJp) x

runNinjaFriendsEn :: forall a. Free NinjaFriends a -> Effect a
runNinjaFriendsEn f = foldFree goNinjaFriendsEn f

goNinjaFriendsEn :: forall a. NinjaFriends a -> Effect a
goNinjaFriendsEn x = (goNinjaEn `or` goFriendsEn) x

ninjaFriends :: String -> String -> String -> Free NinjaFriends Unit
ninjaFriends skill ninja1 ninja2 = do
  wai
  domo ninja1
  sugoi
  yeeart
  tanoshii
  aieee
  sayonara ninja2
  tokuiNandane skill


main :: Effect Unit
main =  do
  log "◆ニンジャフレンズ（日本語）◆"
  runNinjaFriendsJp $ ninjaFriends "クソコード" "サーバル" "カバン"
  log "\n◆Ninja Friends (English)◆"
  runNinjaFriendsEn $ ninjaFriends "Crappy Code" "Serval" "Kaban"
