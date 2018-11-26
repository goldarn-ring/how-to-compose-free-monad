{-# LANGUAGE FlexibleContexts #-}

module NinjaFriends (main) where

import Prelude hiding (or)
import Data.Functor.Sum (Sum(..))
import Control.Monad.Free (Free, foldFree, liftF)

import Inject (Inject, inj)
import Friends (Friends(..), goFriendsJp, goFriendsEn)
import Ninja (Ninja(..), goNinjaJp, goNinjaEn)


liftF' :: (Inject f g, Functor g) => f a -> Free g a
liftF' f = liftF $ inj f


wai :: (Inject Friends f, Functor f) => Free f ()
wai = liftF' $ Wai ()

sugoi :: (Inject Friends f, Functor f) => Free f ()
sugoi = liftF' $ Sugoi ()

tanoshii :: (Inject Friends f, Functor f) => Free f ()
tanoshii = liftF' $ Tanoshii ()

tokuiNandane :: (Inject Friends f, Functor f) => String -> Free f ()
tokuiNandane skill  = liftF' $ TokuiNandane skill ()



domo :: (Inject Ninja f, Functor f) => String -> Free f ()
domo name = liftF' $ Domo name ()

yeeart :: (Inject Ninja f, Functor f) => Free f ()
yeeart = liftF' $ Yeeart ()

aieee :: (Inject Ninja f, Functor f) => Free f ()
aieee = liftF' $ Aieee ()

sayonara :: (Inject Ninja f, Functor f) => String -> Free f ()
sayonara name  = liftF' $ Sayonara name ()



type NinjaFriends = Sum Ninja Friends

or :: (f a -> h a) -> (g a -> h a) -> (Sum f g a -> h a)
or fh gh = or'
  where
    or' (InL left) = fh left
    or' (InR right) = gh right

runNinjaFriendsJp :: Free NinjaFriends a -> IO a
runNinjaFriendsJp f = foldFree goNinjaFriendsJp f

goNinjaFriendsJp :: NinjaFriends a -> IO a
goNinjaFriendsJp x = (goNinjaJp `or` goFriendsJp) x

runNinjaFriendsEn :: Free NinjaFriends a -> IO a
runNinjaFriendsEn f = foldFree goNinjaFriendsEn f

goNinjaFriendsEn :: NinjaFriends a -> IO a
goNinjaFriendsEn x = (goNinjaEn `or` goFriendsEn) x

ninjaFriends :: String -> String -> String -> Free NinjaFriends ()
ninjaFriends skill ninja1 ninja2 = do
  wai
  domo ninja1
  sugoi
  yeeart
  tanoshii
  aieee
  sayonara ninja2
  tokuiNandane skill


main :: IO ()
main =  do
  putStrLn "◆ニンジャフレンズ（日本語）◆"
  runNinjaFriendsJp $ ninjaFriends "クソコード" "サーバル" "カバン"
  putStrLn "\n◆Ninja Friends (English)◆"
  runNinjaFriendsEn $ ninjaFriends "Crappy Code" "Serval" "Kaban"
