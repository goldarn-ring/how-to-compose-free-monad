{-# LANGUAGE DeriveFunctor #-}

module Friends (main, Friends(..), wai, sugoi, tanoshii, tokuiNandane, runFriendsJp, runFriendsEn, goFriendsJp, goFriendsEn) where 

import Control.Monad.Free (Free, foldFree, liftF)

data Friends a
  = Wai a
  | Sugoi a
  | Tanoshii a
  | TokuiNandane String a
  deriving (Functor)

wai :: Free Friends ()
wai = liftF $ Wai ()

sugoi :: Free Friends ()
sugoi = liftF $ Sugoi ()

tanoshii :: Free Friends ()
tanoshii = liftF $ Tanoshii ()

tokuiNandane :: String -> Free Friends ()
tokuiNandane skill  = liftF $ TokuiNandane skill ()

runFriendsJp :: Free Friends a -> IO a
runFriendsJp f = foldFree goFriendsJp f

goFriendsJp :: Friends a -> IO a
goFriendsJp (Wai next) = do
  putStrLn "わーい！"
  pure next
goFriendsJp (Sugoi next) = do
  putStrLn "すごーい！"
  pure next
goFriendsJp (Tanoshii next) = do
  putStrLn "たーのしー！"
  pure next
goFriendsJp (TokuiNandane skill next) = do
  putStrLn ("君は" <> skill <> "が得意なフレンズなんだね！")
  pure next

runFriendsEn :: Free Friends a -> IO a
runFriendsEn f = foldFree goFriendsEn f

goFriendsEn :: Friends a -> IO a
goFriendsEn (Wai next) = do
  putStrLn "Wow!"
  pure next
goFriendsEn (Sugoi next) = do
  putStrLn "Great!"
  pure next
goFriendsEn (Tanoshii next) = do
  putStrLn "Wonderful!"
  pure next
goFriendsEn (TokuiNandane skill next) = do
  putStrLn ("You are friends that good at " <> skill <> "!")
  pure next

friends :: String -> Free Friends ()
friends skill = do
  wai
  sugoi
  tanoshii
  tokuiNandane skill

main :: IO ()
main = do
  putStrLn "◆フレンズ（日本語）◆"
  runFriendsJp $ friends "クソコード"
  putStrLn "\n◆Friends (English)◆"
  runFriendsEn $ friends "Crappy Code"

