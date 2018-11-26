{-# LANGUAGE DeriveFunctor #-}

module Ninja (main, Ninja(..), domo, yeeart, aieee, sayonara, runNinjaJp, runNinjaEn, goNinjaJp, goNinjaEn) where

import Control.Monad.Free (Free, foldFree, liftF)

data Ninja a
  = Domo String a
  | Yeeart a
  | Aieee a
  | Sayonara String a
  deriving (Functor)

domo :: String -> Free Ninja ()
domo name = liftF $ Domo name ()

yeeart :: Free Ninja ()
yeeart = liftF $ Yeeart ()

aieee :: Free Ninja ()
aieee = liftF $ Aieee ()

sayonara :: String -> Free Ninja ()
sayonara name = liftF $ Sayonara name ()

runNinjaJp :: Free Ninja a -> IO a
runNinjaJp f = foldFree goNinjaJp f

goNinjaJp :: Ninja a ->  IO a
goNinjaJp (Domo name next) = do
  putStrLn ("ドーモ、" <> name <> "＝サン") 
  pure next
goNinjaJp (Yeeart next) = do
  putStrLn "イヤーッ！"
  pure next
goNinjaJp (Aieee next) = do
  putStrLn "アイエエエ！？"
  pure next
goNinjaJp (Sayonara name next) = do
  putStrLn ("サヨナラ！ " <> name <> "＝サンは爆発四散！")
  pure next


runNinjaEn :: Free Ninja a -> IO a
runNinjaEn f = foldFree goNinjaEn f

goNinjaEn :: Ninja a ->  IO a
goNinjaEn (Domo name next) = do
  putStrLn ("Do-mo, " <> name <> "-san")
  pure next
goNinjaEn (Yeeart next) = do
  putStrLn "Yeeart!"
  pure next
goNinjaEn (Aieee next) = do
  putStrLn "AIEEE!?"
  pure next
goNinjaEn (Sayonara name next) = do
  putStrLn ("Sayonara! " <> name <> "-san explodes!")
  pure next

ninja :: String -> String -> Free Ninja ()
ninja ninja1 ninja2 = do
  domo ninja1
  yeeart
  aieee
  sayonara ninja2

main :: IO ()
main =  do
  putStrLn "◆ニンジャ（日本語）◆"
  runNinjaJp $ ninja "サーバル" "カバン"
  putStrLn "\n◆Ninja (English)◆"
  runNinjaEn $ ninja "Serval" "Kaban"
