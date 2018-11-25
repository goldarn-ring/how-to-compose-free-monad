module Ninja (main, Ninja(..), domo, yeeart, aieee, sayonara, runNinjaJp, runNinjaEn, goNinjaJp, goNinjaEn) where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Control.Monad.Free (Free, foldFree, liftF)

data Ninja a
  = Domo String a
  | Yeeart a
  | Aieee a
  | Sayonara String a

domo :: String -> Free Ninja Unit
domo name = liftF $ Domo name unit

yeeart :: Free Ninja Unit
yeeart = liftF $ Yeeart unit

aieee :: Free Ninja Unit
aieee = liftF $ Aieee unit

sayonara :: String -> Free Ninja Unit
sayonara name = liftF $ Sayonara name unit

runNinjaJp :: forall a. Free Ninja a -> Effect a
runNinjaJp f = foldFree goNinjaJp f

goNinjaJp :: forall a. Ninja a ->  Effect a
goNinjaJp (Domo name next) = do
  log ("ドーモ、" <> name <> "＝サン") 
  pure next
goNinjaJp (Yeeart next) = do
  log "イヤーッ！"
  pure next
goNinjaJp (Aieee next) = do
  log "アイエエエ！？"
  pure next
goNinjaJp (Sayonara name next) = do
  log ("サヨナラ！ " <> name <> "＝サンは爆発四散！")
  pure next


runNinjaEn :: forall a. Free Ninja a -> Effect a
runNinjaEn f = foldFree goNinjaEn f

goNinjaEn :: forall a. Ninja a ->  Effect a
goNinjaEn (Domo name next) = do
  log ("Do-mo, " <> name <> "-san")
  pure next
goNinjaEn (Yeeart next) = do
  log "Yeeart!"
  pure next
goNinjaEn (Aieee next) = do
  log "AIEEE!?"
  pure next
goNinjaEn (Sayonara name next) = do
  log ("Sayonara! " <> name <> "-san explodes!")
  pure next

ninja :: String -> String -> Free Ninja Unit
ninja ninja1 ninja2 = do
  domo ninja1
  yeeart
  aieee
  sayonara ninja2

main :: Effect Unit
main =  do
  log "◆ニンジャ（日本語）◆"
  runNinjaJp $ ninja "サーバル" "カバン"
  log "\n◆Ninja (English)◆"
  runNinjaEn $ ninja "Serval" "Kaban"
