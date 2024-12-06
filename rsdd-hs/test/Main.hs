{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where

import Foreign.RSDD
import Control.Exception

import Test.Hspec

main :: IO ()
main = hspec $ around (bracket (roBddBuilderDefaultOrder 0)  (const $ pure ())) $ do
  describe "show instance" $ do
    it "shows the right representations" $ \mgr -> do
      (la, a) <- newVar mgr True
      prettyBddIO a >>= (`shouldBe` "(0, F, T)")
      show la `shouldBe` "VarLabel 0"
  describe "bddEq" $ do
    context "constant equality" $ do
      it "is reflexive" $ \mgr -> do
        bddEq mgr ptrTrue ptrTrue `shouldBe` True
        bddEq mgr ptrFalse ptrFalse `shouldBe` True
      it "ff /= tt" $ \mgr -> do
        bddEq mgr ptrFalse ptrTrue `shouldBe` False
      it "tt /= ff" $ \mgr -> do
        bddEq mgr ptrTrue ptrFalse `shouldBe` False
    context "variable equality" $ do
      it "is reflexive" $ \mgr -> do
        a <- newBddPtr mgr True
        bddEq mgr a a `shouldBe` True
      it "is false for different variables" $ \mgr -> do
        a <- newBddPtr mgr True
        b <- newBddPtr mgr True
        bddEq mgr a b `shouldBe` False

  describe "ite" $ do
    it "shows the correct constant" $ \mgr -> do
      let tt = ptrTrue
      let ff = ptrFalse
      ret <- ite mgr tt tt ff
      prettyBddIO ret >>= (`shouldBe` "T")
      ret <- ite mgr ff tt ff
      prettyBddIO ret >>= (`shouldBe` "F")
    it "returns a new pointer, so equality on the nose fails" $ \mgr -> do
      let tt = ptrTrue
      let ff = ptrFalse
      ret <- ite mgr tt tt ff
      ret == tt `shouldBe` False
      ret <- ite mgr ff tt ff
      ret == ff `shouldBe` False
    it "is equal by bdd equality" $ \mgr -> do
      let tt = ptrTrue
      let ff = ptrFalse
      ret <- ite mgr tt tt ff
      bddEq mgr ret tt `shouldBe` True
      ret <- ite mgr ff tt ff
      bddEq mgr ret ff `shouldBe` True

  describe "wmc" $ do
    it "can initialize as empty and have weights added" $ \_ -> do
      p <- wmcParamsIO
      let w = Weight { hi = 1/3, lo = 2/3 }
      let l = (VarLabel 0)
      setWeight p l w
      let w' = varWeight p l
      w' `shouldBe` Just w

      -- setWeight w lb $ Weight { hi = 1/4, lo = 3/4 }






-- main :: IO ()
-- main = do
--   mgr <- roBddBuilderDefaultOrder 0
--   print b
--   ite mgr a b t >>= print
--   ite mgr t t t >>= print
--   print $ varWeight w la
--   print $ varWeight w lb
--   a_or_b <- bddOr mgr a b
--   printBdd a_or_b >>= putStrLn
--   let x = bddWmc a_or_b w
--   print ("hello", x)
--   pure ()
