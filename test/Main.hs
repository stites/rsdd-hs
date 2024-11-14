module Main (main) where

import Foreign.RSDD
import Control.Exception

import Test.Hspec

main :: IO ()
main = hspec $ around (bracket (roBddBuilderDefaultOrder 0)  (const $ pure ())) $ do
  describe "show instance" $ do
    it "shows the right representations" $ \mgr -> do
      (la, a) <- newVar mgr True
      show a `shouldBe` "(0, F, T)"
      show la `shouldBe` "VarLabel 0"
  describe "bddEq" $ do
    context "constant equality" $ do
      it "is reflexive" $ \mgr -> do
        let tt = ptrTrue
        let ff = ptrFalse
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
    it "choses the correct constant" $ \mgr -> do
      pendingWith "fix equality"
      let tt = ptrTrue
      let ff = ptrFalse
      ret <- ite mgr tt tt ff
      show ret `shouldBe` "T"
      ret <- ite mgr ff tt ff
      show ret `shouldBe` "F"



-- main :: IO ()
-- main = do
--   mgr <- roBddBuilderDefaultOrder 0
--   print b
--   ite mgr a b t >>= print
--   ite mgr t t t >>= print
--   let w = newWmc
--   setWeight w la $ Weight { hi = 1/3, lo = 2/3 }
--   setWeight w lb $ Weight { hi = 1/4, lo = 3/4 }
--   print $ varWeight w la
--   print $ varWeight w lb
--   a_or_b <- bddOr mgr a b
--   printBdd a_or_b >>= putStrLn
--   let x = bddWmc a_or_b w
--   print ("hello", x)
--   pure ()
