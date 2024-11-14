{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Foreign.RSDD.Bdd
  ( varOrderLinear
  , roBddBuilderAllTable
  , compileCnf
  , modelCount
  , roBddBuilderDefaultOrder
  , bddVar
  , newVar
  , newBddPtr
  , ite
  , iff
  , bddOr
  , bddNeg
  , bddAnd
  , I.isTrue
  , I.isFalse
  , I.isConst
  , I.bddEq
  , ptrTrue
  , ptrFalse
  , Foreign.RSDD.Bdd.fromBool
  , topvar
  , I.low
  , I.high
  , I.bddWmc
  , wmc
  , wmc1
  , printBdd
  , numRecursiveCalls
  ) where

import Foreign.RSDD.Data
import Foreign.RSDD.Bdd.Internal as I

import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Natural
import GHC.TypeNats
import Data.Proxy
import System.IO.Unsafe
import Control.Monad.IO.Class

-- return a variable order for a given number of variables
varOrderLinear :: Natural -> VarOrder
varOrderLinear numVars = unsafePerformIO $ c_var_order_linear (fromIntegral numVars)

roBddBuilderAllTable :: MonadIO io => VarOrder -> io BddBuilder
roBddBuilderAllTable = liftIO . c_robdd_builder_all_table

compileCnf :: BddBuilder -> Cnf -> BddPtr
compileCnf a b = unsafePerformIO $ c_robdd_builder_compile_cnf a b

modelCount :: BddBuilder -> BddPtr -> Natural
modelCount mgr ptr = unsafePerformIO $ fromIntegral <$> c_robdd_model_count mgr ptr

roBddBuilderDefaultOrder :: MonadIO io => Natural -> io BddBuilder
roBddBuilderDefaultOrder n = liftIO $ c_mk_bdd_manager_default_order (fromIntegral n)

bddVar :: MonadIO io => BddBuilder -> VarLabel -> Bool -> io BddPtr
bddVar mgr (VarLabel lbl) pol = liftIO $ c_bdd_var mgr (fromIntegral lbl) pol

newVar :: MonadIO io => BddBuilder -> Bool -> io (VarLabel, BddPtr)
newVar mgr pol = liftIO $ do
  lbl <- c_bdd_new_label mgr
  ptr <- c_bdd_var mgr lbl pol
  pure ((VarLabel . fromIntegral) lbl, ptr)

newBddPtr :: MonadIO io => BddBuilder -> Bool -> io BddPtr
newBddPtr mgr pol = snd <$> newVar mgr pol

ite :: MonadIO io => BddBuilder -> BddPtr -> BddPtr -> BddPtr -> io BddPtr
ite a b c d = liftIO $ c_bdd_ite a b c d

-- BDD operations: if and only if
iff :: MonadIO io => BddBuilder -> BddPtr -> BddPtr -> io BddPtr
iff m f g = bddNeg m g >>= ite m f g

bddAnd :: MonadIO io => BddBuilder -> BddPtr -> BddPtr -> io BddPtr
bddAnd a b c = liftIO $ c_bdd_and a b c

bddOr :: MonadIO io => BddBuilder -> BddPtr -> BddPtr -> io BddPtr
bddOr a b c = liftIO $ c_bdd_or a b c

bddNeg :: MonadIO io => BddBuilder -> BddPtr -> io BddPtr
bddNeg a b = liftIO $ c_bdd_neg a b

{-# NOINLINE ptrTrue #-}
ptrTrue :: BddPtr
ptrTrue = unsafePerformIO $ c_ptr_true

{-# NOINLINE ptrFalse #-}
ptrFalse :: BddPtr
ptrFalse = unsafePerformIO $ c_ptr_false

fromBool :: Bool -> BddPtr
fromBool True = ptrTrue
fromBool False = ptrFalse

topvar :: BddPtr -> VarLabel
topvar ptr = unsafePerformIO $ VarLabel . fromIntegral <$> c_bdd_topvar ptr

wmc :: BddBuilder -> WmcParams -> [BddPtr] -> Accept -> [Double]
wmc m w qs a = unsafePerformIO $ do
  ns :: [BddPtr] <- mapM (bddAnd m $ accepting a) qs
  pure $ fmap (\n -> bddWmc n w / z) ns
  where
      z = bddWmc (accepting a) w

wmc1 :: BddBuilder -> WmcParams -> BddPtr -> Accept -> Double
wmc1 m w q a = head $ wmc m w [q] a

print_bdd :: BddPtr -> IO String
print_bdd ptr = c_print_bdd ptr >>= peekCString

printBdd :: MonadIO io => BddPtr -> io String
printBdd = liftIO . print_bdd

instance Show BddPtr where
  show = unsafePerformIO . printBdd

numRecursiveCalls :: BddBuilder -> Natural
numRecursiveCalls = fromIntegral . c_bdd_num_recursive_calls

