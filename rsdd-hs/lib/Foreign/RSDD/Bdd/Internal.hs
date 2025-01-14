{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}
module Foreign.RSDD.Bdd.Internal where

import Foreign.RSDD.Data

import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Natural
import GHC.TypeNats
import Data.Proxy
import System.IO.Unsafe
import Control.Monad.IO.Class

-- Call to the Rust function 'var_order_linear' which returns a pointer to a VarOrder structure
foreign import ccall unsafe "var_order_linear"
  c_var_order_linear :: CSize -> IO VarOrder

-- Call to the Rust function 'cnf_from_dimacs' which takes a C string and returns a pointer to a Cnf structure
foreign import ccall unsafe "cnf_from_dimacs"
  c_cnf_from_dimacs :: CString -> IO Cnf

foreign import ccall unsafe "robdd_builder_all_table"
  c_robdd_builder_all_table :: VarOrder -> IO BddBuilder

foreign import ccall unsafe "robdd_builder_compile_cnf"
  c_robdd_builder_compile_cnf :: BddBuilder -> Cnf -> IO BddPtr

foreign import ccall unsafe "robdd_model_count"
  c_robdd_model_count :: BddBuilder -> BddPtr -> IO Word64

-- Creates a new BDD builder with a default variable order
foreign import ccall unsafe "mk_bdd_manager_default_order"
  c_mk_bdd_manager_default_order :: Word64 -> IO BddBuilder

foreign import ccall unsafe "bdd_new_label"
  c_bdd_new_label :: BddBuilder -> IO Word64

foreign import ccall unsafe "bdd_var"
  c_bdd_var :: BddBuilder -> Word64 -> Bool -> IO BddPtr

foreign import ccall unsafe "bdd_new_var"
  c_bdd_new_var :: BddBuilder -> Bool -> IO BddPtr

foreign import ccall unsafe "bdd_ite"
  c_bdd_ite :: BddBuilder -> BddPtr -> BddPtr -> BddPtr -> IO BddPtr

foreign import ccall unsafe "bdd_and"
  c_bdd_and :: BddBuilder -> BddPtr -> BddPtr -> IO BddPtr

foreign import ccall unsafe "bdd_or"
  c_bdd_or :: BddBuilder -> BddPtr -> BddPtr -> IO BddPtr

foreign import ccall unsafe "bdd_negate"
  c_bdd_neg :: BddBuilder -> BddPtr -> IO BddPtr

-- checks if the BddPtr is a constant and is PtrTrue
foreign import ccall unsafe "bdd_is_true"
  isTrue :: BddPtr -> Bool

-- checks if the BddPtr is a constant and is PtrFalse
foreign import ccall unsafe "bdd_is_false"
  isFalse :: BddPtr -> Bool

-- checks if the BddPtr is a constant (meaning either PtrTrue or PtrFalse)
foreign import ccall unsafe "bdd_is_const"
  isConst :: BddPtr -> Bool

-- Create constant BDD nodes of True
foreign import ccall unsafe "bdd_true"
  c_ptr_true :: IO BddPtr

foreign import ccall unsafe "bdd_false"
  c_ptr_false :: IO BddPtr

-- Compare two BDD nodes for equality
foreign import ccall unsafe "bdd_eq"
  bddEq :: BddBuilder -> BddPtr -> BddPtr -> IO Bool

-- Get the top variable of a BDD node
foreign import ccall unsafe "bdd_topvar"
  c_bdd_topvar :: BddPtr -> IO Word64

-- Get the low edge of a BDD node
foreign import ccall unsafe "bdd_low"
  low :: BddPtr -> BddPtr

-- Get the high edge of a BDD node
foreign import ccall unsafe "bdd_high"
  high :: BddPtr -> BddPtr

foreign import ccall unsafe "print_bdd"
  c_print_bdd :: BddPtr -> IO CString

foreign import ccall unsafe "bdd_num_recursive_calls"
  c_bdd_num_recursive_calls :: BddBuilder -> CUInt

foreign import ccall unsafe "bdd_wmc"
  bddWmc :: BddPtr -> WmcParams -> Double

-- foreign import ccall unsafe "bdd_wmc_complex"
--   c_bdd_wmc_complex :: BddPtr -> WmcParams -> (Double, Double)
