{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE Strict #-}
module Foreign.RSDD
  ( varOrderLinear,
    -- , cnfFromDimacs
    roBddBuilderAllTable,
    compileCnf,
    modelCount,
    roBddBuilderDefaultOrder,
    newVar,
    newBddPtr,
    bddVar,
    ite,
    iff,
    bddAnd,
    bddOr,
    bddNeg,
    isTrue,
    isFalse,
    isConst,
    ptrTrue,
    ptrFalse,
    Foreign.RSDD.fromBool,
    bddEq,
    topvar,
    low,
    high,
    newWmc,
    bddWmc,
    Accept(..),
    wmc,
    wmc1,
    setWeight,
    setHigh,
    setHighP,
    setLow,
    setLowP,
    varWeight,
    printBdd,
    BddBuilder,
    VarOrder,
    Cnf,
    BddPtr,
    VarLabel(..),
    WmcParams,
    WmcParamsT,
    mkTypedWmcParams,
    WmcParams1,
    Weight(..),
  )
where

import Foreign
import Foreign.C.String
import Foreign.C.Types
import GHC.Natural
import GHC.TypeNats
import Data.Proxy
import System.IO.Unsafe
import Control.Monad.IO.Class

-- dummy rust data types
data RawRsddBddBuilder

data RawVarOrder

data RawCnf

data RawBddPtr

data RawRsddWmcParamsR

newtype BddBuilder = BddBuilder (Ptr RawRsddBddBuilder)

newtype VarOrder = VarOrder (Ptr RawVarOrder)

newtype Cnf = Cnf (Ptr RawCnf)

newtype BddPtr = BddPtr (Ptr RawBddPtr)

newtype VarLabel = VarLabel Natural

newtype WmcParams = WmcParams (Ptr RawRsddWmcParamsR)

newtype WmcParamsT (m :: Nat) = WmcParamsT { unbound :: WmcParams }

mkTypedWmcParams :: WmcParams -> WmcParamsT (m :: Nat)
mkTypedWmcParams wm = WmcParamsT wm

type WmcParams1 = WmcParamsT 1

-- Call to the Rust function 'var_order_linear' which returns a pointer to a VarOrder structure
foreign import ccall unsafe "var_order_linear"
  c_var_order_linear ::
    CSize -> IO VarOrder

-- return a variable order for a given number of variables
varOrderLinear :: Natural -> VarOrder
varOrderLinear numVars = unsafePerformIO $ c_var_order_linear (fromIntegral numVars)

-- -- Call to the Rust function 'cnf_from_dimacs' which takes a C string and returns a pointer to a Cnf structure
-- foreign import ccall unsafe "cnf_from_dimacs" c_cnf_from_dimacs
--     :: Ptr CChar -> IO Cnf

foreign import ccall unsafe "robdd_builder_all_table"
  roBddBuilderAllTable ::
    VarOrder -> IO BddBuilder

foreign import ccall unsafe "robdd_builder_compile_cnf"
  compileCnf ::
    BddBuilder -> Cnf -> IO BddPtr

foreign import ccall unsafe "robdd_model_count"
  c_robdd_model_count ::
    BddBuilder -> BddPtr -> IO Word64

modelCount :: BddBuilder -> BddPtr -> Natural
modelCount mgr ptr = unsafePerformIO $ fromIntegral <$> c_robdd_model_count mgr ptr

-- Creates a new BDD builder with a default variable order
foreign import ccall unsafe "mk_bdd_manager_default_order"
  c_mk_bdd_manager_default_order ::
    Word64 -> IO BddBuilder

roBddBuilderDefaultOrder :: MonadIO io => Natural -> io BddBuilder
roBddBuilderDefaultOrder n = liftIO $ c_mk_bdd_manager_default_order (fromIntegral n)

foreign import ccall unsafe "bdd_new_label"
  c_bdd_new_label :: BddBuilder -> IO Word64

foreign import ccall unsafe "bdd_var"
  c_bdd_var :: BddBuilder -> Word64 -> Bool -> IO BddPtr

bddVar :: MonadIO io => BddBuilder -> VarLabel -> Bool -> io BddPtr
bddVar mgr (VarLabel lbl) pol = liftIO $ c_bdd_var mgr (fromIntegral lbl) pol

-- getOrCreate :: MonadIO io => BddBuilder -> VarLabel -> io BddPtr
-- getOrCreate mgr (VarLabel lbl) =
--   c_bdd_var mgr (fromIntegral lbl) True


newVar :: MonadIO io => BddBuilder -> Bool -> io (VarLabel, BddPtr)
newVar mgr pol = liftIO $ do
  lbl <- c_bdd_new_label mgr
  ptr <- c_bdd_var mgr lbl pol
  pure ((VarLabel . fromIntegral) lbl, ptr)

newBddPtr :: MonadIO io => BddBuilder -> Bool -> io BddPtr
newBddPtr mgr pol = snd <$> newVar mgr pol

-- BDD operations: If-Then-Else
foreign import ccall unsafe "bdd_ite"
  bdd_ite ::
    BddBuilder -> BddPtr -> BddPtr -> BddPtr -> IO BddPtr

ite :: MonadIO io => BddBuilder -> BddPtr -> BddPtr -> BddPtr -> io BddPtr
ite a b c d = liftIO $ bdd_ite a b c d

-- BDD operations: if and only if
iff :: MonadIO io => BddBuilder -> BddPtr -> BddPtr -> io BddPtr
iff m f g = bddNeg m g >>= ite m f g

-- BDD operations: And
foreign import ccall unsafe "bdd_and"
  bdd_and ::
    BddBuilder -> BddPtr -> BddPtr -> IO BddPtr

bddAnd :: MonadIO io => BddBuilder -> BddPtr -> BddPtr -> io BddPtr
bddAnd a b c = liftIO $ bdd_and a b c

-- BDD operations: Or
foreign import ccall unsafe "bdd_or"
  bdd_or ::
    BddBuilder -> BddPtr -> BddPtr -> IO BddPtr

bddOr :: MonadIO io => BddBuilder -> BddPtr -> BddPtr -> io BddPtr
bddOr a b c = liftIO $ bdd_or a b c

-- BDD operations: Negate
foreign import ccall unsafe "bdd_negate"
  bdd_neg ::
    BddBuilder -> BddPtr -> IO BddPtr

bddNeg :: MonadIO io => BddBuilder -> BddPtr -> io BddPtr
bddNeg a b = liftIO $ bdd_neg a b

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
  ptrTrue :: BddBuilder -> BddPtr

-- Create constant BDD nodes of False
foreign import ccall unsafe "bdd_false"
  ptrFalse :: BddBuilder -> BddPtr

fromBool :: BddBuilder -> Bool -> BddPtr
fromBool mgr True = ptrTrue mgr
fromBool mgr False = ptrFalse mgr

-- Compare two BDD nodes for equality
foreign import ccall unsafe "bdd_eq"
  bddEq :: BddBuilder -> BddPtr -> BddPtr -> Bool

-- Get the top variable of a BDD node
foreign import ccall unsafe "bdd_topvar"
  c_bdd_topvar ::
    BddPtr -> IO Word64

topvar :: BddPtr -> VarLabel
topvar ptr = unsafePerformIO $ VarLabel . fromIntegral <$> c_bdd_topvar ptr

-- Get the low edge of a BDD node
foreign import ccall unsafe "bdd_low"
  low :: BddPtr -> BddPtr

-- Get the high edge of a BDD node
foreign import ccall unsafe "bdd_high"
  high :: BddPtr -> BddPtr

foreign import ccall unsafe "new_wmc_params_f64"
  newWmc :: WmcParams

foreign import ccall unsafe "bdd_wmc"
  bddWmc :: BddPtr -> WmcParams -> Double

newtype Accept = Accept { accepting :: BddPtr }

wmc :: BddBuilder -> WmcParams -> [BddPtr] -> Accept -> [Double]
wmc m w qs a = unsafePerformIO $ do
  ns :: [BddPtr] <- mapM (bddAnd m $ accepting a) qs
  pure $ fmap (\n -> bddWmc n w / z) ns
  where
      z = bddWmc (accepting a) w

wmc1 :: BddBuilder -> WmcParams -> BddPtr -> Accept -> Double
wmc1 m w q a = head $ wmc m w [q] a


foreign import ccall unsafe "wmc_param_f64_set_weight"
  c_wmc_param_f64_set_weight :: WmcParams -> Word64 -> Double -> Double -> IO ()

data Weight = Weight
  { lo :: Double
  , hi :: Double
  } deriving (Show, Eq, Ord)

set_weight :: WmcParams -> VarLabel -> Weight -> IO ()
set_weight wm (VarLabel n) w = c_wmc_param_f64_set_weight wm (fromIntegral n) (lo w) (hi w)
setWeight :: MonadIO io => WmcParams -> VarLabel -> Weight -> io ()
setWeight a b c = liftIO $ set_weight a b c

set_high :: forall mx . KnownNat mx => WmcParamsT mx -> VarLabel -> Double -> IO ()
set_high wm vl hi = set_weight (unbound wm) vl (Weight (x-hi) hi)
  where
    x :: Double
    x = fromIntegral $ natVal (Proxy @mx)

setHigh :: forall mx io . KnownNat mx => MonadIO io => WmcParamsT mx -> VarLabel -> Double -> io ()
setHigh wm vl hi = liftIO $ set_high wm vl hi

setHighP :: MonadIO io => WmcParams -> VarLabel -> Double -> io ()
setHighP w = setHigh wm
  where
    wm :: WmcParamsT 1
    wm = mkTypedWmcParams w

set_low :: forall mx . KnownNat mx => WmcParamsT mx -> VarLabel -> Double -> IO ()
set_low wm vl lo = set_weight (unbound wm) vl (Weight lo (x-lo))
  where
    x :: Double
    x = fromIntegral $ natVal (Proxy @mx)

setLow :: forall mx io . KnownNat mx => MonadIO io => WmcParamsT mx -> VarLabel -> Double -> io ()
setLow wm vl hi = liftIO $ set_low wm vl hi

setLowP :: MonadIO io => WmcParams -> VarLabel -> Double -> io ()
setLowP w = setLow wm
  where
    wm :: WmcParamsT 1
    wm = mkTypedWmcParams w

data RawRsddWmcWeightR

foreign import ccall unsafe "wmc_param_f64_var_weight"
  c_wmc_param_f64_var_weight :: WmcParams -> Word64 -> IO (Ptr RawRsddWmcWeightR)

foreign import ccall unsafe "weight_f64_lo"
  c_weight_f64_lo :: Ptr RawRsddWmcWeightR -> IO Double

foreign import ccall unsafe "weight_f64_hi"
  c_weight_f64_hi :: Ptr RawRsddWmcWeightR -> IO Double

varWeight :: WmcParams -> VarLabel -> Weight
varWeight wm (VarLabel v) = unsafePerformIO $
  c_wmc_param_f64_var_weight wm (fromIntegral v) >>= \n ->
    Weight
      <$> c_weight_f64_lo n
      <*> c_weight_f64_hi n

foreign import ccall unsafe "print_bdd"
  c_print_bdd :: BddPtr -> IO CString

print_bdd :: BddPtr -> IO String
print_bdd ptr = c_print_bdd ptr >>= peekCString

printBdd :: MonadIO io => BddPtr -> io String
printBdd = liftIO . print_bdd

instance Show BddPtr where
  show = unsafePerformIO . printBdd

-------------------------

-- * Haskell-friendly wrappers

-- -- Converts a Haskell String to a C string, calls the Rust function, and returns a pointer to Cnf
-- cnfFromDimacs :: String -> IO Cnf
-- cnfFromDimacs dimacsStr = withCString dimacsStr c_cnf_from_dimacs
