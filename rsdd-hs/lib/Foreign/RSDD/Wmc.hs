{-# LANGUAGE DataKinds #-}
module Foreign.RSDD.Wmc
  ( wmcParamsIO
  , setWeight
  , setHigh
  , setLow
  , setHighP
  , setLowP
  , varWeight
  ) where

import Foreign.RSDD.Data
import Foreign.RSDD.Wmc.Internal



import System.IO.Unsafe (unsafePerformIO)
import GHC.TypeNats (KnownNat, natVal)
import GHC.Utils.Panic
import GHC.Utils.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy(..))

wmcParamsIO :: MonadIO io => io WmcParams
wmcParamsIO = liftIO c_wmc_params

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

varWeight :: WmcParams -> VarLabel -> Maybe Weight
varWeight wm (VarLabel v)
  = unsafePerformIO
  $ flip catch (\(e::SomeException) -> pure Nothing) -- FIXME: this doesn't seem to be working
  $ c_wmc_param_f64_var_weight wm (fromIntegral v) >>= \n -> do
    lo <- c_weight_f64_lo n
    hi <- c_weight_f64_hi n
    pure . Just $ Weight lo hi


-------------------------

-- * Haskell-friendly wrappers

-- -- Converts a Haskell String to a C string, calls the Rust function, and returns a pointer to Cnf
-- cnfFromDimacs :: String -> IO Cnf
-- cnfFromDimacs dimacsStr = withCString dimacsStr c_cnf_from_dimacs
