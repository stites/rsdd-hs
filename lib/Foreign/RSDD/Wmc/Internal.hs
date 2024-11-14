{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE Strict #-}
module Foreign.RSDD.Wmc.Internal where

import Foreign.RSDD.Data

import Foreign


foreign import ccall unsafe "new_wmc_params_f64"
  c_wmc_params :: IO WmcParams

foreign import ccall unsafe "wmc_param_f64_set_weight"
  c_wmc_param_f64_set_weight :: WmcParams -> Word64 -> Double -> Double -> IO ()

foreign import ccall unsafe "wmc_param_f64_var_weight"
  c_wmc_param_f64_var_weight :: WmcParams -> Word64 -> IO (Ptr RawRsddWmcWeightR)

foreign import ccall unsafe "weight_f64_lo"
  c_weight_f64_lo :: Ptr RawRsddWmcWeightR -> IO Double

foreign import ccall unsafe "weight_f64_hi"
  c_weight_f64_hi :: Ptr RawRsddWmcWeightR -> IO Double


-- foreign import ccall unsafe "new_wmc_params_complex"
--   c_wmc_params_complex :: IO WmcParams
--
-- foreign import ccall unsafe "wmc_param_complex_set_weight"
--   c_wmc_param_complex_set_weight :: WmcParams -> Word64 -> Double -> Double -> IO ()
--
-- foreign import ccall unsafe "wmc_param_complex_var_weight"
--   c_wmc_param_complex_var_weight :: WmcParams -> Word64 -> IO (Ptr RawRsddWmcWeightR)
--
-- foreign import ccall unsafe "weight_complex_lo"
--   c_weight_complex_lo :: Ptr RawRsddWmcWeightR -> IO Double
--
-- foreign import ccall unsafe "weight_complex_hi"
--   c_weight_complex_hi :: Ptr RawRsddWmcWeightR -> IO Double
