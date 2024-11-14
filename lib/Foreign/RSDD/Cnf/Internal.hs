module Foreign.RSDD.Cnf.Internal where

import Foreign.RSDD.Data

import Foreign


foreign import ccall unsafe "cnf_new"
  c_cnf_new :: Clause -> Cnf

foreign import ccall unsafe "cnf_min_fill_order"
  c_cnf_min_fill_order :: Cnf -> VarOrder
