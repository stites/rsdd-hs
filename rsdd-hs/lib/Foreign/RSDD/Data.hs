{-# LANGUAGE DataKinds #-}
module Foreign.RSDD.Data where

import Foreign (Ptr)
import GHC.Natural (Natural)
import GHC.TypeNats (Nat)

-- dummy rust data types
data RawRsddBddBuilder

data RawVarOrder

data RawCnf

data RawClause

data RawBddPtr

data RawRsddWmcParamsR

newtype BddBuilder = BddBuilder (Ptr RawRsddBddBuilder)
  deriving Eq

newtype VarOrder = VarOrder (Ptr RawVarOrder)
  deriving (Show, Eq)

newtype Cnf = Cnf (Ptr RawCnf)

newtype Clause = Clause (Ptr RawCnf)

newtype BddPtr = BddPtr (Ptr RawBddPtr)
  deriving Eq

newtype VarLabel = VarLabel Natural
  deriving (Show, Eq, Ord)

newtype WmcParams = WmcParams (Ptr RawRsddWmcParamsR)

newtype WmcParamsT (m :: Nat) = WmcParamsT { unbound :: WmcParams }

-- FIXME: redundant?
mkTypedWmcParams :: WmcParams -> WmcParamsT (m :: Nat)
mkTypedWmcParams = WmcParamsT

type WmcParams1 = WmcParamsT 1


newtype Accept = Accept { accepting :: BddPtr }

data RawRsddWmcWeightR

data Weight = Weight
  { lo :: Double
  , hi :: Double
  } deriving (Show, Eq, Ord)
