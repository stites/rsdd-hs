{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Effectful.RSDD
  ( -- BddMgr,
    RSDD.varOrderLinear,
    bddBuilder,
    compileCnf,
    modelCount,
    defaultBddBuilder,
    defaultEmptyBddBuilder,
    newVar,
    newBddPtr,
    ite,
    bddAnd,
    bddOr,
    bddNeg,
    RSDD.isTrue,
    RSDD.isFalse,
    RSDD.isConst,
    RSDD.ptrTrue,
    RSDD.ptrFalse,
    RSDD.bddEq,
    RSDD.topvar,
    RSDD.low,
    RSDD.high,
    RSDD.wmc,
    RSDD.bddWmc,
    RSDD.mkTypedWmcParams,
    bddWmc',
    setWeights,
    setHigh1,
    setLow1,
    RSDD.varWeight,
    RSDD.printBdd,
    BddBuilder,
    VarOrder,
    Cnf,
    BddPtr,
    VarLabel,
    WmcParams,
    RSDD.WmcParamsT,
    RSDD.WmcParams1,
    RSDD.Weight (..),
  )
where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static
import Effectful.Reader.Dynamic
import Foreign.RSDD (BddBuilder, BddPtr, Cnf, VarLabel, VarOrder, WmcParams)
import Foreign.RSDD qualified as RSDD
import GHC.Natural (Natural)
import GHC.TypeNats (Nat)

-- data BddMgr :: Effect

-- type instance DispatchOf BddMgr = Static WithSideEffects
-- data instance StaticRep BddMgr = BddMgr

-- * Handlers + helpers

bddBuilder :: (IOE :> es) => VarOrder -> Eff es BddBuilder
bddBuilder = liftIO . RSDD.roBddBuilderAllTable

defaultBddBuilder :: (IOE :> es) => Natural -> Eff es BddBuilder
defaultBddBuilder = liftIO . RSDD.roBddBuilderDefaultOrder

defaultEmptyBddBuilder :: (IOE :> es) => Eff es BddBuilder
defaultEmptyBddBuilder = defaultBddBuilder 0

-- runManager :: (BddMgr :> es, Reader BddBuilder :> es) => BddBuilder -> Eff es x -> Eff es x
-- runManager mgr act = do
--    BddMgr <- getStaticRep
--    reinterpret runReader mgr (reinterpret act)

-- withDefaultBuilder :: (IOE :> es, Reader BddBuilder :> es, BddMgr :> es) => Natural -> Eff es x -> Eff es x
-- withDefaultBuilder n act = do
--   mgr <- defaultBddBuilder n
--   runManager mgr

-- * API

compileCnf :: (IOE :> es) => (Reader BddBuilder :> es) => Cnf -> Eff es BddPtr
compileCnf a = flip RSDD.compileCnf a <$> ask

modelCount :: (IOE :> es) => (Reader BddBuilder :> es) => BddPtr -> Eff es Natural
modelCount a = flip RSDD.modelCount a <$> ask

newVar :: (IOE :> es) => (Reader BddBuilder :> es) => Bool -> Eff es (VarLabel, BddPtr)
newVar a = ask >>= liftIO . flip RSDD.newVar a

newBddPtr :: (IOE :> es) => (Reader BddBuilder :> es) => Bool -> Eff es BddPtr
newBddPtr a = ask >>= \m -> liftIO $ RSDD.newBddPtr m a

ite :: (IOE :> es) => (Reader BddBuilder :> es) => BddPtr -> BddPtr -> BddPtr -> Eff es BddPtr
ite a b c = ask >>= \m -> liftIO $ RSDD.ite m a b c

bddAnd :: (IOE :> es) => (Reader BddBuilder :> es) => BddPtr -> BddPtr -> Eff es BddPtr
bddAnd a b = ask >>= \m -> liftIO $ RSDD.bddAnd m a b

bddOr :: (IOE :> es) => (Reader BddBuilder :> es) => BddPtr -> BddPtr -> Eff es BddPtr
bddOr a b = ask >>= \m -> liftIO $ RSDD.bddOr m a b

bddNeg :: (IOE :> es) => (Reader BddBuilder :> es) => BddPtr -> Eff es BddPtr
bddNeg a = ask >>= \m -> liftIO $ RSDD.bddNeg m a

setWeights :: (IOE :> es) => WmcParams -> VarLabel -> RSDD.Weight -> Eff es ()
setWeights a b c = liftIO $ RSDD.setWeight a b c

setHigh1 :: (IOE :> es) => RSDD.WmcParams -> VarLabel -> Double -> Eff es ()
setHigh1 w l h = liftIO $ RSDD.setHigh wmc l h
  where
    wmc :: RSDD.WmcParamsT 1
    wmc = RSDD.mkTypedWmcParams w

setLow1 :: (IOE :> es) => RSDD.WmcParams -> VarLabel -> Double -> Eff es ()
setLow1 w l h = liftIO $ RSDD.setLow wmc l h
  where
    wmc :: RSDD.WmcParamsT 1
    wmc = RSDD.mkTypedWmcParams w

bddWmc' = flip RSDD.bddWmc
