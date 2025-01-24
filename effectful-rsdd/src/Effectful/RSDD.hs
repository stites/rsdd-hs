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
    bddEq,
    RSDD.isTrue,
    RSDD.isFalse,
    RSDD.isConst,
    RSDD.ptrTrue,
    RSDD.ptrFalse,
    RSDD.topvar,
    RSDD.low,
    RSDD.high,
    RSDD.diceInf,
    RSDD.diceInfMany,
    RSDD.bddWmc,
    RSDD.mkTypedWmcParams,
    runWmc,
    evalWmc,
    setWeights,
    setHigh1,
    setLow1,
    RSDD.varWeight,
    RSDD.prettyBddIO,
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

bddEq :: (IOE :> es) => (Reader BddBuilder :> es) => BddPtr -> BddPtr -> Eff es Bool
bddEq a b = ask >>= \m -> liftIO $ RSDD.bddEq m a b

evalWmc :: (IOE :> es) => Eff (Reader WmcParams : es) a -> Eff es (WmcParams, a)
evalWmc act = do
  wmc <- RSDD.wmcParamsIO
  a <- runReader wmc act
  pure (wmc, a)

runWmc :: (IOE :> es) => Eff (Reader WmcParams : es) a -> Eff es a
runWmc act = do
  wmc <- RSDD.wmcParamsIO
  runReader wmc act

varWeight :: (IOE :> es) => (Reader WmcParams :> es) => VarLabel -> Eff es (Maybe RSDD.Weight)
varWeight a = flip RSDD.varWeight a <$> ask

setWeights :: (IOE :> es) => (Reader WmcParams :> es) => VarLabel -> RSDD.Weight -> Eff es ()
setWeights a b = ask >>= \ps -> liftIO $ RSDD.setWeight ps a b

setHigh1 :: (IOE :> es) => (Reader WmcParams :> es) => VarLabel -> Double -> Eff es ()
setHigh1 l h = RSDD.mkTypedWmcParams @1 <$> ask >>= \wmc -> liftIO $ RSDD.setHigh wmc l h

setLow1 :: (IOE :> es) => (Reader WmcParams :> es) => VarLabel -> Double -> Eff es ()
setLow1 l h = RSDD.mkTypedWmcParams @1 <$> ask >>= \wmc -> liftIO $ RSDD.setLow wmc l h
