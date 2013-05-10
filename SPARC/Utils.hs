{-# LANGUAGE
  FlexibleContexts
  #-}

module SPARC.Utils where
  import Counters
  import SPARC.Syntax
  import Types

  import Control.Monad.State
  import Data.Set

  concat :: Seq -> String  -> Type -> Seq -> Seq
  concat (Ans e1)       x tx e2 = Let x tx e1 e2
  concat (Let y t e e1) x tx e2 = Let y t e $ SPARC.Utils.concat e1 x tx e2
  concat (Labeled l e1) x tx e2 = Labeled l $ SPARC.Utils.concat e1 x tx e2

  instrSeq :: MonadState Counter m => Instr -> Seq -> m Seq
  instrSeq i e = do
    idf <- freshName $ genId Tunit
    return $ Let idf Tunit i e

  buildSeq :: MonadState Counter m => Seq -> Seq -> m Seq
  buildSeq (Ans i) e2         = instrSeq i e2
  buildSeq (Let x t i e1) e2  = do
    e1' <- buildSeq e1 e2
    return $ Let x t i e1'
  buildSeq (Labeled l e1) e2  = do
    e1' <- buildSeq e1 e2
    return $ Labeled l e1'

  align :: Integer -> Integer
  align i
    | i `mod` 8 == 0 = i
    | otherwise    = i + 4

  removeAndUniq :: Set String -> [String] -> [String]
  removeAndUniq _ []     = []
  removeAndUniq s (x:xs)
    | x `member` s       = removeAndUniq s xs
    | otherwise          = x : removeAndUniq (x `insert` s) xs

  fvIdOrImm :: IdOrIimm -> [String]
  fvIdOrImm (V x) = [x]
  fvIdOrImm _     = []

  fvInstr :: Instr -> [String]
  fvInstr (Imov x)            = [x]
  fvInstr (Ineg x)            = [x]
  fvInstr (IfMovD x)          = [x]
  fvInstr (IfNegD x)          = [x]
  fvInstr (Isave x _)         = [x]
  fvInstr (Iadd x y')         = x : fvIdOrImm y'
  fvInstr (Isub x y')         = x : fvIdOrImm y'
  fvInstr (ISLL x y')         = x : fvIdOrImm y'
  fvInstr (Ild x y')          = x : fvIdOrImm y'
  fvInstr (IldDF x y')        = x : fvIdOrImm y'
  fvInstr (Ist x y z')        = x : y : fvIdOrImm z'
  fvInstr (IstDF x y z')      = x : y : fvIdOrImm z'
  fvInstr (IfAddD x y)        = [x, y]
  fvInstr (IfSubD x y)        = [x, y]
  fvInstr (IfMulD x y)        = [x, y]
  fvInstr (IfDivD x y)        = [x, y]
  fvInstr (IifEq x y' e1 e2)  = x : fvIdOrImm y' ++ removeAndUniq empty (fvSeq e1 ++ fvSeq e2)
  fvInstr (IifLE x y' e1 e2)  = x : fvIdOrImm y' ++ removeAndUniq empty (fvSeq e1 ++ fvSeq e2)
  fvInstr (IifGE x y' e1 e2)  = x : fvIdOrImm y' ++ removeAndUniq empty (fvSeq e1 ++ fvSeq e2)
  fvInstr (IifFEq x y e1 e2)  = x : y : removeAndUniq empty (fvSeq e1 ++ fvSeq e2)
  fvInstr (IifFLE x y e1 e2)  = x : y : removeAndUniq empty (fvSeq e1 ++ fvSeq e2)
  fvInstr (IcallCls x ys zs)  = x : ys ++ zs
  fvInstr (IcallDir _ ys zs)  = ys ++ zs
  fvInstr _                   = []

  fvSeq :: Seq -> [String]
  fvSeq (Ans i)       = fvInstr i
  fvSeq (Let x _ i e) = fvInstr i ++ removeAndUniq (singleton x) (fvSeq e)
  fvSeq (Labeled _ e) = fvSeq e

  freeVars :: Seq -> [String]
  freeVars sq = removeAndUniq empty $ fvSeq sq

  isReg :: String -> Bool
  isReg ('%':_) = True
  isReg _       = False
