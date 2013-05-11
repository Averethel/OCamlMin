{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.Pattern (typeAndBindingsOfPattern) where
  import CompilerState
  import Syntax.Pattern
  import TypedSyntax.Pattern
  import Types
  import Utils.Errors

  import TypeInference.Constant
  import TypeInference.Constraints
  import TypeInference.Env

  import Control.Monad.Error
  import Control.Monad.State

  idsDistinct :: [String] -> Bool
  idsDistinct []     = True
  idsDistinct (x:xs) = idsDistinct' x xs xs where
    idsDistinct' _  []     []     = True
    idsDistinct' _  []     (y:ys) = idsDistinct' y ys ys
    idsDistinct' x' (z:zs) ys
      | x' /= z                   = idsDistinct' x' zs ys
      | otherwise                 = False

  getids :: Pattern -> [String]
  getids Pwildcard     = []
  getids (Pvar n)      = [n]
  getids (Pconst _)    = []
  getids (Ppair p1 p2) = getids p1 ++ getids p2
  getids (Pcons p1 p2) = getids p1 ++ getids p2

  typeAndBindingsOfPattern :: (MonadError String m,  MonadState CompilerState m) =>
                              Pattern -> m (TypedPattern, Env, Constraints)
  typeAndBindingsOfPattern p
    | idsDistinct $ getids p = typeAndBindingsOfPattern' p
    | otherwise              = throwError $ overlappingIds p
    where
      typeAndBindingsOfPattern' :: (MonadError String m,
                                    MonadState CompilerState m) =>
                                    Pattern ->
                                    m (TypedPattern, Env, Constraints)
      typeAndBindingsOfPattern' Pwildcard     = do
        v <- freshTypeVar
        return (TPwildcard v, emptyEnv, emptyConstraints)
      typeAndBindingsOfPattern' (Pvar n)      = do
        v <- freshTypeVar
        return (TPvar n v, emptyEnv `extend` (n, v), emptyConstraints)
      typeAndBindingsOfPattern' (Pconst c)    = do
        tc <- typeOfConstant c
        return (TPconst (c, tc), emptyEnv, emptyConstraints)
      typeAndBindingsOfPattern' (Ppair p1 p2) = do
        (tp1, e1, c1) <- typeAndBindingsOfPattern p1
        (tp2, e2, c2) <- typeAndBindingsOfPattern p2
        let tp = Tpair (typeOfTypedPattern tp1) $ typeOfTypedPattern tp2
        return (TPpair tp1 tp2 tp, e1 ++ e2, c1 ++ c2)
      typeAndBindingsOfPattern' (Pcons p1 p2) = do
        (tp1, b1, c1) <- typeAndBindingsOfPattern p1
        (tp2, b2, c2) <- typeAndBindingsOfPattern p2
        return (TPcons tp1 tp2 $ typeOfTypedPattern tp2, b1 ++ b2,
                singleConstraint (typeOfTypedPattern tp2)
                (Tlist $ typeOfTypedPattern tp1) `addConstraints`
                c1 `addConstraints` c2 )
