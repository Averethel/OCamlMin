{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.Pattern (typeAndBindingsOfPattern) where
  import Syntax.Pattern
  import Types
  import Utils.Errors

  import TypeInference.Constant
  import TypeInference.Constraints
  import TypeInference.Counter
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
  getids (Ptuple ps)   = concatMap getids ps
  getids (Pcons p1 p2) = getids p1 ++ getids p2

  typeAndBindingsOfPattern :: (MonadError String m,  MonadState Counter m) =>
                              Pattern -> m (Type, Env, Constraints)
  typeAndBindingsOfPattern p
    | idsDistinct $ getids p = typeAndBindingsOfPattern' p
    | otherwise              = throwError $ overlappingIds p
    where
      typeAndBindingsOfPattern' :: (MonadError String m,
                                    MonadState Counter m) =>
                                    Pattern -> m (Type, Env, Constraints)
      typeAndBindingsOfPattern' Pwildcard     = do
        v <- freshVar
        return (v, emptyEnv, emptyConstraints)
      typeAndBindingsOfPattern' (Pvar n)      = do
        v <- freshVar
        return (v, emptyEnv `extend` (n, v), emptyConstraints)
      typeAndBindingsOfPattern' (Pconst c)    = do
        t <- typeOfConstant c
        return (t, emptyEnv, emptyConstraints)
      typeAndBindingsOfPattern' (Ptuple ps)   = do
        tbcs <- mapM typeAndBindingsOfPattern ps
        return (Ttuple $ map (\(a, _, _) -> a) tbcs,
                concatMap (\(_, b, _) -> b) tbcs,
                concatMap (\(_, _, c) -> c) tbcs)
      typeAndBindingsOfPattern' (Pcons p1 p2) = do
        (t1, b1, c1) <- typeAndBindingsOfPattern p1
        (t2, b2, c2) <- typeAndBindingsOfPattern p2
        return (t2, b1 ++ b2,
                singleConstraint t2 (Tlist t1) `addConstraints`
                c1 `addConstraints` c2 )
