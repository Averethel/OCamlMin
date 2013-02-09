{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.Unification (unify) where
  import Types
  import Utils.Errors

  import TypeInference.Constraints

  import Control.Exception.Base
  import Control.Monad.Error

  canUnify :: Type -> Type -> Bool
  canUnify (Tvar _)      _             = True
  canUnify _             (Tvar _)      = True
  canUnify (Tlist t1)    (Tlist t2)    = canUnify t1 t2
  canUnify (Tref t1)     (Tref t2)     = canUnify t1 t2
  canUnify (Ttuple ts1)  (Ttuple ts2)  =
    length ts1 == length ts2 &&
    and (zipWith canUnify ts1 ts2)
  canUnify (Tfun as1 t1) (Tfun as2 t2) =
    length as1 == length as2 &&
    and (zipWith canUnify as1 as2) &&
    canUnify t1 t2
  canUnify t1            t2            = t1 == t2

  newConstraints :: Type -> Type -> Constraints
  newConstraints (Tlist t1)    (Tlist t2)    = [(t1, t2)]
  newConstraints (Tref t1)     (Tref t2)     = [(t1, t2)]
  newConstraints (Ttuple ts1)  (Ttuple ts2)  = zip ts1 ts2
  newConstraints (Tfun as1 t1) (Tfun as2 t2) = (t1, t2) : zip as1 as2
  newConstraints t1            t2            = assert (t1 == t2) []

  unify :: MonadError String m => Constraints -> m Subst
  unify cns = unify' cns emptySubst where
    unify' :: MonadError String m => Constraints -> Subst -> m Subst
    unify' []          s = return s
    unify' ((a, b):cs) s = do
      let a' = a `applySubst` s
      let b' = b `applySubst` s
      case (a', b') of
        (Tvar i, _)      -> unify' cs $ composeSubst (singleSubst i b') s
        (_,      Tvar i) -> unify' cs $ composeSubst (singleSubst i a') s
        (_,      _)      ->
          if canUnify a' b'
          then unify' (newConstraints a' b' `addConstraints` cs) s
          else throwError $ cannotUnify a' b'
