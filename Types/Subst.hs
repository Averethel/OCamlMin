module Types.Subst (
  Subst,
  emptySubst,
  composeSubst,
  singleSubst,
  applySubst
) where
  import Types.Base

  type Subst = [(String, Type)]

  emptySubst :: Subst
  emptySubst = []

  composeSubst :: Subst -> Subst -> Subst
  composeSubst = flip (++)

  singleSubst :: String -> Type -> Subst
  singleSubst i t = [(i, t)]

  applySubst :: Type -> Subst -> Type
  applySubst = foldl $ flip applySingleSubst

  applySingleSubst :: (String, Type) -> Type -> Type
  applySingleSubst (sb, tb) (Tvar v)
    | sb == v                             = tb
    | otherwise                           = Tvar v
  applySingleSubst sb       (Tlist tp)    = Tlist $ applySingleSubst sb tp
  applySingleSubst sb       (Tref tp)     = Tref $ applySingleSubst sb tp
  applySingleSubst sb       (Tpair t1 t2) = Tpair (applySingleSubst sb t1) $
                                                   applySingleSubst sb t2
  applySingleSubst sb       (Tfun as tp)  =
    Tfun (map (applySingleSubst sb) as) $ applySingleSubst sb tp
  applySingleSubst _        tp            = tp
