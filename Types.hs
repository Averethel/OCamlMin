module Types (
  Type(..),
  genId,
  Subst,
  emptySubst,
  composeSubst,
  singleSubst,
  applySubst,
  pprType
) where
  import Types.Base
  import Types.Subst
