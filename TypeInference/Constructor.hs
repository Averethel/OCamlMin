{-# LANGUAGE
  FlexibleContexts
  #-}

module TypeInference.Constructor where
  import Counters
  import Syntax.Constructor
  import TypedSyntax.Constructor
  import Types

  import TypeInference.Env

  import Control.Exception.Base
  import Control.Monad.Error
  import Control.Monad.State

  typeAndBindingsOfConstructor :: (MonadState Counter m, MonadError String m) =>
                                  Constructor -> [String] ->
                                  m (TypedConstructor, Env)
  typeAndBindingsOfConstructor CNnil   []     = do
    v <- freshTypeVar
    return ((CNnil, Tlist v), [])
  typeAndBindingsOfConstructor CNcons  [h, t] = do
    v <- freshTypeVar
    return ((CNcons, Tfun [v, Tlist v] $ Tlist v), [(h, v), (t, Tlist v)])
  typeAndBindingsOfConstructor CNpair  [x, y] = do
    v1 <- freshTypeVar
    v2 <- freshTypeVar
    return ((CNpair, Tfun [v1, v2] $ Tpair v1 v2), [(x, v1), (y, v2)])
  typeAndBindingsOfConstructor CNtrue  []     =
    return ((CNtrue, Tbool), [])
  typeAndBindingsOfConstructor CNfalse []     =
    return ((CNfalse, Tbool), [])
  typeAndBindingsOfConstructor CNunit  []     =
    return ((CNunit, Tunit), [])
  typeAndBindingsOfConstructor c       as     =
    assert (arity c == length as) typeAndBindingsOfConstructor c as
