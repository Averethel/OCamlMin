module RegAlloc.Env where
  import SPARC.Syntax
  import SPARC.Utils
  import Types

  import Control.Monad
  import Control.Monad.Exception.Synchronous

  type Env = [(String, String)]

  envFind :: Monad m => String -> Type -> Env ->
              ExceptionalT (String, Type) m String
  envFind x t regenv
    | isReg x   = return x
    | otherwise =
      case x `lookup` regenv of
        Nothing -> throwT (x, t)
        Just y  -> return y

  envFind' :: Monad m => IdOrIimm -> Env ->
                ExceptionalT (String, Type) m IdOrIimm
  envFind' (V x) regenv = liftM V $ envFind x Tint regenv
  envFind' c     _      = return c
