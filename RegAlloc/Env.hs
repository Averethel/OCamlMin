module RegAlloc.Env where
  import SPARC.Syntax
  import SPARC.Utils
  import Types

  type Env = [(String, String)]

  envFind :: String -> Type -> Env -> Either (String, Type) String
  envFind x t regenv
    | isReg x   = Right x
    | otherwise =
      case x `lookup` regenv of
        Nothing -> Left (x, t)
        Just r  -> Right r

  envFind' :: IdOrIimm -> Env -> Either (String, Type) IdOrIimm
  envFind' (V x) regenv =
    case envFind x Tint regenv of
      Left err -> Left err
      Right s  -> Right $ V s
  envFind' c     _      = Right c