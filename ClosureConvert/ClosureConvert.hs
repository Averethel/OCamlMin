module ClosureConvert.ClosureConvert where
  import ClosureConvert.CSyntax

  import KNormal.KSyntax

  import Data.Set hiding (map)

  closureConvert :: [Label] -> KExpr -> IO Program
  closureConvert _    (KEunit t)                =
    return $ P [] $ CEunit t
  closureConvert _    (KEnil t)                 =
    return $ P [] $ CEnil t
  closureConvert _    (KEint n t)               =
    return $ P [] $ CEint n t
  closureConvert _    (KEneg x t)               =
    return $ P [] $ CEneg x t
  closureConvert _    (KEload r t)              =
    return $ P [] $ CEload r t
  closureConvert _    (KEadd x y t)             =
    return $ P [] $ CEadd x y t
  closureConvert _    (KEsub x y t)             =
    return $ P [] $ CEsub x y t
  closureConvert _    (KEmult x y t)            =
    return $ P [] $ CEmult x y t
  closureConvert _    (KEdiv x y t)             =
    return $ P [] $ CEdiv x y t
  closureConvert _    (KEmod x y t)             =
    return $ P [] $ CEmod x y t
  closureConvert _    (KEstore r x t)           =
    return $ P [] $ CEstore r x t
  closureConvert _    (KEvar x t)               =
    return $ P [] $ CEvar x t
  closureConvert _    (KEerror m t)             =
    return $ P [] $ CEerror m t
  closureConvert known (KEifEq x y e1 e2 t)     = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2)
           $ CEifEq x y (main p1) (main p2) t
  closureConvert known (KEifLE x y e1 e2 t)     = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2)
           $ CEifLE x y (main p1) (main p2) t
  closureConvert known (KElet (x, tx) e1 e2 t)  = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2)
           $ CElet x tx (main p1) (main p2) t
  closureConvert known (KEletRec fd e t)        = do
    let (x, tx) = KNormal.KSyntax.name fd
    let as      = KNormal.KSyntax.args fd
    let e1      = KNormal.KSyntax.body fd
    let known'  = L x : known
    p1 <- closureConvert known' e1
    let zs = ClosureConvert.CSyntax.freeVars (main p1) \\ fromList as
    (p1', known'') <-
      if zs /= empty
      then do
        putStrLn $ "Free variable(s) " ++ show (toList zs) ++
                   " found in function " ++ x ++ "."
        putStrLn $ "Function " ++ x ++ " cannot be applied directly."
        p <- closureConvert known e1
        return (p, known)
      else
        return (p1, known')
    p2 <- closureConvert known'' e
    let fvs = toList $ ClosureConvert.CSyntax.freeVars (main p1') \\
                       singleton (x, tx) `union` fromList as
    let df = ClosureConvert.CSyntax.FD { ClosureConvert.CSyntax.name = (L x, tx),
             ClosureConvert.CSyntax.args = as,
                  formalFvs = fvs, ClosureConvert.CSyntax.body = main p1' }
    let dfs = df : (definitions p1' ++ definitions p2)
    if (x, tx) `member` ClosureConvert.CSyntax.freeVars (main p2)
    then
      return $ P dfs $ CEmakeClj x C { entry = (L x, t), actFvs = fvs } (main p2) t
    else do
      putStrLn $ "Eliminating closure " ++ x
      return $ P dfs $ main p2
  closureConvert known (KEapply s ss t)
    | L (fst s) `elem` known                    = do
      putStrLn $ "Directly applying " ++ fst s
      return $ P [] $ CEappDir (L $ fst s, snd s) ss t
    | otherwise                                 =
      return $ P [] $ CEappClj s ss t
  closureConvert _     (KEpair a b t)           =
    return $ P [] $ CEpair a b t
  closureConvert _     (KEcons h t tp)          =
    return $ P [] $ CEcons h t tp
  closureConvert known (KEletPair a b p e t)    = do
    pr <- closureConvert known e
    return $ P (definitions pr) $ CEletPair a b p (main pr) t
  closureConvert known (KEletList h t l e tp)   = do
    p <- closureConvert known e
    return $ P (definitions p) $ CEletList h t l (main p) tp
  closureConvert known (KEhandle e1 e2 t)       = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2) $ CEhandle (main p1) (main p2) t
  closureConvert known (KEseq e1 e2 t)          = do
    p1 <- closureConvert known e1
    p2 <- closureConvert known e2
    return $ P (definitions p1 ++ definitions p2) $ CEseq (main p1) (main p2) t
  closureConvert _    (KEextFunApp s ss t)      =
    return $ P [] $ CEappDir (L . fst $ s, snd s) ss t
