{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.ToCases (handlesToCases) where
  import CompilerState
  import Syntax
  import TypedSyntax
  import Types
  import Rename

  import Control.Exception.Base
  import Control.Monad.State

  type Equation = ([TypedPattern], TypedExpr)

  isVar :: Equation -> Bool
  isVar (TPvar _ _:_, _) = True
  isVar _                = False

  getCon :: Equation -> TypedConstructor
  getCon (TPconst (Cnil, t):_, _)        = (CNnil, t)
  getCon (TPcons p1 p2 t:_, _)           = (CNcons, Tfun [typeOfTypedPattern p1,
                                                       typeOfTypedPattern p2] t)
  getCon (TPpair p1 p2 t:_, _)           = (CNpair, Tfun [typeOfTypedPattern p1,
                                                       typeOfTypedPattern p2] t)
  getCon (TPconst (Cbool True, t):_, _)  = (CNtrue, t)
  getCon (TPconst (Cbool False, t):_, _) = (CNfalse, t)
  getCon (TPconst (Cunit, t):_, _)       = (CNunit, t)
  getCon e                               = assert False $ getCon e

  subpaterns :: TypedPattern -> [TypedPattern]
  subpaterns (TPcons p1 p2 _) = [p1, p2]
  subpaterns (TPpair p1 p2 _) = [p1, p2]
  subpaterns _                = []

  partition :: (a -> Bool) -> [a] -> [[a]]
  partition _ []       = []
  partition _ [x]      = [[x]]
  partition f (x:y:xs)
    | f x == f y       =
      tack x $ partition f (y:xs)
    | otherwise        =
      [x] : partition f (y:xs)

  tack :: a -> [[a]] -> [[a]]
  tack x xss = (x : head xss) : tail xss

  foldrM :: Monad m => (b -> a -> m a) -> a -> [b] -> m a
  foldrM _ a []     = return a
  foldrM f a (x:xs) = do
    acc' <- foldrM f a xs
    f x acc'

  getVars :: [TypedPattern] -> [(String, Type)]
  getVars = map (\(TPvar x t) -> (x, t))

  matchVar :: MonadState CompilerState m =>
              [(String, Type)] -> [Equation] -> TypedExpr -> m TypedExpr
  matchVar ((u, _):us) qs =
    match us [(ps, rename v u e) | (TPvar v _: ps, e) <- qs]
  matchVar us     qs = assert False $ matchVar us qs

  choose :: TypedConstructor -> [Equation] -> [Equation]
  choose c qs = [q | q <- qs, fst (getCon q) == fst c]

  matchClause :: MonadState CompilerState m =>
                 TypedConstructor -> [(String, Type)] -> [Equation] ->
                 TypedExpr -> m TypedCaseClause
  matchClause c (_:us) qs def = do
    let types = case snd c of { Tfun ts _ -> ts; _ -> [] }
    us' <- mapM freshPMVar types
    e'  <- match (zip us' types ++ us)
          [(subpaterns p ++ ps, e) | (p : ps, e) <- qs] def
    return TCC { tccConstructor = c, tccVariables = zip us' types, tccBody = e' }
  matchClause c us     qs def = assert False $ matchClause c us qs def

  matchCon :: MonadState CompilerState m => [(String, Type)] -> [Equation] ->
              TypedExpr -> m TypedExpr
  matchCon ((u, t):us) qs def = do
    let cs = constructors . getCon . head $ qs
    ms' <- mapM (\c -> matchClause c ((u, t):us) (choose c qs) def) cs
    let Tfun [_] r = typeOfTypedCaseClause . head $ ms'
    return $ TEcase (TEvar u t) ms' r
  matchCon us     qs def = assert False $ matchCon us qs def

  matchVarCon :: MonadState CompilerState m =>
                 [(String, Type)] -> [Equation] -> TypedExpr -> m TypedExpr
  matchVarCon us qs def
    | isVar . head $ qs =
      matchVar us qs def
    | otherwise         =
      matchCon us qs def

  match :: MonadState CompilerState m => [(String, Type)] -> [Equation] ->
           TypedExpr -> m TypedExpr
  match []     qs def =
    return $ foldr (\a b -> TEhandle a b $ typeOfTypedExpr def) def
            [e | ([], e) <- qs ]
  match (u:us) qs def =
    foldrM (matchVarCon (u:us)) def $ partition isVar qs

  decompose :: TypedExpr -> [Equation]
  decompose (TEhandle (TEapply (TEfun [fc] _) _ _) e2 _) =
    (tfcArguments fc, tfcBody fc) : decompose e2
  decompose _                                   = []

  handlesToCasesCaseClause :: MonadState CompilerState m => TypedCaseClause ->
                              m TypedCaseClause
  handlesToCasesCaseClause cc = do
    b' <- handlesToCases $ tccBody cc
    return cc{ tccBody = b' }

  handlesToCasesFunClauses :: MonadState CompilerState m => [TypedFunClause] ->
                              m TypedFunClause
  handlesToCasesFunClauses fcs = do
    let eqs = concatMap (decompose . tfcBody) fcs
    let ags = getVars . tfcArguments . head $ fcs
    cs' <- match ags eqs $ TEmatchFailure $ typeOfTypedFunClause . head $ fcs
    return TFC { tfcArguments = map (uncurry TPvar) ags, tfcBody = cs' }

  handlesToCases :: MonadState CompilerState m => TypedExpr -> m TypedExpr
  handlesToCases (TEfun fcs t)            = do
    cs <- handlesToCasesFunClauses fcs
    return $ TEfun [cs] t
  handlesToCases (TElet p e1 e2 t)        = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ TElet p e1' e2' t
  handlesToCases (TEletrec n t1 fcs e t2) = do
    cs <- handlesToCasesFunClauses fcs
    e' <- handlesToCases e
    return $ TEletrec n t1 [cs] e' t2
  handlesToCases (TEapply e1 as t)        = do
    e1' <- handlesToCases e1
    as' <- mapM handlesToCases as
    return $ TEapply e1' as' t
  handlesToCases (TEpair e1 e2 t)         = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ TEpair e1' e2' t
  handlesToCases (TEcons e1 e2 t)         = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ TEcons e1' e2' t
  handlesToCases (TEif e1 e2 e3 t)        = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    e3' <- handlesToCases e3
    return $ TEif e1' e2' e3' t
  handlesToCases (TEseq e1 e2 t)          = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ TEseq e1' e2' t
  handlesToCases (TEcase e1 ccs t)        = do
    e1'  <- handlesToCases e1
    ccs' <- mapM handlesToCasesCaseClause ccs
    return $ TEcase e1' ccs' t
  handlesToCases (TEhandle e1 e2 t)       = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ TEhandle e1' e2' t
  handlesToCases e                        =
    return e
