{-# LANGUAGE
  FlexibleContexts
  #-}

module PatternMatching.ToCases (handlesToCases) where
  import Syntax
  import Rename

  import PatternMatching.Counters

  import Control.Exception.Base
  import Control.Monad.State

  type Equation = ([Pattern], Expr)

  isVar :: Equation -> Bool
  isVar (Pvar _:_, _)    = True
  isVar _                = False

  getCon :: Equation -> Constructor
  getCon (Pconst Cnil:_, _)          = CNnil
  getCon (Pcons _ _:_, _)            = CNcons
  getCon (Ppair _ _:_, _)            = CNpair
  getCon (Pconst (Cbool True):_, _)  = CNtrue
  getCon (Pconst (Cbool False):_, _) = CNfalse
  getCon (Pconst Cunit:_, _)         = CNunit
  getCon e                           = assert False $ getCon e

  subpaterns :: Pattern -> [Pattern]
  subpaterns (Pcons p1 p2) = [p1, p2]
  subpaterns (Ppair p1 p2) = [p1, p2]
  subpaterns _             = []

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

  getVars :: [Pattern] -> [String]
  getVars = map (\(Pvar x) -> x)

  matchVar :: MonadState Counter m =>
              [String] -> [([Pattern], Expr)] -> Expr -> m Expr
  matchVar (u:us) qs =
    match us [(ps, rename v u e) | (Pvar v : ps, e) <- qs]
  matchVar us     qs = assert False $ matchVar us qs

  choose :: Constructor -> [Equation] -> [Equation]
  choose c qs = [q | q <- qs, getCon q == c]

  matchClause :: MonadState Counter m =>
                 Constructor -> [String] -> [Equation] -> Expr -> m CaseClause
  matchClause c (_:us) qs def = do
    let k  = arity c
    us' <- mapM (\_ -> freshVar) [1..k]
    e'  <- match (us' ++ us) [(subpaterns p ++ ps, e) | (p : ps, e) <- qs] def
    return CC { constructor = c, variables = us', cbody = e' }
  matchClause c us     qs def = assert False $ matchClause c us qs def

  matchCon :: MonadState Counter m => [String] -> [Equation] -> Expr -> m Expr
  matchCon (u:us) qs def = do
    let cs = constructors $ getCon $ head qs
    ms' <- mapM (\c -> matchClause c (u:us) (choose c qs) def) cs
    return $ Ecase (Evar u) ms'
  matchCon us     qs def = assert False $ matchCon us qs def

  matchVarCon :: MonadState Counter m =>
                 [String] -> [Equation] -> Expr -> m Expr
  matchVarCon us qs def
    | isVar . head $ qs =
      matchVar us qs def
    | otherwise         =
      matchCon us qs def

  match :: MonadState Counter m => [String] -> [Equation] -> Expr -> m Expr
  match []     qs def =
    return $ foldr Ehandle def [e | ([], e) <- qs ]
  match (u:us) qs def =
    foldrM (matchVarCon (u:us)) def $ partition isVar qs

  decompose :: Expr -> [Equation]
  decompose (Ehandle (Eapply (Efun [fc]) _) e2) =
    (arguments fc, fbody fc) : decompose e2
  decompose _                                   = []

  handlesToCasesCaseClause :: MonadState Counter m => CaseClause -> m CaseClause
  handlesToCasesCaseClause cc = do
    b' <- handlesToCases $ cbody cc
    return cc{ cbody = b' }

  handlesToCasesFunClauses :: MonadState Counter m => [FunClause] -> m FunClause
  handlesToCasesFunClauses fcs = do
    let eqs = concatMap (decompose . fbody) fcs
    let ags = getVars . arguments . head $ fcs
    cs' <- match ags eqs EmatchFailure
    return FC { arguments = map Pvar ags, fbody = cs' }

  handlesToCases :: MonadState Counter m => Expr -> m Expr
  handlesToCases (Efun fcs)        = do
    cs <- handlesToCasesFunClauses fcs
    return $ Efun [cs]
  handlesToCases (Elet p e1 e2)     = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ Elet p e1' e2'
  handlesToCases (Eletrec n fcs e) = do
    cs <- handlesToCasesFunClauses fcs
    e' <- handlesToCases e
    return $ Eletrec n [cs] e'
  handlesToCases (Eapply e1 as)     = do
    e1' <- handlesToCases e1
    as' <- mapM handlesToCases as
    return $ Eapply e1' as'
  handlesToCases (Epair e1 e2)      = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ Epair e1' e2'
  handlesToCases (Econs e1 e2)      = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ Econs e1' e2'
  handlesToCases (Eif e1 e2 e3)     = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    e3' <- handlesToCases e3
    return $ Eif e1' e2' e3'
  handlesToCases (Eseq e1 e2)       = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ Eseq e1' e2'
  handlesToCases (Ecase e1 ccs)     = do
    e1'  <- handlesToCases e1
    ccs' <- mapM handlesToCasesCaseClause ccs
    return $ Ecase e1' ccs'
  handlesToCases (Ehandle e1 e2)    = do
    e1' <- handlesToCases e1
    e2' <- handlesToCases e2
    return $ Ehandle e1' e2'
  handlesToCases e                  =
    return e
