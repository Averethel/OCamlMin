module Rename (rename) where
  import Syntax.Expr
  import Syntax.Pattern

  isBound :: String -> Pattern -> Bool
  isBound n (Pvar x)      = x == n
  isBound n (Ppair p1 p2) = isBound n p1 || isBound n p2
  isBound n (Pcons p1 p2) = isBound n p1 || isBound n p2
  isBound _ _             = False

  renameFunClause :: String -> String -> FunClause -> FunClause
  renameFunClause n1 n2 fc
    | any (isBound n1) $ arguments fc = fc
    | otherwise                       = fc { fbody = rename n1 n2 $ fbody fc }

  renameCaseClause :: String -> String -> CaseClause -> CaseClause
  renameCaseClause n1 n2 cc
    | n1 `elem` variables cc = cc
    | otherwise              = cc { cbody = rename n1 n2 $ cbody cc }

  rename :: String -> String -> Expr -> Expr
  rename n1 n2 (Evar n)
    | n == n1                   = Evar n2
    | otherwise                 = Evar n
  rename n1 n2 (Efun fcs)       =
    Efun $ map (renameFunClause n1 n2) fcs
  rename n1 n2 (Elet p e1 e2)
    | isBound n1 p              = Elet p e1 e2
    | otherwise                 = Elet p (rename n1 n2 e1) $ rename n1 n2 e2
  rename n1 n2 (Eletrec s fc e)
    | s == n1                   = Eletrec s fc e
    | otherwise                 = Eletrec s (map (renameFunClause n1 n2) fc) $
                                          rename n1 n2 e
  rename n1 n2 (Eapply e1 as)   = Eapply (rename n1 n2 e1) $
                                         map (rename n1 n2) as
  rename n1 n2 (Epair e1 e2)    = Epair (rename n1 n2 e1) $ rename n1 n2 e2
  rename n1 n2 (Econs e1 e2)    = Econs (rename n1 n2 e1) $ rename n1 n2 e2
  rename n1 n2 (Eif e1 e2 e3)   = Eif (rename n1 n2 e1) (rename n1 n2 e2) $
                                      rename n1 n2 e3
  rename n1 n2 (Eseq e1 e2)     = Eseq (rename n1 n2 e1) $ rename n1 n2 e2
  rename n1 n2 (Ecase e ccs)    = Ecase (rename n1 n2 e) $
                                        map (renameCaseClause n1 n2) ccs
  rename n1 n2 (Ehandle e1 e2)  = Ehandle (rename n1 n2 e1) $ rename n1 n2 e2
  rename _  _  e                = e