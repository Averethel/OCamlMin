module Rename (rename) where
  import TypedSyntax.Expr
  import TypedSyntax.Pattern

  isBound :: String -> TypedPattern -> Bool
  isBound n (TPvar x _)      = x == n
  isBound n (TPpair p1 p2 _) = isBound n p1 || isBound n p2
  isBound n (TPcons p1 p2 _) = isBound n p1 || isBound n p2
  isBound _ _                = False

  renameFunClause :: String -> String -> TypedFunClause -> TypedFunClause
  renameFunClause n1 n2 fc
    | any (isBound n1) $ tfcArguments fc = fc
    | otherwise                          = fc { tfcBody = rename n1 n2 $
                                                          tfcBody fc }

  renameCaseClause :: String -> String -> TypedCaseClause -> TypedCaseClause
  renameCaseClause n1 n2 cc
    | n1 `elem` map fst (tccVariables cc) = cc
    | otherwise                           = cc { tccBody = rename n1 n2 $
                                                            tccBody cc }

  rename :: String -> String -> TypedExpr -> TypedExpr
  rename n1 n2 (TEvar n t)
    | n == n1                           = TEvar n2 t
    | otherwise                         = TEvar n t
  rename n1 n2 (TEfun fcs t)            =
    TEfun (map (renameFunClause n1 n2) fcs) t
  rename n1 n2 (TElet p e1 e2 t)
    | isBound n1 p                      = TElet p e1 e2 t
    | otherwise                         = TElet p (rename n1 n2 e1)
                                                  (rename n1 n2 e2) t
  rename n1 n2 (TEletrec s t1 fc e t2)
    | s == n1                           = TEletrec s t1 fc e t2
    | otherwise                         = TEletrec s t1
                                            (map (renameFunClause n1 n2) fc)
                                            (rename n1 n2 e) t2
  rename n1 n2 (TEapply e1 as t)        = TEapply (rename n1 n2 e1)
                                              (map (rename n1 n2) as) t
  rename n1 n2 (TEpair e1 e2 t)         = TEpair (rename n1 n2 e1)
                                                 (rename n1 n2 e2) t
  rename n1 n2 (TEcons e1 e2 t)         = TEcons (rename n1 n2 e1)
                                                 (rename n1 n2 e2) t
  rename n1 n2 (TEif e1 e2 e3 t)        = TEif (rename n1 n2 e1)
                                               (rename n1 n2 e2)
                                               (rename n1 n2 e3) t
  rename n1 n2 (TEseq e1 e2 t)          = TEseq (rename n1 n2 e1)
                                                (rename n1 n2 e2) t
  rename n1 n2 (TEcase e ccs t)         = TEcase (rename n1 n2 e)
                                            (map (renameCaseClause n1 n2) ccs) t
  rename n1 n2 (TEhandle e1 e2 t)       = TEhandle (rename n1 n2 e1)
                                                   (rename n1 n2 e2) t
  rename _  _  e                        = e