module Utils.Iseq (
  Iseq,
  iNil,
  iStr,
  iAppend,
  iNewline,
  iIndent,
  iConcat,
  iInterleave,
  indentation ) where

  data Iseq =
      INil
    | INewline
    | IStr String
    | IIndent Iseq
    | IAppend Iseq Iseq

  iNil :: Iseq
  iNil = INil

  iStr :: String -> Iseq
  iStr s = case lines s of
    [l] -> IStr l
    ls  -> iInterleave iNewline $ map IStr ls

  iAppend :: Iseq -> Iseq -> Iseq
  iAppend = IAppend

  iNewline :: Iseq
  iNewline = INewline

  iIndent :: Iseq -> Iseq
  iIndent = IIndent

  iConcat :: [Iseq] -> Iseq
  iConcat = foldr iAppend iNil

  iInterleave :: Iseq -> [Iseq] -> Iseq
  iInterleave _   []     = iNil
  iInterleave _   [s]    = s
  iInterleave sep (s:ss) = s `iAppend` sep `iAppend` iInterleave sep ss

  spaces :: Int -> String
  spaces n = replicate n ' '

  flatten :: Int -> [(Iseq, Int)] -> String
  flatten _   []                               =
    ""
  flatten col ((INil, _     ) : seqs)          =
    flatten col seqs
  flatten _   ((INewline, indent) : seqs)      =
    '\n':spaces indent ++ flatten indent seqs
  flatten col ((IStr s, _     ) : seqs)        =
    s ++ flatten (col + length s) seqs
  flatten col ((IIndent s, _     ) : seqs)     =
    flatten col ((s, col) : seqs)
  flatten col ((IAppend s1 s2, indent) : seqs) =
    flatten col ((s1, indent):(s2, indent):seqs)

  iDisplay :: Iseq -> String
  iDisplay iseq = flatten 0 [(iseq, 0)]

  instance Show Iseq where
    show = iDisplay

  indentation :: Iseq
  indentation = iStr "  "
