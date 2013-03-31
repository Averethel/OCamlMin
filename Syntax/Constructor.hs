module Syntax.Constructor where
  import Utils.Iseq

  data Constructor =
      CNnil
    | CNcons
    | CNpair
    | CNtrue
    | CNfalse
    | CNunit
    deriving Eq

  pprConstructor :: Constructor -> Iseq
  pprConstructor CNnil    = iStr "NIL"
  pprConstructor CNcons   = iStr "CONS"
  pprConstructor CNpair   = iStr "PAIR"
  pprConstructor CNtrue   = iStr "TRUE"
  pprConstructor CNfalse  = iStr "FALSE"
  pprConstructor CNunit   = iStr "UNIT"

  instance Show Constructor where
    show = show . pprConstructor

  arity :: Constructor -> Int
  arity CNnil   = 0
  arity CNcons  = 2
  arity CNpair  = 2
  arity CNtrue  = 0
  arity CNfalse = 0
  arity CNunit  = 0
