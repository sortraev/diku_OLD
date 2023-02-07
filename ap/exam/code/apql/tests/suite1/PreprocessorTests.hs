module PreprocessorTests where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List (sort)

import Types
import MyTestUtils



preprocessorTests :: TestTree
preprocessorTests = testGroup ">>>> Preprocessor tests"
  [
   transformTests
  ,clausifyTests
  ,stratifyTests
  ]



transformTests :: TestTree
transformTests = testGroup (">> Tests of each of the transformation "
                            ++ "equivalences in the assignment.")
  [
   posT "negation equivalence 1" "p() if not (a() and b())."
       [Rule (Atom "p" []) (CNot (CAtom (Atom "a" []))),
        Rule (Atom "p" []) (CNot (CAtom (Atom "b" [])))]
  ,posT "negation equivalence 2" "p() if not (a() or b())."
       [Rule (Atom "p" []) (CAnd (CNot (CAtom (Atom "a" [])))
                                 (CNot (CAtom (Atom "b" []))))]
  ,posT "negation equivalence 3" "p() if not (not a())."
       [Rule (Atom "p" []) (CAtom (Atom "a" []))]


  ,posT "associativity of or splitting" "foo(a, b) if x is y or a is not b or foo(b, b)."
                  [Rule (Atom "foo" [TVar "a",TVar "b"])
                        (CEq (TVar "x") (TVar "y")),
                   Rule (Atom "foo" [TVar "a",TVar "b"])
                        (CNot (CEq (TVar "a") (TVar "b"))),
                   Rule (Atom "foo" [TVar "a",TVar "b"])
                        (CAtom (Atom "foo" [TVar "b",TVar "b"]))]


  ,posT "and distribution equivalence" "p() if x is y and (a() or b())."
                  [Rule (Atom "p" []) (CAnd (CEq (TVar "x") (TVar "y"))
                                      (CAtom (Atom "a" []))),
                   Rule (Atom "p" []) (CAnd (CEq (TVar "x") (TVar "y"))
                                      (CAtom (Atom "b" [])))]
  ,posT "discard \"atom and false\" rules" "q() if p(). p() if false."
                  [Rule (Atom "q" []) (CAtom (Atom "p" []))]

  ,posT "multiple rounds of transformations needed"
         "p(x) if q(x) and not (r(x) and x is not a)."
                 [Rule (Atom "p" [TVar "x"])
                       (CAnd (CAtom (Atom "q" [TVar "x"]))
                             (CNot (CAtom (Atom "r" [TVar "x"])))),
                  Rule (Atom "p" [TVar "x"])
                       (CAnd (CAtom (Atom "q" [TVar "x"]))
                             (CEq (TVar "x") (TVar "a")))]

  , testGroup "> A couple of transformation properties" $
    [
     posT "transforming empty program" "" []
    ,equalT "or splitting" "foo(a, b) if bar(a, b) or baz(c, e)."
                           "foo(a, b) if bar(a, b). foo(a, b) if baz(c, e)."
    ,equalT "associativity of or splitting" "p() if (a() or (b() or (c() or d()) ) or e())."
                                            "p() if ((((a() or b()) or c()) or d()) or e())."
    ,equalT "commutativivity of or splitting" "p() if (a() or (b() or (c() or d()) ) or e())."
                                              "p() if ((((b() or d()) or a()) or e()) or c())."

    ]
  ]

clausifyTests :: TestTree
clausifyTests = testGroup ">> clausify tests"
  [
   posC "trivial satisfaction of variable occurence condition 1" "p() if a(b, c, \"d\", e)."
     (IDB [("p", 0)] [Clause (Atom "p" []) [Atom "a" [TVar "b", TVar "c", TData "d", TVar "e"]] []])

  ,posC "trivial satisfaction of variable occurence condition 2" "p(\"a\") if foo()."
     (IDB [("p", 1)] [Clause (Atom "p" [TData "a"]) [Atom "foo" []] []])



  ,posC "recursive satisfaction of occurence condition" "p(a, b, c) if p(c, b, a)."
     (IDB [("p", 3)] [Clause (Atom "p" [TVar "a", TVar "b", TVar "c"]) [Atom "p" [TVar "a", TVar "b", TVar "c"]] []])

  ,posC "many clauses, all satisfy occurence condition" "p(a) if bar(a). p(\"b\", \"c\") if x is not y and bar(x, y)."
     (IDB [("p", 1),  ("p", 2)]
          [Clause (Atom "p" [TVar "a"])             [Atom "bar" [TVar "a"]]           [],
           Clause (Atom "p" [TData "b", TData "c"]) [Atom "bar" [TVar "x", TVar "y"]] [TNeq (TVar "x") (TVar "y")]
          ])

  ,posCNoSort "simple test of correct preserving of order of rules in result IDB"
              "p(\"a\"). bar(a, b) if x is b and car(a, b, x). bar(a, b) if btl(b, a) and not not true. p(\"b\")."
     (IDB [("p",1), ("bar",2)]
          [Clause (Atom "p"   [TData "a"])         []                                        [],
           Clause (Atom "bar" [TVar "a",TVar "b"]) [Atom "car" [TVar "a",TVar "b",TVar "x"]] [TEq (TVar "x") (TVar "b")],
           Clause (Atom "bar" [TVar "a",TVar "b"]) [Atom "btl" [TVar "b",TVar "a"]]          [],
           Clause (Atom "p"   [TData "b"])         []                                        []])

  ,negC "test var missing from non-negated atom vars"   "p1() if x is not \"a\"."

  ,negC "head var missing from non-negated atom vars"   "p(a) if not bar(a)."

  ,negC "head var missing from non-negated atom vars 2" "p(a) if bar()."

  ,negC "head and test var missing from non-negated atom vars" "p(x, z) if x is y and q(x)."

  ,negC "many clauses, only one breaks occurence condition"
          "p(a) if and c(a). p(e) if not b(a) and c(e). p(a) if not(a)."

  ]


stratifyTests :: TestTree
stratifyTests = testGroup ">> stratify tests"
  [
  posS "Assert empty program produces empty strata"
          ("", []) []

 ,posS "Example stratification from the assignment text"
          ("p() if q() and r(). p() if p() and not r(). q() if q()" ++
           " and not s(). s() if r().", [("r", 0)])
          [[("s", 0)], [("p", 0), ("q", 0)]]

 ,posS "Multiple strata" ("different3(a, b, a) if not same(a, a) and is_happy(a, b)." ++
                          "same(a, b) if not different2(a, b) and error(b, a)." ++
                          "error(a, b) if is_dead_var(a) and is_dead_var(b)." ++
                          "tautology(yes, no) if ((yes(yes) and no(no)) implies error(a, b)) " ++
                          "and same(yes, no)."
                          , [("different2", 2), ("is_dead_var", 1)])
                       [[("same", 2), ("error", 2)], [("different3", 3), ("tautology", 2)]]

 ,posS "`is_taller` example" 
   ("is_taller(a, b) if taller(a, b). is_taller(a, b) if " ++
               "taller(a, c) and is_taller(c, b).", [("taller", 2)]) [[("is_taller", 2)]]

 ,negS "Overlap in ex- and intensional predicates"
         ("p() if q() and r(). p() if p() and not r(). q() if q() and not s(). s() if r().",
               [("foo", 17), ("bar", 8), ("p", 0), ("a", 3)])

 ,negS "Mutually recursive negative references within same stratum"
         ("foo(a, b) if not bar(b, a) and baz(a, b). bar(a, b) if not foo(b, b) and baz(b, a).",
                       [])
  ]


