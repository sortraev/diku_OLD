module ParserTests where

import Test.Tasty
import Test.Tasty.HUnit

import Types
import MyTestUtils



-------------------
--- PARSER TEST ---
-------------------
parserTests :: TestTree
parserTests = testGroup ">>>> Parser tests"
  [mainParserTests
  ,miscTests
  ]

mainParserTests :: TestTree
mainParserTests = testGroup ">> Cond and rule tests"
  [

   posP "the empty program" "" []

  ,posP "right-association of implies" "a() if b() implies c() implies d()."
     [Rule (Atom "a" []) (CNot (COr (CAtom (Atom "b" [])) (CNot (COr (CAtom (Atom "c" [])) (CAtom (Atom "d" []))))))]

  ,posP "left-association of or" "a() if b() or c() or d()."
     [Rule (Atom "a" []) (COr (COr (CAtom (Atom "b" [])) (CAtom (Atom "c" []))) (CAtom (Atom "d" [])))]

  ,posP "left-association of and" "a() if b() and c() and d()."
     [Rule (Atom "a" []) (CAnd (CAnd (CAtom (Atom "b" [])) (CAtom (Atom "c" []))) (CAtom (Atom "d" [])))]

  ,posP "precedence of and/or/implies" "a() if b() and c() implies d() or e()."
     [Rule (Atom "a" []) (CNot (COr (CAnd (CAtom (Atom "b" [])) (CAtom (Atom "c" [])))
                                    (COr (CAtom (Atom "d" []))
                                         (CAtom (Atom "e" [])))))]

  ,posP "associativity of logical negation" "p() if not x() and not not not x()."
     [Rule (Atom "p" []) (CAnd (CNot (CAtom (Atom "x" []))) (CNot (CNot (CNot (CAtom (Atom "x" []))))))]
  ,posP "precedence of logical negation" "p() if not x() and not not not x()."
     [Rule (Atom "p" []) (CAnd (CNot (CAtom (Atom "x" []))) (CNot (CNot (CNot (CAtom (Atom "x" []))))))]
  ,posP "precedence of parenthesized expressions" "p() if not (x() or y())."
     [Rule (Atom "p" []) (CNot (COr (CAtom (Atom "x" [])) (CAtom (Atom "y" []))))]

  ,posP "precedence of \"is\"/\"is not\" 1:" "p() if not x is y."
     [Rule (Atom "p" []) (CNot (CEq (TVar "x") (TVar "y")))]

  ,posP "precedence of \"is\"/\"is not\" 2" "p() if not x is \"y\"."
     [Rule (Atom "p" []) (CNot (CEq (TVar "x") (TData "y")))]


  ,negP "non-association of \"is\""     "p() if x is y is z."
  ,negP "non-association of \"is not\"" "p() if x is not y is not z."

  ,negP "missing closing parenthesis" "foo(a, b) if (not (bar(y) and (x is y or baz()))."
  ,negP "unexpected closing parenthesis" "foo(a, b) if not bar(y) and x is y or baz())."
  ,negP "parenthesis around terms" "foo(a, b) if (\"x\") is y."
  ,negP "missing rule terminating period" "foo(a, b)"

  ,testGroup "> A couple of Cond and Rule parsing properties" $
    [
     "p() if x is not y."      `equalP` "p() if not x is y."
    ,"p() unless q()."         `equalP` "p() if not q()."
    ,"p() if a() implies b()." `equalP` "p() if not (a() or b())."
    ,"p()."                    `equalP` "p() if true."

    ,"p(a, b, c) if (not((not((bar(x))))and foo is baz))." `equalP`
     "p(a, b, c) if not (not bar(x) and foo is baz)."
    ]
  ]



miscTests :: TestTree
miscTests = testGroup ">> Misc parser tests"
  [
   stringConstTests
  ,keywordHandlingTests
  ,whitespaceHandlingTests
  ]

stringConstTests :: TestTree
stringConstTests = testGroup "> String constant tests"
  [
   posP "simple string const" "foo(\"barrrr\")."
     [Rule (Atom "foo" [TData "barrrr"]) CTrue]

     
  ,posP "string const with printable whitespace" "foo(\"   hej der \")."
     [Rule (Atom "foo" [TData "   hej der "]) CTrue]

  ,posP "double quotes in string const" "foo(\"\"\"fooo\"\"\")."
     [Rule (Atom "foo" [TData "\"fooo\""]) CTrue]

  ,posP "nested double quotes in string const" "foo(\"\"\"foo\"\"fooo\"\"\"\"bar\")."
     [Rule (Atom "foo" [TData "\"foo\"fooo\"\"bar"]) CTrue]

  ,negP "printable but non-ascii characters" "foo(\"€€€apeæøå\")."
  ,negP "non-printable characters" "foo(\"'\255', '\19'\")."
  ,negP "non-printable whitespace" "foo(\"    \t\")."
  ,negP "bad double quotes in string const" "foo(\"\"hej der\"\"\")."
  ]

keywordHandlingTests :: TestTree
keywordHandlingTests = testGroup "> Keyword handling tests"
  [
   posP "respected keywords" "foo() if a is b." 
     [Rule (Atom "foo" []) (CEq (TVar "a") (TVar "b"))]
  ,posP "notx correctly parsed as a variable" "foo(x, y) if y is notx."
     [Rule (Atom "foo" [TVar "x",TVar "y"]) (CEq (TVar "y") (TVar "notx"))]
  ,posP "case sensitive reserved keywords" "foo(x, y) if uNless is And."
     [Rule (Atom "foo" [TVar "x",TVar "y"]) (CEq (TVar "uNless") (TVar "And"))]

  ,negP "reserved keyword as term"       "p() if (and is yes)."
  ,negP "reserved keyword as data const" "p() if and"
  ,negP "reserved keyword as pred name"  "p() if and"
  ]


whitespaceHandlingTests :: TestTree
whitespaceHandlingTests = testGroup "> Whitespace handling tests" $
  (map ($ pos_tests_expected)
 [
  posP "sane amount of whitespace" $ "foo(a,b) unless\n\t\t (car(a) implies man(b)) and baz(b)\n\t or\n\t\t" ++
                                    "true and \"b\" is not \"a\".\n\nbaz(\"b\")."

 ,posP "minimal whitespace (parentheses instead)" $ "foo(a,b)unless(car(a)implies(man(b)))and(baz(b))or(true)" ++
                                                   "and(\"b\"is not\"a\").baz(\"b\")."

 ,posP "spaces all over" $ " foo ( a , b ) unless ( car ( a ) implies ( man ( b ) ) ) and ( baz ( b ) ) or" ++
                              " ( true ) and ( \"b\" is not \"a\" ) . baz ( \"b\" ) . "

 ,posP "comments as whitespace" $ "foo(a,b)(**)unless(**)(car(a)(**)implies(**)man(b))(**)and(**)baz(b)(**)" ++
                                 "or(**)true(**)and(**)\"b\"(**)is(**)not(**)\"a\".(**)baz(\"b\")."

 ,posP "tab and newlines" $ "foo(a,b)\n unless\n (car(a)\t\t implies man(\nb)) and baz(b\n) or true\n" ++
                           " and \"b\" is not \"a\".\nbaz(\"b\")."

 ,posP "all types of whitespace" $ "foo(a,b)\n\r unless\v\v\n (car(a)\t\t\n implies\f\f\f man(\nb)) and\r" ++
                                  " baz(b\n) or\v true\n and \"b\" is not \"a\".\nbaz(\"b\")."
 ])
 ++
 [
  posP "just a comment"  "(*just a comment*)" []
 ,posP "trailing comment"  "foo().(**)" [Rule (Atom "foo" []) CTrue]
 ,posP "leading comment"  "(*hej*)foo()."  [Rule (Atom "foo" []) CTrue]
 ,negP "missing whitespace 1" "p() ifq()."
 ,negP "missing whitespace 2" "p() if yes isno."
 ,negP "missing whitespace 3" "p() if yesis no."
 ,negP "missing whitespace 4" "p() if bar() andbaz()."
 ]
 where pos_tests_expected =
         [Rule (Atom "foo" [TVar "a",TVar "b"])
                 (CNot (COr
                         (CAnd (CNot (COr (CAtom (Atom "car" [TVar "a"]))
                                          (CAtom (Atom "man" [TVar "b"]))))
                               (CAtom (Atom "baz" [TVar "b"])))
                         (CAnd
                           CTrue
                           (CNot (CEq (TData "b") (TData "a")))))),
           Rule (Atom "baz" [TData "b"]) CTrue]
