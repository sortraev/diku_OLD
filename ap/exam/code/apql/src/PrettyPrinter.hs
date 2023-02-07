module PrettyPrinter where

import Data.List
import Types
-- TODO: some imports probably missing.


-----------------------
--- PRETTY PRINTING ---
-----------------------
prettyProgram :: Program -> String
prettyProgram = intercalate "\n" . map prettyRule

prettyRule :: Rule -> String
prettyRule (Rule atom CTrue)    = prettyAtom atom ++ "."
prettyRule (Rule atom (CNot c)) = prettyAtom atom ++ " unless " ++ prettyCond c ++ "."
prettyRule (Rule atom c)        = prettyAtom atom ++ " if " ++ prettyCond c ++ "."

prettyAtom :: Atom -> String
prettyAtom (Atom name terms) = name ++ "(" ++
  intercalate ", " (map prettyTerm terms) ++ ")"

prettyCond :: Cond -> String
prettyCond (CAtom atom)       = prettyAtom atom
prettyCond CTrue              = "true"
prettyCond (CNot (CTrue))     = "false"
prettyCond (CEq  t1 t2)       = prettyTerm t1 ++ " is " ++ prettyTerm t2
prettyCond (CNot (CEq t1 t2)) = prettyTerm t1 ++ " is not " ++ prettyTerm t2
prettyCond (COr  c1 c2)       = prettyCond c1 ++ " or " ++ prettyCond c2
prettyCond (CAnd c1 c2)       = prettyCond c1 ++ " and " ++ prettyCond c2
prettyCond (CNot (COr c1 c2)) = prettyCond c1 ++ " implies " ++ prettyCond c2
prettyCond (CNot c)           = "not " ++ prettyCond c

prettyTerm :: Term -> String
prettyTerm (TVar  name) = name
prettyTerm (TData str)  = "\"" ++ str ++ "\""


prettyIDB :: IDB -> String
prettyIDB (IDB pspecs clauses) =
  "IDB:\n  PSPecs:  " ++ prettyPSpecs pspecs ++
  "\n  Clauses:\n    " ++ prettyClauses clauses

prettyClauses :: [Clause] -> String
prettyClauses = intercalate "\n    " . map prettyClause

prettyClause :: Clause -> String
prettyClause (Clause atom atoms tests) =
  "{" ++ prettyAtom atom ++ ", "
      ++ prettyMap prettyAtom atoms ++ ", "
      ++ prettyMap prettyTest tests ++ "}"

prettyPSpecs :: [PSpec] -> String
prettyPSpecs = bracket . intercalate ", " . map prettyPSpec

prettyPSpec :: PSpec -> String
prettyPSpec (pname, arity) = pname ++ "/" ++ show arity


prettyTest :: Test -> String
prettyTest (TNot atom)  = "not " ++ prettyAtom atom
prettyTest (TEq t1 t2)  = prettyTerm t1 ++ " is "     ++ prettyTerm t2
prettyTest (TNeq t1 t2) = prettyTerm t1 ++ " is not " ++ prettyTerm t2


prettyMap :: (a -> String) -> [a] -> String
prettyMap printer = bracket . intercalate ", " . map printer

bracket :: String -> String
bracket str = "[" ++ str ++ "]"
