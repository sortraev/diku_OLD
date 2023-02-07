{-# LANGUAGE StandaloneDeriving #-}
module MyTestUtils where

import Data.List (sort)

import Test.Tasty
import Test.Tasty.HUnit
import Types

import PreprocessorImpl
import ParserImpl



-----------------------------------------
--- GENERIC TESTING UTILITY FUNCTIONS ---
-----------------------------------------
posTestGeneric :: (Show b, Eq b) =>
                  (a -> Either ErrMsg b) -> String -> a -> b -> TestTree
posTestGeneric test_me test_name input expected =
  testCase test_name $ (test_me input) @?= (Right expected)


negTestGeneric :: (Eq a, Show b) =>
                  (a -> Either ErrMsg b) -> String -> a -> TestTree
negTestGeneric test_me test_name input = testCase ("*NEG* " ++ test_name) $
  case (test_me input) of
    Left (EUser _)            -> return ()
    Left (EUnimplemented err) -> assertFailure err
    Left (EInternal err)      -> assertFailure $ ">> Unexpected internal error: " ++ err
    Right p                   -> assertFailure $ ">> Unexpected success! Got: "   ++ show p


equalGeneric :: (Show b, Eq b) =>
                (a -> Either ErrMsg b) -> String -> a -> a -> TestTree
equalGeneric test_me test_name input1 input2 =
  testCase test_name $ (test_me input1) @?= (test_me input2)










----------------------------------------
--- PARSER TESTING UTILITY FUNCTIONS ---
----------------------------------------
posP = posTestGeneric parseString
negP = negTestGeneric parseString
equalP a b = equalGeneric parseString (a ++ " <=> " ++ b) a b



----------------------------------
--- CLAUSIFY TESTING UTILITIES ---
----------------------------------
deriving instance Ord Rule
deriving instance Ord Atom
deriving instance Ord Term
deriving instance Ord Cond
deriving instance Ord IDB
deriving instance Ord Test
deriving instance Ord Clause

sortIDB :: IDB -> IDB
sortIDB (IDB ps cs) = IDB (sort ps) (map sortClause cs)
  where sortClause (Clause atom atoms tests) = Clause (sortAtom atom) (map sortAtom atoms) (sort tests)
        sortAtom (Atom name args) = Atom name (sort args)

clausify' input = sortIDB <$> (parseString input >>= clausify)


posC test_name input expected = posTestGeneric clausify' test_name input (sortIDB expected)
negC   = negTestGeneric clausify'
equalC = equalGeneric   clausify'

-- used to test preserving of order of rules in the output IDB.
clausifyNoSort input = parseString input >>= clausify
posCNoSort = posTestGeneric clausifyNoSort


-- transform testing
transform' str = sort . transform <$> parseString str
posT test_name input expected = posTestGeneric transform' test_name input (sort expected)
negT    = negTestGeneric transform'
equalT  = equalGeneric   transform'



----------------------------------
--- STRATIFY TESTING UTILITIES ---
----------------------------------
stratify' (input, eps) = (parseString input >>= clausify >>= flip stratify eps)

posS   = posTestGeneric stratify'
negS   = negTestGeneric stratify'
equalS = equalGeneric   stratify'

