module ParserImpl where

import Text.ParserCombinators.ReadP
import Control.Monad (guard)
import Control.Applicative ((<|>),  (<**>), liftA2)
import Data.Functor (($>))
import Data.Char (isAscii, isPrint, isAlpha, isDigit)

import Utils ((.:))
import Types

-------------
---  API  ---
-------------
parseString :: String -> Either ErrMsg Program
parseString = parseMain (parseProgram <* eof)

parseMain :: ReadP a -> String -> Either ErrMsg a
parseMain parser str = 
  case readP_to_S parser str of
    [(result, _)] -> Right result
    []            -> Left $ EUser     $ "Parsing error! Invalid program."
    _             -> Left $ EInternal $ "Internal error! Ambiguous parse."



----------------------------------
---  PROGRAM AND RULE PARSING  ---
----------------------------------
-- A program is zero or more rules each terminated with a period.
-- A program can, of course, befollowed by arbitrary whitespace
-- and arbitrarily many comments.
parseProgram :: ReadP Program
parseProgram = endBy parseRule (char' '.') <* skipComments

parseRule :: ReadP Rule
parseRule = flip Rule CTrue <$> parseAtom
        <|> ruleA (parseAtom <* keyword "if") parseCond
        <|> ruleA (parseAtom <* keyword "unless") (CNot <$> parseCond)
  where ruleA = liftA2 Rule



----------------------
---  COND PARSING  ---
----------------------
parseCond :: ReadP Cond
parseCond = parseCondBinOps

parseCondBinOps :: ReadP Cond
parseCondBinOps = chainr1 infix2   $ keyword "implies" $> CNot .: COr
  where infix2  = chainl1 infix3   $ keyword "or"      $> COr
        infix3  = chainl1 parseNot $ keyword "and"     $> CAnd

parseNot :: ReadP Cond
parseNot = (keyword "not" >> CNot <$> parseNot) <|> parseBottom

-- Not really the bottom of the parse tree, but rather the
-- bottommost internal node in the conceptual parse tree.
parseBottom :: ReadP Cond
parseBottom = CAtom <$> parseAtom
          <|> parseBoolConsts
          <|> parseTermBinOps
          <|> parenthesized parseCond

parseBoolConsts :: ReadP Cond
parseBoolConsts = keyword "true"  $> CTrue
              <|> keyword "false" $> CNot CTrue



-------------------------------
---  TERM AND ATOM PARSING  ---
-------------------------------
parseTermBinOps :: ReadP Cond
parseTermBinOps = parseTerm <**> termBinOp <*> parseTerm
  where termBinOp = keyword "is"                   $> CEq
               <|> (keyword "is" >> keyword "not") $> CNot .: CEq

parseTerm :: ReadP Term
parseTerm = (TVar <$> parseName) <|> (TData <$> parseData)

parseTermz :: ReadP [Term]
parseTermz = sepBy parseTerm (char' ',')

parseAtom :: ReadP Atom
parseAtom = Atom <$> parseName <*> parenthesized parseTermz



----------------------
---  MISC PARSING  ---
----------------------
parseName :: ReadP VName
parseName = lexeme $ name >>= \i -> look >>= guard .
              (i `notElem` reserved &&) . canFollowKeyword >> return i
  where name     = liftA2 (:) letter nameTail
        nameTail = many (letter <|> number <|> char '_')
        reserved = ["and", "or", "true", "false", "if",
                    "unless", "implies", "is", "not"]

parseData :: ReadP Data
parseData = lexeme $ between (char '"') (char '"') strConst
  where strConst = many (string "\"\"" $> '"' <|>   -- replace double quotes with single quotes;
                     (satisfy isStrContent))        -- else parse anything printable but single quotes.
        isStrContent c = isPrint c && isAscii c && c /= '"'


-- Comment parser.
-- FIXME: probably parses too much whitespace, but this
-- only a matter of efficiency and thus not a priority.
skipComments :: ReadP ()
skipComments = many (skipSpaces >> string "(*" >>
                manyTill (satisfy (const True)) (string "*)")) >> skipSpaces

-- Keyword parser. Parse only successful if keyword is
-- followed by something that can legally follow a keyword.
keyword :: String -> ReadP String
keyword = (<* (look >>= guard . canFollowKeyword)) . string'

-- Also used by parseName.
canFollowKeyword :: String -> Bool
canFollowKeyword (c:_) = not (isDigit c || isAlpha c)
canFollowKeyword _     = True


---------------
--  HELPERS  --
---------------
-- I choose to skip whitespace *before* parsers, since this is what I have been
-- used to working with since before becoming familiar with Andrzej's advice of
-- parsing white-space *after* parsers.
lexeme :: ReadP a -> ReadP a
lexeme = (skipComments >>)

char' :: Char -> ReadP Char
char' = lexeme . char

string' :: String -> ReadP String
string' = lexeme . string

oneOf :: [Char] -> ReadP Char
oneOf = satisfy . flip elem

letter, number, anyChar :: ReadP Char
letter  = satisfy isAlpha
number  = satisfy isDigit
anyChar = satisfy (const True)

parenthesized :: ReadP a -> ReadP a
parenthesized = between (char' '(') (char' ')')
