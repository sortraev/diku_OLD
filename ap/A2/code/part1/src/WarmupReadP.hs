module WarmupReadP where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).
-- Rewritten grammar, without left-recursion:
--   E ::= T E'
--       | "-" T E'
--   E' ::= "+" E'
--        | "-" E'
--        | eps
--   T ::= num
--       | "(" E ")" .

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

type Parser a = ReadP a   -- may use synomym for easier portability to Parsec

type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp | Mul Exp Exp
  deriving (Eq, Show)

runParser :: ReadP a -> ReadS a
runParser = readP_to_S

parseString :: String -> Either ParseError Exp
parseString str = 
  case runParser (parseE <* eof) str of
    [(res, _)] -> Right res
    []         -> Left "No parse!"
    _          -> Left "Ambiguous parse!"


-- E ::= T E'
--     | - T E'
parseE :: ReadP Exp
parseE = (parseT >>= parseE') <|>
         (skipSpaces >> char '-' >> parseT >>= parseE' . Negate)


-- E' ::= + E'
--      | - E'
--      | eps
parseE' :: Exp -> ReadP Exp
parseE' e = skipSpaces >> option e ((char '+' >> binOp e id)
                               <|>  (char '-' >> binOp e Negate))
  where binOp e maybeNegate = parseT >>= parseE' . Add e . maybeNegate


-- T ::= num
--     | ( E )
parseT :: ReadP Exp
parseT = skipSpaces >> parseNum <|> parenthesized parseE
  where parenthesized = between (char '(') (char ')')

parseNum :: ReadP Exp
parseNum = Num . read <$> digits
  where digits = skipSpaces >> (string "0" <|> ((:) <$> posNum <*> many num))
        posNum = satisfy (`elem` ['1'..'9'])
        num    = satisfy (`elem` ['0'..'9'])
