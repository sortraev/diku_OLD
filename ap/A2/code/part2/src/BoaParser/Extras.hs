module BoaParser.Extras where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))
import Data.Functor (void)
import Control.Monad (guard)
import Data.Char


----------------------------------------
---- utilities and helper functions ----
----------------------------------------
prepend :: ReadP a -> ReadP [a] -> ReadP [a]
prepend = (<*>) . (<$>) (:)

anyChar = satisfy (const True)

oneOf :: [Char] -> ReadP Char
oneOf xs = satisfy (`elem` xs)

parenthesized, bracketed :: ReadP a -> ReadP a
parenthesized = between termOpenPar     termClosePar
bracketed     = between termOpenBracket termCloseBracket


-- can this string immediately follow a keyword?
-- also used by `parseIdent`.
canFollowKeyword :: String -> Bool
canFollowKeyword (c:_) = not (isDigit c || isAlpha c || c == '_')
canFollowKeyword _     = True


-- parses a keyword, but only if that keyword is followed by
-- by something that can legally follow a keyword.
keyword :: String -> ReadP String
keyword kw = string' kw >>= \s -> look >>=
               guard . canFollowKeyword >> return s




-- skip zero or more comments, where a comment is:
--   * a leading '#' preceded by zero or more whitespace characters.
--   * zero or more arbitrary characters (the assignment text does not specify
--       restrictions on the contents of comments).
--   * a terminating newline or EOF.
skipComment :: ReadP ()
skipComment = many (skipSpaces >> char '#' >>
                manyTill (satisfy isPrint) parseCommentTerminator) >>
                 skipSpaces
 where parseCommentTerminator = eof <|> void (satisfy (== '\n'))


-- used to tokenize parsers. skips and leading comments and/or whitespace.
lexeme :: ReadP a -> ReadP a
lexeme p = skipComment >> p

char' :: Char -> ReadP Char
char'   = lexeme . char
string' :: String -> ReadP String
string' = lexeme . string


--------------------------------
---- keywords and terminals ----
--------------------------------
letter, num, positiveNum, listDelim :: ReadP Char
letter      = oneOf (['a'..'z'] ++ ['A'..'Z'])
num         = oneOf ['0'..'9']
positiveNum = oneOf ['1'..'9']

listDelim   = char' ',' -- TODO: just char ','? whitespace surrounding
                        -- delims might be handled in other parsers.

keywordFor   = keyword "for"
keywordIn    = keyword "in"
keywordIf    = keyword "if"
keywordNot   = keyword "not"
keywordNotIn = keywordNot >> keywordIn

termTrue  = string' "True"
termFalse = string' "False"
termNone  = string' "None"

-- binary op terminals
termPlus  = char' '+'
termMinus = char' '-'
termTimes = char' '*'
termMod   = char' '%'
termDiv   = string' "//"

termLess = char' '<'
termLessEq    = string' "<="
termGreater   = char'   '>'
termGreaterEq = string' ">="
termEq        = string' "=="
termNotEq     = string' "!="

-- brackets and parentheses
termOpenPar      = char' '('
termClosePar     = char' ')'
termOpenBracket  = char' '['
termCloseBracket = char' ']'

-- for SDef's
termEquals    = char' '='
termSemicolon = char' ';'

reservedKeywords :: [String]
reservedKeywords = ["None", "True", "False", "for", "in", "if", "not"]
