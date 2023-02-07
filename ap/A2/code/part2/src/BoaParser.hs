module BoaParser (ParseError, parseString) where

import BoaAST
import Text.ParserCombinators.ReadP

import BoaParser.ParseStmt

type ParseError = String

parseString :: String -> Either ParseError Program
parseString str =
  case readP_to_S (parseProgram <* eof) str of
    [(exp, _)] -> Right exp
    []         -> Left $ "No parse of: " ++ str
    out@((_, _):_rest) -> Left $ "Ambiguous parse. Unparsed: "
                                  ++ show' out

show' :: [(a, String)] -> String
show' [] = ""
show' ((_, b):rest) = (if not (null b) then b ++ ", " else "") ++ show' rest
