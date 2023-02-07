module BoaParser.ParseStmt where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

import BoaAST
import BoaParser.Extras
import BoaParser.ParseExp


-- a Program is:
--   * 1 or more semicolon-separated statements (the last statement should
--       not have a trailing semicolon)
--   * an arbitrary amount of trailing comments/whitespace.
parseProgram :: ReadP Program
-- parseProgram = sepBy1 parseStmt termSemicolon <* skipComment
parseProgram = skipComment >> sepBy1 parseStmt termSemicolon

----------------------
---- Stmt parsing ----
----------------------
parseStmt :: ReadP Stmt
parseStmt = parseStmtDef <|> parseStmtExp

parseStmtDef :: ReadP Stmt
parseStmtDef = SDef <$> (parseIdent' <* termEquals) <*> parseExp

parseStmtExp :: ReadP Stmt
parseStmtExp = SExp <$> parseExp
