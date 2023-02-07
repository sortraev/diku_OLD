module BoaParser.ParseExp where

import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>), (<**>))
import Data.Functor (($>))
import Data.Char (isPrint, isSpace)
import Control.Monad (guard)

import BoaAST
import BoaParser.Extras

---------------------
---- Exp parsing ----
---------------------
parseExp :: ReadP Exp
parseExp = (keywordNot >> Not <$> parseExp) <|> parseRelBinOps


parseRelBinOps :: ReadP Exp
parseRelBinOps = parseArithBinOps <**> relBinOp <*> parseArithBinOps
                 <|> parseArithBinOps
  where relBinOp = termLess      $> Oper Less
               <|> termGreater   $> Oper Greater
               <|> termEq        $> Oper Eq
               <|> keywordIn     $> Oper In
               <|> termGreaterEq $> (Not .) . Oper Less
               <|> termLessEq    $> (Not .) . Oper Greater
               <|> termNotEq     $> (Not .) . Oper Eq
               <|> keywordNotIn  $> (Not .) . Oper In

        -- (??) = (((<*>) .) f .) . return -- <*> return a

parseArithBinOps :: ReadP Exp
parseArithBinOps = infix1
  where infix1 = chainl1 infix2    $ Oper <$> (termPlus  $> Plus
                                          <|>  termMinus $> Minus)
        infix2 = chainl1 parseAtom $ Oper <$> (termTimes $> Times
                                          <|>  termDiv   $> Div
                                          <|>  termMod   $> Mod)


parseAtom :: ReadP Exp
parseAtom = lexeme $ parseConst
                 <|> parseVarExp
                 <|> parseComprExp
                 <|> parseListExp
                 <|> parseCallExp
                 <|> parenthesized parseExp

parseVarExp :: ReadP Exp
parseVarExp = Var <$> parseIdent


---------------------------------
---- Const and value parsing ----
---------------------------------
parseConst :: ReadP Exp
parseConst = Const <$> (parseIntVal <|> parseStringVal <|> parseMiscVal)

parseListExp :: ReadP Exp
parseListExp  = bracketed $ List <$> sepBy parseExp listDelim

parseComprExp :: ReadP Exp
parseComprExp = bracketed $ Compr <$> parseExp <*> cclauses

  where cclauses = ccFor `prepend` many cclause
        cclause  = ccFor <|> ccIf
        ccFor    = keywordFor >> CCFor <$> (parseIdent' <* keywordIn) <*> parseExp
        ccIf     = keywordIf  >> CCIf <$> parseExp



parseCallExp :: ReadP Exp
parseCallExp = Call <$> parseIdent <*> parenthesized parseArgs
  where parseArgs = sepBy parseExp listDelim


-----------------------
---- Value parsing ----
-----------------------
parseIntVal :: ReadP Value
parseIntVal = IntVal <$> (parsePosInt <|> parseNegInt)
  where parsePosInt =          read <$> digits
        parseNegInt = negate . read <$> ((char '-') >> digits)
        digits = (string "0") <|> (positiveNum `prepend` many num)

-- TODO: restrictions on string literals; see assignment text
parseStringVal :: ReadP Value
parseStringVal = StringVal <$> between (char '\'') (char '\'')
                               (many (satisfy isString))
  where isString c = isPrint c || isSpace c
 

parseMiscVal :: ReadP Value
parseMiscVal = (termTrue  $> TrueVal)
           <|> (termFalse $> FalseVal)
           <|> (termNone  $> NoneVal)

--------------
---- misc ----
--------------
parseIdent, parseIdent' :: ReadP String
parseIdent = ident >>= \i -> look >>= guard . canFollowKeyword
  >> guard (i `notElem` reservedKeywords) >> return i

  where ident     = prepend identHead identTail
        identHead = letter <|> char '_'
        identTail = many (identHead <|> num)

parseIdent' = lexeme $ parseIdent
