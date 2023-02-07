import Debug.Trace
import Data.List

data IRep = INum Integer | IBinop Op IRep IRep
  deriving (Show)

data Op = Plus | Minus | Mult
  deriving (Show)

data Exp = Num Integer | Binop Op
  deriving (Show)

toExp :: [IRep] -> [Exp] -> [Exp]
toExp [] out = out
toExp (INum n : es) out        = toExp es (Num n : out)
toExp (IBinop op a b : es) out = toExp (es ++ [b, a]) (Binop op : out)


eval :: [Exp] -> Maybe Integer
eval exp = eval' exp []
  where eval' (Binop Mult  : es) (a : b : ns) = eval' es (ns ++ [a * b])
        eval' (Binop Plus  : es) (a : b : ns) = eval' es (ns ++ [a + b])
        eval' (Binop Minus : es) (a : b : ns) = eval' es (ns ++ [a - b])
        eval' (Num n:es) ns = eval' es (ns ++ [n])
        eval' [] [res] = Just res
        eval' _ _      = Nothing


-- run :: IRep -> Maybe Integer
run = eval . flip toExp [] . (:[])

example  = IBinop Minus (IBinop Plus (INum 1) (INum 3)) (IBinop Mult (INum 5) (INum 7))
example2 = IBinop Mult  (IBinop Plus (INum 1) (IBinop Minus (INum 3) (INum 5))) (INum 7)
example3 = IBinop Minus (IBinop Plus (INum 2) (IBinop Mult (INum 3) (INum 4)))
                        (IBinop Minus (IBinop Plus (INum 2) (INum 4)) (INum 1))

pretty (Num x)       = show x
pretty (Binop Plus)  = "+"
pretty (Binop Minus) = "-"
pretty (Binop Mult)  = "*"
