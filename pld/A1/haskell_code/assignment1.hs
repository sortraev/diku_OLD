import Data.List

merge :: [a] -> [a] -> [a]
-- merge xs [] = xs
-- merge [] ys = ys
merge (x:xs) (y:ys) = x : y : merge xs ys
merge xs ys = xs ++ ys


data Element = Number Integer | Symbol String | List [Element]
add :: Element -> Integer
add (List (x:xs)) = add x + add (List xs)
add (Number x)    = x
add _             = 0



example = List [Number 4, Symbol "()", List
                 [Number 5, List [Number (-6), Number 0,
                                   List [Number 1, Symbol "NULL", Number (-1)]],
                    Number 7, Symbol "cons"],
                 Number 3, Symbol "foo"]
