import Data.Char


encode :: [Char] -> [Char]
encode (first:rest) = encode_single (toLower first) ++ encode rest
encode _       = ""


encode_single :: Char -> [Char]
encode_single letter
          | letter == 'e' = "."
          | letter == 't' = "-"
          | letter == 'i' = ".."
          | letter == 'a' = ".-"
          | letter == 'n' = "-."
          | letter == 'm' = "--"
          | letter == ' ' = ""
          | otherwise = undefined


-- decode_single :: [Char] -> Maybe Char
-- decode_single code
--           | code == "."    = Just 'e'
--           | code == "-"    = Just 't'
--           | code == ".."   = Just 'i'
--           | code == ".-"   = Just 'a'
--           | code == "-."   = Just 'n'
--           | code == "--"   = Just 'm'
          -- | otherwise      = Nothing

decode_single :: [Char] -> Char
decode_single code
          | code == "."    = 'e'
          | code == "-"    = 't'
          | code == ".."   = 'i'
          | code == ".-"   = 'a'
          | code == "-."   = 'n'
          | code == "--"   = 'm'
          | otherwise      = undefined

decode :: [Char] -> [[Char]]
decode "" = [""]
decode (x ++ xs) = map ((decode_single x) ++) $ decode xs
decode (x:y:ys)  = map 
