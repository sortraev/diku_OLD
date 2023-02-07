import Data.Char


encode :: [Char] -> [Char]
encode (first:rest) = encode_single (toLower first) ++ encode rest
encode _       = ""


encode_single :: Char -> [Char]
encode_single letter
          | letter == 'a' = ".-" 
          | letter == 'b' = "-..." 
          | letter == 'c' = "-.-." 
          | letter == 'd' = "-.." 
          | letter == 'e' = "." 
          | letter == 'f' = "..-." 
          | letter == 'g' = "--." 
          | letter == 'h' = "...." 
          | letter == 'i' = ".." 
          | letter == 'j' = ".---" 
          | letter == 'k' = "-.-" 
          | letter == 'l' = ".-.." 
          | letter == 'm' = "--" 
          | letter == 'n' = "-." 
          | letter == 'o' = "---" 
          | letter == 'p' = ".--." 
          | letter == 'q' = "--.-" 
          | letter == 'r' = ".-." 
          | letter == 's' = "..." 
          | letter == 't' = "-" 
          | letter == 'u' = "..-" 
          | letter == 'v' = "...-" 
          | letter == 'w' = ".--" 
          | letter == 'x' = "-..-" 
          | letter == 'y' = "-.--" 
          | letter == 'z' = "--.." 
          | letter == ' ' = "" 
          | otherwise = undefined


decode_single :: [Char] -> Maybe Char
decode_single letter
          | code == ".-"   = Just 'a'
          -- | code == "-..." = Just 'b'
          -- | code == "-.-." = Just 'c'
          -- | code == "-.."  = Just 'd'
          | code == "."    = Just 'e'
          -- | code == "..-." = Just 'f'
          -- | code == "--."  = Just 'g'
          -- | code == "...." = Just 'h'
          | code == ".."   = Just 'i'
          -- | code == ".---" = Just 'j'
          -- | code == "-.-"  = Just 'k'
          -- | code == ".-.." = Just 'l'
          | code == "--"   = Just 'm'
          | code == "-."   = Just 'n'
          -- | code == "---"  = Just 'o'
          -- | code == ".--." = Just 'p'
          -- | code == "--.-" = Just 'q'
          -- | code == ".-."  = Just 'r'
          -- | code == "..."  = Just 's'
          | code == "-"    = Just 't'
          -- | code == "..-"  = Just 'u'
          -- | code == "...-" = Just 'v'
          -- | code == ".--"  = Just 'w'
          -- | code == "-..-" = Just 'x'
          -- | code == "-.--" = Just 'y'
          -- | code == "--.." = Just 'z'
          | otherwise      = Nothing

decode :: [Char] -> [[Char]]
decode "" = [""]
decode (c1:rest)          = decode_single c1 : decode rest
decode (c1:c2:rest)       = decode_single c1 : [decode_single c2]


