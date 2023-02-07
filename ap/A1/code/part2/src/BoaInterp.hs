-- Skeleton file for Boa Interpreter. Edit only definitions with 'undefined'

module BoaInterp
  (Env, RunError(..), Comp(..),
   abort, look, withBinding, output,
   truthy, operate, apply,
   eval, exec, execute)
  where

import BoaAST
import Control.Monad
import Data.List (intercalate)

type Env = [(VName, Value)]

data RunError = EBadVar VName | EBadFun FName | EBadArg String
  deriving (Eq, Show)

newtype Comp a = Comp {runComp :: Env -> (Either RunError a, [String]) }

instance Monad Comp where
  return a = Comp (\_ -> (return a, mempty))
  m >>= f =
    Comp (\env -> case runComp m env of
                    (Left re, s) -> (Left re, s)
                    (Right a, s) -> (a', mappend s s')
                      where (a', s') = runComp (f a) env)

-- You shouldn't need to modify these
instance Functor Comp where
  fmap = liftM
instance Applicative Comp where
  pure = return
  (<*>) = ap

-- Operations of the monad
abort :: RunError -> Comp a
abort err = Comp (\_ -> (Left err, mempty))


look :: VName -> Comp Value
look v = Comp $ \env -> let res = maybe (Left $ EBadVar v) Right (find v env)
                        in (res, mempty)
  where find :: VName -> Env -> Maybe Value
        find v ((w, val):xs) = if v == w then Just val else find v xs
        find _ [] = Nothing


withBinding :: VName -> Value -> Comp a -> Comp a
withBinding v val m = Comp $ \env -> runComp m ((v, val):env)


output :: String -> Comp ()
output s = Comp $ \_ -> (Right (), [s])


truthy :: Value -> Bool
truthy NoneVal  = False
truthy FalseVal = False
truthy TrueVal  = True
truthy (IntVal n)    = n /= 0
truthy (StringVal s) = not $ null s
truthy (ListVal xs)  = not $ null xs


-- the reverse of truthy :)
truthy' :: Bool -> Value
truthy' True  = TrueVal
truthy' False = FalseVal


operate :: Op -> Value -> Value -> Either String Value
operate Plus  (IntVal a) (IntVal b) = Right $ IntVal $ a + b
operate Minus (IntVal a) (IntVal b) = Right $ IntVal $ a - b
operate Times (IntVal a) (IntVal b) = Right $ IntVal $ a * b

operate Div   (IntVal a) (IntVal b) = if b /= 0 then Right $ IntVal $ div a b
                                                else Left "Div error: zero divisor"
operate Mod   (IntVal a) (IntVal b) = if b /= 0 then Right $ IntVal $ mod a b
                                                else Left "Mod error: zero divisor"

operate Eq a b = Right $ truthy' $ a == b               -- a single case, since Value implements Eq.
operate Less    (IntVal a) (IntVal b) = Right $ truthy' $ a < b
operate Greater (IntVal a) (IntVal b) = Right $ truthy' $ a > b

operate In a (ListVal xs) = Right $ truthy' $ elem a xs -- again, simple solution since Value implements Eq.

operate op _ _      = Left $ "Type mismatch for " ++ show op


apply :: FName -> [Value] -> Comp Value
apply "print" args = output (unwords $ map showVal args) -- unwords necessary in order to format correctly.
                       >> return NoneVal
  where showVal NoneVal  = "None"
        showVal TrueVal  = "True"
        showVal FalseVal = "False"
        showVal (IntVal n) = show n
        showVal (StringVal s) = s
        showVal (ListVal xs) = "[" ++ intercalate ", " (map showVal xs) ++ "]"

apply "range" args = -- apply "range" is a mess, but it works.
  case args of
    [IntVal hi]                         -> boaRange 0  hi 1
    [IntVal lo, IntVal hi]              -> boaRange lo hi 1
    [IntVal lo, IntVal hi, IntVal step] -> boaRange lo hi step
    args -> abort $ EBadArg $ argError $ length args

    where argError num_args = "range expects " ++
            if elem num_args [1..3] then "integer arguments only"
            else "1-3 arguments, got " ++ show num_args
          boaRange lo hi step =
            if step == 0 then abort $ EBadArg "range arg 3 must not be zero"
            else return $ ListVal $ map IntVal
              [lo, lo+step..hi + (if step < 0 then 1 else -1)]

apply fname _ = abort $ EBadFun fname


buildCompr :: Comp Value -> [CClause] -> Comp [Value]
buildCompr out [] = (:[]) <$> out
buildCompr out (CCFor v e:cs) = eval e >>= \e_val ->
  case e_val of   -- bind xs to v; evaluate rest of clauses; concat results
    ListVal xs -> concatMapM (\x -> withBinding v x (buildCompr out cs)) xs
    _          -> abort $ EBadArg "Compr error: generator expects list expression"
  where concatMapM f xs = concat <$> mapM f xs

buildCompr out (CCIf e:cs) = eval e >>= \cond ->
  if truthy cond then buildCompr out cs else return [] -- stop generating!


eval :: Exp -> Comp Value
eval (Const val) = return val
eval (Var v)     = look v

eval (Oper op e1 e2) = eval e1 >>= \e1_val -> eval e2 >>= \e2_val ->
  either (abort . EBadArg) return (operate op e1_val e2_val)

eval (Not e)       = truthy' . not . truthy <$> eval e

eval (Call f args) = mapM eval args >>= apply f

eval (List es)    = ListVal <$> mapM eval es
eval (Compr e cs) = ListVal <$> buildCompr (eval e) cs


exec :: Program -> Comp ()
exec (SDef v e:stms) = eval e >>= \e_val -> withBinding v e_val $ exec stms
exec (SExp e:stms)   = eval e >> exec stms
exec []              = return ()


execute :: Program -> ([String], Maybe RunError)
execute program = let (res, output) = runComp (exec program) []
                      status        = either Just (\_ -> Nothing) res
                  in (output, status)
