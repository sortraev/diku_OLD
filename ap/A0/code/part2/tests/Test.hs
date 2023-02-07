import EvalSimpleTests
import EvalFullTests
import EvalErrTests

main :: IO ()
main = do
  putStrLn ""
  EvalSimpleTests.run
  EvalFullTests.run
  EvalErrTests.run
