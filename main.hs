module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let a = read $ args !! 0
  let b = read $ args !! 1
  let c = a + b
  putStrLn ("Sum: " ++ show a ++ " + " ++ show b ++ " = " ++ show c)
  name <- getLine
  putStrLn ("Hello " ++ name)
