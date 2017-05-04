module Main where

import Data.Array

data Instruction = Z Int | S Int | M Int Int | J Int Int Int | E deriving (Show, Eq)

type Register = Array Int Int

main :: IO ()
main = do
  let x = 5 :: Int
      y = 3 :: Int
      resultRegister = ranRAM 1 $ array (0,10) (zip [0..10] (0 : x : y : replicate 8 0))

  putStrLn "Index  Value"
  putStrLn $ showRegister resultRegister

showRegister :: Register -> String
showRegister register = unlines [show idx ++ " " ++ show value | (idx, value) <- assocs register]

-- １オリジン
instructions :: [Instruction]
instructions = [ M 3 2
               , J 1 3 6
               , S 0
               , S 3
               , J 0 0 2
               , E
               ]

ranRAM :: Int -> Register -> Register
ranRAM pc register
  | instruct == E = register
  | otherwise = let (newPC, newRegister) = manipulate instruct pc register in ranRAM newPC newRegister
  where instruct = instructions !! (pc - 1) :: Instruction

-- registerは０オリジン
manipulate :: Instruction -> Int -> Register -> (Int, Register)
manipulate (Z n) pc register = (pc + 1, register // [(n, 0)])
manipulate (S n) pc register = (pc + 1, register // [(n, (register ! n) + 1 )])
manipulate (M toCopy fromCopy) pc register = (pc + 1, register // [(toCopy, register ! fromCopy)])
manipulate (J n1 n2 jump) pc register = if (register ! n1) == (register ! n2) then (jump, register) else (pc + 1, register)
