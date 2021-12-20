{-# Language DuplicateRecordFields, OverloadedStrings #-}
module Main where

import Lib
import Lang

ex = Int 3
ex2 = Arith Add [Int 3, Int 4]
ex2' = Arith Add [Int 3, Bool False]
ex3 = Letrec [ Def { name = Name "f", params = [Name "x"], body = Arith Add [Variable (Name "x"), Int 4] } ] (Variable (Name "f"))
ex4 = Letrec [ Def { name = Name "f", params = [Name "x"], body = Arith Add [Variable (Name "x"), Int 4] } ] 
        (App (Variable $ Name "f") [Int 3])

ex5 = 
    Letrec [ Def 
        { name = Name "fact"
        , params = [Name "x"]
        , body = If (Arith Eq [Variable (Name "x"), Int 0])
                    (Int 1)
                    (Arith Mult [Variable (Name "x"), App (Variable (Name "fact"))
                        [Arith Add [Variable (Name "x"), Int (-1)]]])}]
                        (Variable (Name "fact"))
        

main :: IO ()
main = someFunc
