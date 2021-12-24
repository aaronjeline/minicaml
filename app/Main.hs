{-# Language DuplicateRecordFields, OverloadedStrings #-}
module Main where

import Lib
import Lang

ex = Int 3
ex2 = Arith Add [Int 3, Int 4]
ex2' = Arith Add [Int 3, Bool False]
ex3 = Letrec [ Def { name = Name "f", params = [Name "x"], body = Arith Add [Variable (Name "x"), Int 4] } ] (Variable (Name "f"))
ex3' = Letrec [ Def { name = Name "five", params = [Name "x"], body = Int 5 } ] ((Variable (Name "five")) :@ [Int 3])
ex3'' = Letrec [ Def { name = Name "id", params = [Name "x"], body = Variable $ Name "x" } ] ((Variable (Name "id")) :@ [Int 3])
ex4 = Letrec [ Def { name = Name "f", params = [Name "x"], body = Arith Add [Variable (Name "x"), Int 4] } ] 
        ((Variable $ Name "f") :@ [Int 3])

ex5 = 
    Letrec [ Def 
        { name = Name "fact"
        , params = [Name "x"]
        , body = If (Arith Eq [Variable (Name "x"), Int 0])
                    (Int 1)
                    (Arith Mult [Variable (Name "x"), 
                    (Variable (Name "fact"))
                      :@  [Arith Add [Variable (Name "x"), Int (-1)]]])}]
                        ((Variable (Name "fact")) :@ [Int 5])
        

ex6 = Lam ["x"] (Variable "x") :@ [Int 3]
ex7 = (Lam ["x"] (Lam ["y"] (Variable "x")) :@ [Int 3]) :@ [Int 4]

main :: IO () 
main = putStr "hello"
