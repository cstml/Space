module Examples.ExampleTerms where

import Space.Syntax.Types

{- | Term equivalent to:

{- let's bind 1 to x -}
type x : Int
[1]@_ <x>@_

{- then evaluate x -}
x

{- and evaluate x in a closure -}
{x}

{- let's push a push term -}
[[x]@_]@_

type y : ({Int}@_ -> {Int,Int}@_)

y 
-}
exampleTerm1 =
  [ TypeExp ( "x" , TypeVar "Int")
  , TermExp $ Push Spine (Variable "1" NoOp) $ Bind Spine "x"
    $ Variable "x" 
    $ Closure (Variable  "x" NoOp) 
    $ Push Spine (Push Spine (Variable "x" NoOp) NoOp)
    $ NoOp
  , TypeExp ( "y", TypeLocation Spine (TypeVector [TypeVar "Int"])
              :->: TypeLocation Spine (TypeVector [TypeVar "Int", TypeVar "Int"]))
  , TermExp $ Variable "y" $ NoOp 
  ]
