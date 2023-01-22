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

<y>@_
