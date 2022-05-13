[1;+;*]; -- pushes +1 to the stack
<x>;     -- binds it from the stack to x
[2;+;*]; -- pushes +2 to the stack
<y>;     -- ...
3;x;y;   -- pushes 3 and does the +1 +2 

{- same thing again but bound to z -}
[[1;+;*]; -- pushes +1 to the stack
<x>;      -- binds it from the stack to x
[2;+;*];  -- pushes +2 to the stack
<y>;      -- ...
3;x;y;*]; -- pushes 3 and does the +1 +2 
<z>;   
