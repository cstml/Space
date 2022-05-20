[1];<x>;
-- x  := 1

[1];<xx>;
-- xx := 1

[10];<y>;
-- y := 10
  
[[x]@O; x; <i> ; x; xx ; +; !; <x>; i ; !; <xx>;];<a>;
-- a := print x . i = x . x = xx + x . xx = i . evaluate z 
  
[[x]@O];<b>;
-- b := print x
  
[ [a;z];
  [b];
  [1;y;-;!;<y>;y;0;==];
  if;
];<z>;
{- z := if (y := y - 1 . y == 0)
        then (b)
        else (a)
-}

z;
-- evaluate z 
