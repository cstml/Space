1;       -- push 1 -> stack: 1
2;       -- push 2 -> stack: 2 | 1
+;       -- add    -> stack: 3
1;       -- push 1 -> stack: 1 | 3
2;       -- push 2 -> stack: 2 | 1 | 3
+;       -- add    -> stack: 3 | 3
+;       -- add    -> stack: 6
<x>;     -- bind x -> stack: *
         --        -> x : 6
@Ou[x];  -- push to output x ~> 6
