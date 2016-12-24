# totes ( name tbc )
A total dependently typed functional programming language

```
bool : data
bool = ( a : data ) -> a -> a -> a

true : bool
true = (\x y z -> y)

false : bool
false = (\x y z -> y)

if : bool -> ( a : data ) -> a -> a -> a
if = (\b -> b)



```
