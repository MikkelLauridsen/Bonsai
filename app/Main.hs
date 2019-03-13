data MyType = A Int | B MyType deriving (Show)

getEnd (A v) = v
getEnd (B t) = getEnd (t)


addList l = foldl (+) 0 l

uglyfactorial x = foldl (*) 1 [1..x]

main = print (getEnd (B (B (A 10))))