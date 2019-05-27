 ![](https://i.imgur.com/CmcWYtf.png)

Bonsai is a simple clean functional programming language inspired by Haskell, Clean, SML and GCL. The purpose of Bonsai is to provide simple implementation of compilers and interpreters. Bonsai was created by group d406f19 as a part of the 4th semester of the computer science bachelor on Aalborg University.
# Features
   - Pattern matching
   - Algebraic datatypes
   - Uniqueness-type based I/O
   - Selection by guarded commands
   - Higher-order functions
   - Constraint-based type inference
# Installation
1. install stack and intero for Haskell
2. compile with stack
# Snippets
### Global variable declarations:
```Haskell
var x = 10
var y::[Int] = [x, 43, 98, 0x11]
var z = (true, 10.3, 0xB8, "string")
```
### Type declarations:
```Haskell
type Maybe<a> = {
    | Just a
    | Nothing
}

type Map<k, v> = {
    | Map (k, v, Map, Map)
    | Nil
}

type WeekDays = {
    | Monday
    | Tuesday
    | Wednesday
    | Thursday
    | Friday
    | Saturday
    | Sunday
}
```
### Pattern matching:
```Haskell
var foo = match Just ('5', true) {
    | Just ('a', ?)    -> "a"
    | Just ('b', true) -> "b and true"
    | Just ?           -> "some pair"
    | Nothing          -> "nothing"
}

var bar = match [1, 2, 3, 4] {
    | [1, 2, 3, 4] -> 1 + 2 + 3 + 4
    | [1, 2, 3, ?] -> 1 + 2 + 3
    | (1:xs)       -> 1
    | (x:xs)       -> x
    | []           -> 0
}
```
### Functions:
```Haskell
var fun = x => {
    y => {
        x * y
    }
}

var fun2 = fun 5
```
### Case and let-in:
```Haskell
var res = case {
    | 1 == 0 -> "goodbye all reasoning"
    | false  -> "false"
    | ?      -> "wildcard!"
}

var factorial = n => {
    case {
        n <= 1 -> 1
        ?      -> n * (factorial (n - 1))
    }
}

var nested = let fun = n => {
    case {
        n <= 1 -> 1
        ?      -> n * (factorial (n - 1))
    }
} in (fun 10)
```
### I/O and uniqueness types:
```Haskell
var main = sys => {
    let (?, stdout') = writes (show (Just [5, 3, 2])) stdout
    in (writes "hello world!" stdout') # use the new file* value!
}

var main = sys => {
    let (succ, c, ?) = read stdin
    in (case {
        | succ -> writes
        match c {
            | 'y' -> "you said yes!"
            | ?   -> "I am disappointed in you :("
        } stdout
        | ?    -> writes "an error occured" stdout
    })
}

var writes = s => {
    f => {
        match s {
            | []     -> (true, f)
            | (c:cs) -> 
                let (succ, f') = write c f 
                in (case {
                    | succ == true -> (writes cs f')
                    | ?            -> (false, f') 
                })
        }
    }
}
```
