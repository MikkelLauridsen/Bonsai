module Prettifier (prettify) where

import Ast

prettify :: Either String ProgAST -> String
prettify (Left err) = err
prettify (Right ast) = prettify_aux (show ast) 0

space indent_count = take (4 * indent_count) (cycle ":   ")

prettify_aux :: String -> Int -> [Char]
prettify_aux [] _ = []
prettify_aux ('(':xs) indent_count = start_paran "(" xs indent_count
prettify_aux ('[':xs) indent_count = start_paran "[" xs indent_count
prettify_aux (')':xs) indent_count = end_paren ")" xs indent_count
prettify_aux (']':xs) indent_count = end_paren "]" xs indent_count
prettify_aux (x:xs) indent_count = x:(prettify_aux xs indent_count)

start_paran char tail' indent_count = char ++ (head (words tail')) ++ "\n" ++ (space (indent_count + 1)) ++ drop 1 (prettify_aux (drop (length (head (words tail'))) tail') (indent_count + 1))
end_paren char tail' indent_count = "\n" ++ space (indent_count - 1) ++ char ++ (prettify_aux tail' (indent_count - 1))