{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.MemoTrie
import GHC.Generics (Generic)

data Color
    = RGB Int Int Int
    | NamedColor String
    deriving (Generic)

instance HasTrie Color where
    newtype Color :->: b = ColorTrie {unColorTrie :: Reg Color :->: b}
    trie = trieGeneric ColorTrie
    untrie = untrieGeneric unColorTrie
    enumerate = enumerateGeneric unColorTrie

runColor :: Color -> Int
runColor (RGB r g b) = r + g + b
runColor (NamedColor _) = length [1 .. 10000000000 :: Int]

runColorMemoized :: Color -> Int
runColorMemoized = memo runColor

main :: IO ()
main = do
    -- putStrLn "first call (should take a few seconds): "
    -- print $ runColorMemoized (NamedColor "")
    -- putStrLn "cached call (should be instantaneous): "
    -- print $ runColorMemoized (NamedColor "")
    putStrLn "first call (should take a few seconds): "
    print $ fibNaive 42
    putStrLn "cached call (should be instantaneous): "
    print $ fibMemo 42

fibNaive :: Integer -> Integer
fibNaive 0 = 0
fibNaive 1 = 1
fibNaive n = fibNaive (n - 1) + fibNaive (n - 2)

fibFix :: (Integer -> Integer) -> Integer -> Integer
fibFix _ 0 = 0
fibFix _ 1 = 1
fibFix next n = next (n - 1) + next (n - 2)

fibMemo :: Integer -> Integer
fibMemo = memoFix fibFix
