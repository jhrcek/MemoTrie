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
main =
    do
        putStrLn "first call (should take a few seconds): "
        print $ runColorMemoized (NamedColor "")
        putStrLn "cached call (should be instantaneous): "
        print $ runColorMemoized (NamedColor "")
