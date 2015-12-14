module Lex (Prim(..),
            Slash(..),
            Cmplx(..),
            Lexicon,
            createLexicon) where

import qualified Data.Map as Map

data Prim  = N | NP | S deriving (Eq, Show, Ord)
data Slash = Forw | Back deriving (Eq, Ord)

instance Show Slash where
    show Forw = "/"
    show Back = "\\"

data Cmplx = CmplxLeaf Prim |
             CmplxTree Cmplx Slash Cmplx deriving (Eq, Ord)

instance Show Cmplx where
    show (CmplxLeaf a)     = show a
    show (CmplxTree a b c) = "(" ++ show a ++ show b ++ show c ++ ")"

type Lexicon = Map.Map String [Cmplx]

createLexicon :: [(String, [Cmplx])] -> Lexicon
createLexicon = Map.fromList