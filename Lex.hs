module Lex (Prim(..),
            Slash(..),
            Cmplx(..),
            Lexicon,
            createLexicon) where

import qualified Data.Map as Map

data Prim  = N | NP | S deriving (Eq, Show, Ord)
data Slash = Forw | Back deriving (Eq, Show, Ord)
data Cmplx  = CmplxLeaf Prim |
             CmplxTree Cmplx Slash Cmplx deriving (Eq, Show, Ord)

type Lexicon = Map.Map String [Cmplx]

createLexicon :: [(String, [Cmplx])] -> Lexicon
createLexicon = Map.fromList