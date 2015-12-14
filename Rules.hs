module Rules (applyRules) where

import Lex

applyRules :: Cmplx -> Cmplx -> [Cmplx]
applyRules a b = forwardApplication           a b ++
                 backwardApplication          a b ++
                 forwardComposition           a b ++
                 forwardCrossingComposition   a b ++
                 backwardComposition          a b ++
                 backwardCrossingComposition  a b ++
                 forwardSubstitution          a b ++
                 forwardCrossingSubstitution  a b ++
                 backwardSubstitution         a b ++
                 backwardCrossingSubstitution a b
    where
        forwardApplication :: Cmplx -> Cmplx -> [Cmplx]
        forwardApplication (CmplxTree lhs Forw rhs) x = [lhs | rhs == x]
        forwardApplication _ _ = []

        backwardApplication :: Cmplx -> Cmplx -> [Cmplx]
        backwardApplication x (CmplxTree lhs Back rhs) = [lhs | rhs == x]
        backwardApplication _ _ = []

        forwardComposition :: Cmplx -> Cmplx -> [Cmplx]
        forwardComposition (CmplxTree lhs Forw rhs) (CmplxTree lhs' Forw rhs') = [CmplxTree lhs Forw rhs' | rhs == lhs']
        forwardComposition _ _ = []

        forwardCrossingComposition :: Cmplx -> Cmplx -> [Cmplx]
        forwardCrossingComposition (CmplxTree lhs Forw rhs) (CmplxTree lhs' Back rhs') = [CmplxTree lhs Back rhs' | rhs == lhs']
        forwardCrossingComposition _ _ = []

        backwardComposition :: Cmplx -> Cmplx -> [Cmplx]
        backwardComposition (CmplxTree lhs Back rhs) (CmplxTree lhs' Back rhs') = [CmplxTree lhs' Back rhs | lhs == rhs']
        backwardComposition _ _ = []

        backwardCrossingComposition :: Cmplx -> Cmplx -> [Cmplx]
        backwardCrossingComposition (CmplxTree lhs Forw rhs) (CmplxTree lhs' Back rhs') = [CmplxTree lhs' Forw rhs | lhs == rhs']
        backwardCrossingComposition _ _ = []

        forwardSubstitution :: Cmplx -> Cmplx -> [Cmplx]
        forwardSubstitution (CmplxTree (CmplxTree lhs'' Forw rhs'') Forw _) (CmplxTree lhs' Forw rhs') = [CmplxTree lhs'' Forw rhs' | rhs'' == lhs']
        forwardSubstitution _ _ = []

        forwardCrossingSubstitution :: Cmplx -> Cmplx -> [Cmplx]
        forwardCrossingSubstitution (CmplxTree (CmplxTree lhs'' Forw rhs'') Back _) (CmplxTree lhs' Back rhs') = [CmplxTree lhs'' Back rhs' | rhs'' == lhs']
        forwardCrossingSubstitution _ _ = []

        backwardSubstitution :: Cmplx -> Cmplx -> [Cmplx]
        backwardSubstitution (CmplxTree lhs Back rhs) (CmplxTree (CmplxTree lhs'' Back rhs'') Back _) = [CmplxTree lhs'' Back rhs | lhs == rhs'']
        backwardSubstitution _ _ = []

        backwardCrossingSubstitution :: Cmplx -> Cmplx -> [Cmplx]
        backwardCrossingSubstitution (CmplxTree lhs Forw rhs) (CmplxTree (CmplxTree lhs'' Back rhs'') Forw _) = [CmplxTree lhs'' Forw rhs | lhs == rhs'']
        backwardCrossingSubstitution _ _ = []