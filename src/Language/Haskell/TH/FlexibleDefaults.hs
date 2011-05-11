-- |A code-generation system for complex typeclass default-implementation configurations.
module Language.Haskell.TH.FlexibleDefaults
    ( Defaults
    , scoreBy
    , Function
    , function
    , requireFunction
    , Implementation
    , implementation
    , score
    , cost
    , dependsOn
    
    , withDefaults
    , implementDefaults
    ) where

import Data.List
import Data.Monoid 
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S
import Language.Haskell.TH
import Language.Haskell.TH.FlexibleDefaults.DSL
import Language.Haskell.TH.FlexibleDefaults.Solve

declaredValueNames :: Dec -> [Name]
declaredValueNames (FunD n _)    = [n]
declaredValueNames (ValD p _ _)  = matchedNames p
declaredValueNames _ = []

matchedNames :: Pat -> [Name]
matchedNames (VarP n)           = [n]
matchedNames (TupP ps)          = concatMap matchedNames ps
matchedNames (InfixP p1 _ p2)   = matchedNames p1 ++ matchedNames p2
matchedNames (TildeP p)         = matchedNames p
matchedNames (BangP p)          = matchedNames p
matchedNames (AsP n p)          = n : matchedNames p
matchedNames (RecP _ fs)        = concatMap (matchedNames . snd) fs
matchedNames (ListP ps)         = concatMap matchedNames ps
matchedNames (SigP p _)         = matchedNames p
matchedNames (ViewP _ p)        = matchedNames p
matchedNames _                  = []

deleteKeys :: Ord k => S.Set k -> M.Map k v -> M.Map k v
deleteKeys ks m = m M.\\ M.fromDistinctAscList [(k,()) | k <- S.toAscList ks]

-- |Given a partial list of function declarations, complete that list based on
-- the 'Defaults' specification given.
implementDefaults :: (Ord s, Monoid s) => Defaults s () -> [Dec] -> Q [Dec]
implementDefaults defs decs = do
    let prob = toProblem defs
        
        implemented = S.fromList (map nameBase (concatMap declaredValueNames decs))
        unimplemented = deleteKeys implemented prob
        
        solutions = chooseImplementations unimplemented
    
    implementations <- case solutions of
        []  -> fail "implementDefaults: incomplete set of basis functions"
        ss  -> 
            let best = maximumBy (comparing scoreSolution) ss
             in sequence [ decQ | ImplSpec _ _ decQ <- M.elems best]
    
    return (decs ++ concat implementations)

-- TODO: maybe make this accept multiple instance declarations, and/or pass non-instance Dec's unmodified.
-- Or even accept something like "M.Map String (exists s. Defaults s)" to support
-- many different instance decls, choosing the 'Defaults' spec by class name.

-- |Given a @Q [Dec]@ containing an instance declaration, complete that instance
-- declaration using the given 'Defaults' specification.  Typical usage would be
-- along the lines of the following:
--
-- > $(withDefaults fooDefaults [d| instance Foo t where {- ... -} |])
withDefaults :: (Monoid s, Ord s) => Defaults s () -> Q [Dec] -> Q [Dec]
withDefaults defs decQ = do
    dec <- decQ
    
    case dec of
        [InstanceD clsCxt cls decs] -> do
            impl <- implementDefaults defs decs
            return [InstanceD clsCxt cls impl]
        
        _ -> fail "withDefaults: second parameter should be a single instance declaration"

