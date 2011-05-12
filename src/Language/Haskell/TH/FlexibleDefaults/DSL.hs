{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Language.Haskell.TH.FlexibleDefaults.DSL where

import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Writer
import Data.List
import Data.Monoid
import qualified Data.Map as M
import Data.Ord
import qualified Data.Set as S
import Language.Haskell.TH
import Language.Haskell.TH.FlexibleDefaults.Solve

-- newtype wrapper for Problem, because the default implementation of Monoid
-- (@mappend = union@) is not the one we want here; we want
-- @mappend = unionWith mappend@
newtype Impls s = Impls { unImpls :: M.Map String [ImplSpec s] }

instance Functor Impls where
    fmap f (Impls m) = Impls (M.map (map (fmap f)) m)

instance Monoid (Impls s) where
    mempty = Impls mempty
    mappend (Impls x) (Impls y) = Impls (M.unionWith mappend x y)
    
newtype Defaults s a = Defaults { unDefaults :: Writer (Impls s) a }
    deriving (Functor, Applicative, Monad)


addImplSpecs :: String -> [ImplSpec s] -> Defaults s ()
addImplSpecs f = Defaults . tell . Impls . M.singleton f

addImplSpec :: String -> ImplSpec s -> Defaults s ()
addImplSpec f = addImplSpecs f . (:[])

toProblem :: (Ord s, Monoid s) => Defaults s () -> Problem s
toProblem
    = fmap (sortBy (flip (comparing scoreImplSpec)))
    . unImpls 
    . snd 
    . runWriter 
    . unDefaults

-- |Map a function over all scores.  This function's name comes from the
-- following idiom (where 'Sum' is replaced by whatever monoid-constructor
-- you want to use to combine scores):
-- 
-- > foo = scoreBy Sum $ do
-- >    ...
scoreBy :: (a -> b) -> Defaults a t -> Defaults b t
scoreBy f = Defaults . mapWriterT (fmap (fmap (fmap f))) . unDefaults

newtype Function s a = Function (ReaderT String (Defaults s) a)
    deriving (Functor, Applicative, Monad)

-- |Declare a function that must be implemented, and provide a description
-- of any default implementations which can be used.
function :: String -> Function s a -> Defaults s a
function f (Function x) = do
    requireFunction f
    runReaderT x f

-- |State that a function must be implemented but has no default implementation.
requireFunction :: String -> Defaults s ()
requireFunction f = addImplSpecs f []

newtype Implementation s a = Implementation (State (Maybe s, S.Set String, Maybe InlineSpec) a)
    deriving (Functor, Applicative, Monad)

-- |Describe a default implementation of the current function
implementation :: Implementation s (Q [Dec]) -> Function s ()
implementation (Implementation x) = case runState x (Nothing, S.empty, Nothing) of
    (dec, (s, deps, inl)) -> Function $ do
        fName <- ask
        lift (addImplSpec fName (ImplSpec s deps (applyInline fName inl dec)))

applyInline :: String -> Maybe InlineSpec -> Q [Dec] -> Q [Dec]
applyInline _ Nothing       = id
applyInline n (Just inl)    = fmap (PragmaD (InlineP (mkName n) inl) :)

-- |Specify the score associated with the current implementation.  Only one 
-- invocation of either 'score' or 'cost' may be used per implementation.
score :: s -> Implementation s ()
score s = Implementation $ do
    (oldS, deps, inl) <- get
    case oldS of
        Nothing -> put (Just s, deps, inl)
        Just _  -> fail "score: score was already set"

-- |Specify the cost (negated score) associated with the current implementation.
-- Only one invocation of either 'score' or 'cost' may be used per implementation.
cost :: Num s => s -> Implementation s ()
cost = score . negate

-- |Specify that the current implementation must not be used unless the given
-- function is already defined.  If this implementation can be used
-- mutually-recursively with _ALL_ potential implementations of some other
-- function, then a dependency need not be declared on that function.
dependsOn :: String -> Implementation s ()
dependsOn dep = Implementation $ do
    (s, deps, inl) <- get
    put (s, S.insert dep deps, inl)

setInline :: InlineSpec -> Implementation s ()
setInline inl = Implementation $ do
    (s, deps, _) <- get
    put (s, deps, Just inl)

inline :: Implementation s ()
inline = setInline (InlineSpec True False Nothing)

noinline :: Implementation s ()
noinline = setInline (InlineSpec False False Nothing)
