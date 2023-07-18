{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fenable-rewrite-rules #-}

-- ScopedTypeVariables works around a 6.10 bug.  The forall keyword is
-- supposed to be recognized in a RULES pragma.

----------------------------------------------------------------------

----------------------------------------------------------------------

{- |
Module      :  Data.MemoTrie
Copyright   :  (c) Conal Elliott 2008-2016
License     :  BSD3

Maintainer  :  conal@conal.net
Stability   :  experimental

Trie-based memoizer

Adapted from sjanssen's paste: <http://hpaste.org/3839 \"a lazy trie\">,
which I think is based on Ralf Hinze's paper "Memo Functions,
Polytypically!".

You can automatically derive generic instances. for example:

@
{\-# LANGUAGE <https://ocharles.org.uk/blog/posts/2014-12-16-derive-generic.html DeriveGeneric>, TypeOperators, TypeFamilies #-\}
import Data.MemoTrie
import GHC.Generics (Generic)

data Color = RGB Int Int Int
           | NamedColor String
 deriving ('Generic')

instance HasTrie Color where
  newtype (Color :->: b) = ColorTrie { unColorTrie :: 'Reg' Color :->: b }
  trie = 'trieGeneric' ColorTrie
  untrie = 'untrieGeneric' unColorTrie
  enumerate = 'enumerateGeneric' unColorTrie
@

see @examples/Generic.hs@, which can be run with:

@
cabal configure -fexamples && cabal run generic
@
-}
module Data.MemoTrie
    ( HasTrie (..)
    , (:->:) (..)
    , domain
    , idTrie
    , (@.@)
    -- , trie2, trie3, untrie2, untrie3
    , memo
    , memo2
    , memo3
    , mup
    , inTrie
    , inTrie2
    , inTrie3
    , trieGeneric
    , untrieGeneric
    , enumerateGeneric
    , Reg
    , memoFix
    ) where

import Control.Arrow (first, (&&&))
import Data.Bits (Bits (shiftL, shiftR, testBit, (.|.)))
import Data.Bool (bool)
import Data.Function (fix, on)
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Kind (Type)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word64, Word8)
import GHC.Generics
    ( Generic (..)
    , K1 (K1)
    , M1 (M1)
    , U1 (U1)
    , V1
    , type (:*:) ((:*:))
    , type (:+:) (L1, R1)
    )

infixr 0 :->:

-- | Mapping from all elements of @a@ to the results of some function
class HasTrie a where
    -- | Representation of trie with domain type @a@
    data (:->:) a :: Type -> Type

    -- | Create the trie for the entire domain of a function
    trie :: (a -> b) -> (a :->: b)

    -- | Convert a trie to a function, i.e., access a field of the trie
    untrie :: (a :->: b) -> (a -> b)

    -- | List the trie elements.  Order of keys (@:: a@) is always the same.
    enumerate :: (a :->: b) -> [(a, b)]

-- | Domain elements of a trie
domain :: HasTrie a => [a]
domain = map fst (enumerate (trie (const oops)))
  where
    oops = error "Data.MemoTrie.domain: range element evaluated."

-- Hm: domain :: [Bool] doesn't produce any output.

instance (HasTrie a, Eq b) => Eq (a :->: b) where
    (==) = (==) `on` (map snd . enumerate)

instance (HasTrie a, Show a, Show b) => Show (a :->: b) where
    show t = "Trie: " ++ show (enumerate t)

-- | Trie-based function memoizer
memo :: HasTrie t => (t -> a) -> (t -> a)
memo = untrie . trie

{- | Memoize a binary function, on its first argument and then on its
second.  Take care to exploit any partial evaluation.
-}
memo2 :: (HasTrie s, HasTrie t) => (s -> t -> a) -> (s -> t -> a)
memo2 = mup memo

{- | Memoize a ternary function on successive arguments.  Take care to
exploit any partial evaluation.
-}
memo3 :: (HasTrie r, HasTrie s, HasTrie t) => (r -> s -> t -> a) -> (r -> s -> t -> a)
memo3 = mup memo2

-- | Lift a memoizer to work with one more argument.
mup :: HasTrie t => (b -> c) -> (t -> b) -> (t -> c)
mup mem f = memo (mem . f)

-- | Memoizing recursion. Use like `fix`.
memoFix :: HasTrie a => ((a -> b) -> (a -> b)) -> (a -> b)
memoFix h = fix (memo . h)

-- | Apply a unary function inside of a trie
inTrie ::
    (HasTrie a, HasTrie c) =>
    ((a -> b) -> (c -> d)) ->
    ((a :->: b) -> (c :->: d))
inTrie = untrie ~> trie

-- | Apply a binary function inside of a trie
inTrie2 ::
    (HasTrie a, HasTrie c, HasTrie e) =>
    ((a -> b) -> (c -> d) -> (e -> f)) ->
    ((a :->: b) -> (c :->: d) -> (e :->: f))
inTrie2 = untrie ~> inTrie

-- | Apply a ternary function inside of a trie
inTrie3 ::
    (HasTrie a, HasTrie c, HasTrie e, HasTrie g) =>
    ((a -> b) -> (c -> d) -> (e -> f) -> (g -> h)) ->
    ((a :->: b) -> (c :->: d) -> (e :->: f) -> (g :->: h))
inTrie3 = untrie ~> inTrie2

instance HasTrie Void where
    data Void :->: a = VoidTrie
    trie _ = VoidTrie
    untrie VoidTrie = \case {}
    enumerate VoidTrie = []

instance HasTrie () where
    newtype () :->: a = UnitTrie a
    trie f = UnitTrie (f ())
    untrie (UnitTrie a) () = a
    enumerate (UnitTrie a) = [((), a)]

-- Proofs of inverse properties:

{-
    untrie (trie f)
      == { trie def }
    untrie (UnitTrie (f ()))
      == { untrie def }
    \ () -> (f ())
      == { const-unit }
    f

    trie (untrie (UnitTrie a))
      == { untrie def }
    trie (\ () -> a)
      == { trie def }
    UnitTrie ((\ () -> a) ())
      == { beta-reduction }
    UnitTrie a

Oops -- the last step of the first direction is bogus when f is non-strict.
Can be fixed by using @const a@ in place of @\ () -> a@, but I can't do
the same for other types, like integers or sums.

All of these proofs have this same bug, unless we restrict ourselves to
memoizing hyper-strict functions.

-}

instance HasTrie Bool where
    data Bool :->: x = BoolTrie x x
    trie f = BoolTrie (f False) (f True)
    untrie (BoolTrie f t) = bool f t
    enumerate (BoolTrie f t) = [(False, f), (True, t)]

{-
    untrie (trie f)
      == { trie def }
    untrie (BoolTrie (f False) (f True))
      == { untrie def }
    if' (f False) (f True)
      == { if' spec }
    f

    trie (untrie (BoolTrie f t))
      == { untrie def }
    trie (if' f t)
      == { trie def }
    BoolTrie (if' f t False) (if' f t True)
      == { if' spec }
    BoolTrie f t
-}

instance HasTrie a => HasTrie (Maybe a) where
    data Maybe a :->: b = MaybeTrie b (a :->: b)
    trie f = MaybeTrie (f Nothing) (trie (f . Just))
    untrie (MaybeTrie nothing_val a_trie) = maybe nothing_val (untrie a_trie)
    enumerate (MaybeTrie nothing_val a_trie) = (Nothing, nothing_val) : enum' Just a_trie

instance (HasTrie a, HasTrie b) => HasTrie (Either a b) where
    data Either a b :->: x = EitherTrie (a :->: x) (b :->: x)
    trie f = EitherTrie (trie (f . Left)) (trie (f . Right))
    untrie (EitherTrie s t) = either (untrie s) (untrie t)
    enumerate (EitherTrie s t) = enum' Left s `weave` enum' Right t

enum' :: HasTrie a => (a -> a') -> (a :->: b) -> [(a', b)]
enum' f = (fmap . first) f . enumerate

weave :: [a] -> [a] -> [a]
[] `weave` as = as
as `weave` [] = as
(a : as) `weave` bs = a : (bs `weave` as)

{-
    untrie (trie f)
       == { trie def }
    untrie (EitherTrie (trie (f . Left)) (trie (f . Right)))
       == { untrie def }
    either (untrie (trie (f . Left))) (untrie (trie (f . Right)))
       == { untrie . trie }
    either (f . Left) (f . Right)
       == { either }
    f

    trie (untrie (EitherTrie s t))
       == { untrie def }
    trie (either (untrie s) (untrie t))
       == { trie def }
    EitherTrie (trie (either (untrie s) (untrie t) . Left))
               (trie (either (untrie s) (untrie t) . Right))
       == { either }
    EitherTrie (trie (untrie s)) (trie (untrie t))
       == { trie . untrie }
    EitherTrie s t
-}

instance (HasTrie a, HasTrie b) => HasTrie (a, b) where
    newtype (a, b) :->: x = PairTrie (a :->: (b :->: x))
    trie f = PairTrie (trie (trie . curry f))
    untrie (PairTrie t) = uncurry (untrie . untrie t)
    enumerate (PairTrie tt) =
        [((a, b), x) | (a, t) <- enumerate tt, (b, x) <- enumerate t]

{-
    untrie (trie f)
      == { trie def }
    untrie (PairTrie (trie (trie . curry f)))
      == { untrie def }
    uncurry (untrie . untrie (trie (trie . curry f)))
      == { untrie . trie }
    uncurry (untrie . trie . curry f)
      == { untrie . untrie }
    uncurry (curry f)
      == { uncurry . curry }
    f

    trie (untrie (PairTrie t))
      == { untrie def }
    trie (uncurry (untrie .  untrie t))
      == { trie def }
    PairTrie (trie (trie . curry (uncurry (untrie .  untrie t))))
      == { curry . uncurry }
    PairTrie (trie (trie . untrie .  untrie t))
      == { trie . untrie }
    PairTrie (trie (untrie t))
      == { trie . untrie }
    PairTrie t
-}

instance (HasTrie a, HasTrie b, HasTrie c) => HasTrie (a, b, c) where
    newtype (a, b, c) :->: x = TripleTrie (((a, b), c) :->: x)
    trie f = TripleTrie (trie (f . trip))
    untrie (TripleTrie t) = untrie t . detrip
    enumerate (TripleTrie t) = enum' trip t

trip :: ((a, b), c) -> (a, b, c)
trip ((a, b), c) = (a, b, c)

detrip :: (a, b, c) -> ((a, b), c)
detrip (a, b, c) = ((a, b), c)

instance HasTrie x => HasTrie [x] where
    newtype [x] :->: a = ListTrie (Either () (x, [x]) :->: a)
    trie f = ListTrie (trie (f . list))
    untrie (ListTrie t) = untrie t . delist
    enumerate (ListTrie t) = enum' list t

list :: Either () (x, [x]) -> [x]
list = either (const []) (uncurry (:))

delist :: [x] -> Either () (x, [x])
delist [] = Left ()
delist (x : xs) = Right (x, xs)

instance HasTrie Word where
    newtype Word :->: a = WordTrie ([Bool] :->: a)
    trie f = WordTrie (trie (f . unbits))
    untrie (WordTrie t) = untrie t . bits
    enumerate (WordTrie t) = enum' unbits t

instance HasTrie Word8 where
    newtype Word8 :->: a = Word8Trie ([Bool] :->: a)
    trie f = Word8Trie (trie (f . unbits))
    untrie (Word8Trie t) = untrie t . bits
    enumerate (Word8Trie t) = enum' unbits t

instance HasTrie Word16 where
    newtype Word16 :->: a = Word16Trie ([Bool] :->: a)
    trie f = Word16Trie (trie (f . unbits))
    untrie (Word16Trie t) = untrie t . bits
    enumerate (Word16Trie t) = enum' unbits t

instance HasTrie Word32 where
    newtype Word32 :->: a = Word32Trie ([Bool] :->: a)
    trie f = Word32Trie (trie (f . unbits))
    untrie (Word32Trie t) = untrie t . bits
    enumerate (Word32Trie t) = enum' unbits t

instance HasTrie Word64 where
    newtype Word64 :->: a = Word64Trie ([Bool] :->: a)
    trie f = Word64Trie (trie (f . unbits))
    untrie (Word64Trie t) = untrie t . bits
    enumerate (Word64Trie t) = enum' unbits t

-- instance HasTrie Word where
--   newtype Word :->: a = WordTrie ([Bool] :->: a)
--   trie f = WordTrie (trie (f . unbits))
--   untrie (WordTrie t) = untrie t . bits
--   enumerate (WordTrie t) = enum' unbits t

-- | Extract bits in little-endian order
bits :: (Num t, Bits t) => t -> [Bool]
bits 0 = []
bits x = testBit x 0 : bits (shiftR x 1)

-- | Convert boolean to 0 (False) or 1 (True)
unbit :: Num t => Bool -> t
unbit False = 0
unbit True = 1

-- | Bit list to value
unbits :: (Num t, Bits t) => [Bool] -> t
unbits [] = 0
unbits (x : xs) = unbit x .|. shiftL (unbits xs) 1

instance HasTrie Char where
    newtype Char :->: a = CharTrie (Int :->: a)
    untrie (CharTrie t) n = untrie t (fromEnum n)
    trie f = CharTrie (trie (f . toEnum))
    enumerate (CharTrie t) = enum' toEnum t

-- Although Int is a Bits instance, we can't use bits directly for
-- memoizing, because the "bits" function gives an infinite result, since
-- shiftR (-1) 1 == -1.  Instead, convert between Int and Word, and use
-- a Word trie.  Any Integral type can be handled similarly.

instance HasTrie Int where
    newtype Int :->: a = IntTrie (Word :->: a)
    untrie (IntTrie t) n = untrie t (fromIntegral n)
    trie f = IntTrie (trie (f . fromIntegral))
    enumerate (IntTrie t) = enum' fromIntegral t
instance HasTrie Int8 where
    newtype Int8 :->: a = Int8Trie (Word8 :->: a)
    untrie (Int8Trie t) n = untrie t (fromIntegral n)
    trie f = Int8Trie (trie (f . fromIntegral))
    enumerate (Int8Trie t) = enum' fromIntegral t
instance HasTrie Int16 where
    newtype Int16 :->: a = Int16Trie (Word16 :->: a)
    untrie (Int16Trie t) n = untrie t (fromIntegral n)
    trie f = Int16Trie (trie (f . fromIntegral))
    enumerate (Int16Trie t) = enum' fromIntegral t
instance HasTrie Int32 where
    newtype Int32 :->: a = Int32Trie (Word32 :->: a)
    untrie (Int32Trie t) n = untrie t (fromIntegral n)
    trie f = Int32Trie (trie (f . fromIntegral))
    enumerate (Int32Trie t) = enum' fromIntegral t
instance HasTrie Int64 where
    newtype Int64 :->: a = Int64Trie (Word64 :->: a)
    untrie (Int64Trie t) n = untrie t (fromIntegral n)
    trie f = Int64Trie (trie (f . fromIntegral))
    enumerate (Int64Trie t) = enum' fromIntegral t

-- For unbounded integers, we don't have a corresponding Word type, so
-- extract the sign bit.

instance HasTrie Integer where
    newtype Integer :->: a = IntegerTrie ((Bool, [Bool]) :->: a)
    trie f = IntegerTrie (trie (f . unbitsZ))
    untrie (IntegerTrie t) = untrie t . bitsZ
    enumerate (IntegerTrie t) = enum' unbitsZ t

unbitsZ :: (Num n, Bits n) => (Bool, [Bool]) -> n
unbitsZ (positive, bs) = sig (unbits bs)
  where
    sig
        | positive = id
        | otherwise = negate

bitsZ :: (Num n, Ord n, Bits n) => n -> (Bool, [Bool])
bitsZ = (>= 0) &&& (bits . abs)

-- TODO: make these definitions more systematic.

---- Instances

{-

The \"semantic function\" 'untrie' is a morphism over 'Monoid', 'Functor',
'Applicative', 'Monad', 'Category', and 'Arrow', i.e.,

  untrie mempty          == mempty
  untrie (s `mappend` t) == untrie s `mappend` untrie t

  untrie (fmap f t)      == fmap f (untrie t)

  untrie (pure a)        == pure a
  untrie (tf <*> tx)     == untrie tf <*> untrie tx

  untrie (return a)      == return a
  untrie (u >>= k)       == untrie u >>= untrie . k

  untrie id              == id
  untrie (s . t)         == untrie s . untrie t

  untrie (arr f)         == arr f
  untrie (first t)       == first (untrie t)

These morphism properties imply that all of the expected laws hold,
assuming that we interpret equality semantically (or observationally).
For instance,

  untrie (mempty `mappend` a)
    == untrie mempty `mappend` untrie a
    == mempty `mappend` untrie a
    == untrie a

  untrie (fmap f (fmap g a))
    == fmap f (untrie (fmap g a))
    == fmap f (fmap g (untrie a))
    == fmap (f.g) (untrie a)
    == untrie (fmap (f.g) a)

The implementation instances then follow from applying 'trie' to both
sides of each of these morphism laws.

-}

instance (HasTrie a, Monoid b) => Monoid (a :->: b) where
    mempty = trie mempty

instance (HasTrie a, Semigroup b) => Semigroup (a :->: b) where
    (<>) = inTrie2 (<>)

instance HasTrie a => Functor ((:->:) a) where
    fmap f = inTrie (fmap f)

instance HasTrie a => Applicative ((:->:) a) where
    pure b = trie (pure b)
    (<*>) = inTrie2 (<*>)

instance HasTrie a => Monad ((:->:) a) where
    return = pure
    u >>= k = trie (untrie u >>= untrie . k)

-- | Identity trie
idTrie :: HasTrie a => a :->: a
idTrie = trie id

infixr 9 @.@

-- | Trie composition
(@.@) ::
    (HasTrie a, HasTrie b) =>
    (b :->: c) ->
    (a :->: b) ->
    (a :->: c)
(@.@) = inTrie2 (.)

-- instance Category (:->:) where
--   id  = idTrie
--   (.) = (.:)

-- instance Arrow (:->:) where
--   arr f = trie (arr f)
--   first = inTrie first

{-

Correctness of these instances follows by applying 'untrie' to each side
of each definition and using the property @'untrie' . 'trie' == 'id'@.

The `Category` and `Arrow` instances don't quite work, however, because of
necessary but disallowed `HasTrie` constraints on the domain type.

-}

(~>) :: (a' -> a) -> (b -> b') -> ((a -> b) -> (a' -> b'))
(~>) pre post f = post . f . pre

-- | just like @void@
instance HasTrie (V1 x) where
    data V1 x :->: b = V1Trie
    trie _ = V1Trie
    untrie V1Trie = \case {}
    enumerate V1Trie = []

-- | just like @()@
instance HasTrie (U1 x) where
    newtype U1 x :->: b = U1Trie b
    trie f = U1Trie (f U1)
    untrie (U1Trie b) U1 = b
    enumerate (U1Trie b) = [(U1, b)]

-- | wraps @Either (f x) (g x)@
instance (HasTrie (f x), HasTrie (g x)) => HasTrie ((f :+: g) x) where
    newtype (f :+: g) x :->: b = EitherTrie1 (Either (f x) (g x) :->: b)
    trie f = EitherTrie1 (trie (f . liftSum))
    untrie (EitherTrie1 t) = untrie t . dropSum
    enumerate (EitherTrie1 t) = enum' liftSum t

-- | wraps @(f x, g x)@
instance (HasTrie (f x), HasTrie (g x)) => HasTrie ((f :*: g) x) where
    newtype (f :*: g) x :->: b = PairTrie1 ((f x, g x) :->: b)
    trie f = PairTrie1 (trie (f . liftProduct))
    untrie (PairTrie1 t) = untrie t . dropProduct
    enumerate (PairTrie1 t) = enum' liftProduct t

-- | wraps @a@
instance HasTrie a => HasTrie (K1 i a x) where
    newtype K1 i a x :->: b = K1Trie (a :->: b)
    trie f = K1Trie (trie (f . K1))
    untrie (K1Trie t) (K1 a) = untrie t a
    enumerate (K1Trie t) = enum' K1 t

-- | wraps @f x@
instance HasTrie (f x) => HasTrie (M1 i t f x) where
    newtype M1 i t f x :->: b = M1Trie (f x :->: b)
    trie f = M1Trie (trie (f . M1))
    untrie (M1Trie t) (M1 a) = untrie t a
    enumerate (M1Trie t) = enum' M1 t

{- | the data type in a __reg__ular form.
"unlifted" generic representation. (i.e. is a unary type constructor).
-}
type Reg a = Rep a ()

-- | 'Generic'-friendly default for 'trie'
trieGeneric ::
    (Generic a, HasTrie (Reg a)) =>
    ((Reg a :->: b) -> (a :->: b)) ->
    (a -> b) ->
    (a :->: b)
trieGeneric theConstructor f = theConstructor (trie (f . to))
{-# INLINEABLE trieGeneric #-}

-- | 'Generic'-friendly default for 'untrie'
untrieGeneric ::
    (Generic a, HasTrie (Reg a)) =>
    ((a :->: b) -> (Reg a :->: b)) ->
    (a :->: b) ->
    (a -> b)
untrieGeneric theDestructor t a = untrie (theDestructor t) (from a)
{-# INLINEABLE untrieGeneric #-}

-- | 'Generic'-friendly default for 'enumerate'
enumerateGeneric ::
    (Generic a, HasTrie (Reg a)) =>
    ((a :->: b) -> (Reg a :->: b)) ->
    (a :->: b) ->
    [(a, b)]
enumerateGeneric theDestructor t = enum' to (theDestructor t)
{-# INLINEABLE enumerateGeneric #-}

dropProduct :: (f :*: g) a -> (f a, g a)
dropProduct (a :*: b) = (a, b)
{-# INLINEABLE dropProduct #-}

liftProduct :: (f a, g a) -> (f :*: g) a
liftProduct (a, b) = a :*: b
{-# INLINEABLE liftProduct #-}

dropSum :: (f :+: g) a -> Either (f a) (g a)
dropSum s = case s of
    L1 x -> Left x
    R1 x -> Right x
{-# INLINEABLE dropSum #-}

liftSum :: Either (f a) (g a) -> (f :+: g) a
liftSum = either L1 R1
{-# INLINEABLE liftSum #-}
