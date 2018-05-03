module Data.PriorityQueue (PQ, singleton, insert, minView, minViewWithKey, minAlterWithKeyF,
                           from, toOrdList, mapMaybeWithKeyA, foldMapWithKey,
                           foldrWithKey, foldlWithKey, foldrWithKeyM, foldlWithKeyM) where

import Control.Applicative
import Control.Monad
import Control.Monad.Primitive
import Data.Bits
import Data.Bool
import Data.Filtrable
import Data.Foldable
import Data.Function (on)
import Data.Functor.Classes
import Data.Tuple (swap)
import Data.Vector.Generic ((!?), Vector)
import qualified Data.Vector.Generic as V
import Data.Vector.Generic.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as MV
import Util.Vector as V
import Util.Vector.Mutable as MV
import Util

newtype PQ v k a = PQ { toV :: v (k, a) }

foldMapWithKey :: (Ord k, Vector v (k, a)) => Monoid b => (k -> a -> b) -> PQ v k a -> b
foldMapWithKey f = foldrWithKey ((<>) ∘∘ f) mempty

foldrWithKey :: (Ord k, Vector v (k, a)) => (k -> a -> b -> b) -> b -> PQ v k a -> b
foldrWithKey f z = minViewWithKey & \ case Nothing -> z
                                           Just (k, a, pq) -> f k a $ foldrWithKey f z pq

foldlWithKey :: (Ord k, Vector v (k, a)) => (k -> b -> a -> b) -> b -> PQ v k a -> b
foldlWithKey f z xs = foldrWithKey (\ k a c x -> c (f k x a)) id xs z

foldrWithKeyM :: (Ord k, Vector v (k, a)) => Monad m => (k -> a -> b -> m b) -> b -> PQ v k a -> m b
foldrWithKeyM f z xs = foldlWithKey f' pure xs z where f' k c x z = f k x z >>= c

foldlWithKeyM :: (Ord k, Vector v (k, a)) => Monad m => (k -> b -> a -> m b) -> b -> PQ v k a -> m b
foldlWithKeyM f z xs = foldrWithKey f' pure xs z where f' k x c z = f k z x >>= c

mapMaybeWithKeyA :: (Ord k, Vector v (k, a), Vector v (k, b), Applicative p)
                 => (k -> a -> p (Maybe (k, b))) -> PQ v k a -> p (PQ v k b)
mapMaybeWithKeyA f = fmap from . mapMaybeA (uncurry f) . V.toList . toV

toOrdList :: (Ord k, Vector v (k, a)) => PQ v k a -> [(k, a)]
toOrdList = foldrWithKey (curry (:)) []

from :: (Ord k, Vector v (k, a), Foldable f) => f (k, a) -> PQ v k a
from = fromV . V.fromList . toList

fromV :: (Ord k, Vector v (k, a)) => v (k, a) -> PQ v k a
fromV = PQ . V.modify (buildBy (compare `on` fst))

singleton :: Vector v (k, a) => k -> a -> PQ v k a
singleton = curry $ PQ . V.singleton

insert :: (Ord k, Vector v (k, a)) => k -> a -> PQ v k a -> PQ v k a
insert k a (PQ xs) = PQ $ V.modify go xs
  where go xs = do
            let l = MV.length xs
            xs <- MV.unsafeGrow xs 1
            MV.unsafeWrite xs l (k, a)
            siftUpBy (compare `on` fst) xs l

minAlterWithKeyF :: (Ord k, Vector v (k, a), Functor f)
                 => (Maybe (k, a) -> f (Maybe (k, a))) -> PQ v k a -> f (PQ v k a)
minAlterWithKeyF f (PQ xs) = PQ . bool (uncurry id . \ a -> V.modify' (go a) xs) (maybe xs V.singleton) (V.null xs) <$> f (xs !? 0)
  where go Nothing xs = do
            MV.unsafeSwap xs 0 (MV.length xs - 1)
            V.init <$ siftDownBy (compare `on` fst) (MV.init xs)
        go (Just (k, a)) xs = do
            MV.unsafeWrite xs 0 (k, a)
            id     <$ siftDownBy (compare `on` fst) xs

minView :: (Ord k, Vector v (k, a)) => PQ v k a -> Maybe (a, PQ v k a)
minView = minViewWithKey & fmap (\ (_, a, pq) -> (a, pq))

minViewWithKey :: (Ord k, Vector v (k, a)) => PQ v k a -> Maybe (k, a, PQ v k a)
minViewWithKey = minAlterWithKeyF (join (,)) & swap & sequenceA & fmap (\ (pq, (k, a)) -> (k, a, pq))

siftUpBy :: (MVector v a, PrimMonad m) => (a -> a -> Ordering) -> v (PrimState m) a -> Int -> m ()
siftUpBy cmp xs = go
  where go 0 = pure ()
        go n = do
            let m = n `shiftR` 1
            i <- MV.unsafeRead xs m
            j <- MV.unsafeRead xs n
            when (GT == cmp i j) $ MV.unsafeSwap xs m n >> go m

siftDownBy :: (MVector v a, PrimMonad m) => (a -> a -> Ordering) -> v (PrimState m) a -> m ()
siftDownBy cmp xs = go 0
  where go m = (liftA2 (minBy cmp') `on` \ n ->
                fmap ((,) n) <$> MV.readMaybe xs n) (m + m) (m + m + 1) >>= \ case
            Nothing -> pure ()
            Just (n, j) -> do
                i <- MV.unsafeRead xs m
                when (GT == cmp i j) $ MV.unsafeSwap xs m n >> go n
        cmp' = flip $ liftCompare (flip cmp `on` snd)

minBy :: (a -> a -> Ordering) -> a -> a -> a
minBy cmp x y | GT <- cmp x y = y | otherwise = x

buildBy :: (MVector v a, PrimMonad m) => (a -> a -> Ordering) -> v (PrimState m) a -> m ()
buildBy cmp xs = for_ (reverse [0 .. MV.length xs `shiftR` 1]) $ \ n -> siftDownBy cmp $ MV.slice 0 n xs
