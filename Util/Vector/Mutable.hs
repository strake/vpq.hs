module Util.Vector.Mutable where

import Control.Monad (guard)
import Control.Monad.Primitive
import Data.Vector.Generic.Mutable as MV

readMaybe :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m (Maybe a)
readMaybe xs k = (<$ guard (k < MV.length xs)) <$> unsafeRead xs k
