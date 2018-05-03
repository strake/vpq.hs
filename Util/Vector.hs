module Util.Vector where

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Vector.Generic
import Unsafe.Coerce

modify' :: Vector v a => (∀ s . Mutable v s a -> ST s b) -> v a -> (b, v a)
modify' f xs = runST $ do
    ref <- newSTRef undefined
    let ys = modify (f >=> writeSTRef ((unsafeCoerce :: STRef s a -> STRef t a) ref)) xs
    flip (,) ys <$> seq ys (readSTRef ref)
