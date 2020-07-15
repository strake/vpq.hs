module Util.Vector where

import Control.Monad.ST
import Data.Vector.Generic
import Data.Vector.Generic.New (New (..))

modify' :: Vector v a => (∀ s . Mutable v s a -> ST s b) -> v a -> (b, v a)
modify' f = fmap new . helper . clone
  where
    helper (New p) = (runST (fst <$> x), New (snd <$> x))
      where
        x = do
            v <- p
            flip (,) v <$> f v
