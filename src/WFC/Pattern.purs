module WFC.Pattern where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldr, foldMap, all)
import Data.Maybe (Maybe, fromJust)
import Data.Traversable (class Traversable, traverse, sequence)
import Partial.Unsafe (unsafePartial)
import WFC.Direction (Direction, dirOffset)

newtype PatternId = PatternId Int

derive instance eqPatternId  :: Eq PatternId
derive instance ordPatternId :: Ord PatternId

instance showPatternId :: Show PatternId where
  show (PatternId i) = "P" <> show i

-- Flat row-major pixel array; length must equal n*n
newtype Pattern a = Pattern (Array a)

derive instance eqPattern  :: Eq a => Eq (Pattern a)
derive instance ordPattern :: Ord a => Ord (Pattern a)

instance showPattern :: Show a => Show (Pattern a) where
  show (Pattern px) = "Pattern " <> show px

instance functorPattern :: Functor Pattern where
  map f (Pattern px) = Pattern (map f px)

instance foldablePattern :: Foldable Pattern where
  foldl f z (Pattern px) = foldl f z px
  foldr f z (Pattern px) = foldr f z px
  foldMap f (Pattern px) = foldMap f px

instance traversablePattern :: Traversable Pattern where
  traverse f (Pattern px) = map Pattern (traverse f px)
  sequence (Pattern px)   = map Pattern (sequence px)

unsafePatternIndex :: forall a. Pattern a -> Int -> a
unsafePatternIndex (Pattern px) i = unsafePartial (fromJust (Array.index px i))

patternGet :: forall a. Int -> Pattern a -> Int -> Int -> Maybe a
patternGet n (Pattern px) x y = Array.index px (x + y * n)

-- Rotate pattern 90° clockwise: new(x,y) = old(y, n-1-x)
rotate :: forall a. Int -> Pattern a -> Pattern a
rotate n p = Pattern $ do
  y <- Array.range 0 (n - 1)
  x <- Array.range 0 (n - 1)
  pure $ unsafePatternIndex p (y + (n - 1 - x) * n)

-- Reflect pattern horizontally: new(x,y) = old(n-1-x, y)
reflect :: forall a. Int -> Pattern a -> Pattern a
reflect n p = Pattern $ do
  y <- Array.range 0 (n - 1)
  x <- Array.range 0 (n - 1)
  pure $ unsafePatternIndex p ((n - 1 - x) + y * n)

-- Up to 8 symmetry variants (rotate + reflect combinations)
symmetryVariants :: forall a. Int -> Int -> Pattern a -> Array (Pattern a)
symmetryVariants n count p =
  Array.take count
    [ p
    , reflect n p
    , rotate n p
    , reflect n (rotate n p)
    , rotate n (rotate n p)
    , reflect n (rotate n (rotate n p))
    , rotate n (rotate n (rotate n p))
    , reflect n (rotate n (rotate n (rotate n p)))
    ]

-- Check whether two patterns agree in the overlap region for direction dir.
-- Implements the "compatible" check from the original WFC overlapping model.
agrees :: forall a. Eq a => Int -> Direction -> Pattern a -> Pattern a -> Boolean
agrees n dir p1 p2 =
  let { dx, dy } = dirOffset dir
      xMin = max 0 dx
      xMax = min n (n + dx)
      yMin = max 0 dy
      yMax = min n (n + dy)
  in all identity $ do
    y <- Array.range yMin (yMax - 1)
    x <- Array.range xMin (xMax - 1)
    pure $ patternGet n p1 x y == patternGet n p2 (x - dx) (y - dy)
