module WFC.Pattern where

import Prelude

import Data.Array as Array
import Data.Foldable (class Foldable, foldl, foldr, foldMap, all)
import Data.Maybe (Maybe, fromJust)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Tuple (Tuple(..))
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

-- Whether a variant needed a rotation and/or a mirror to be derived from
-- the base (untransformed) window — `{ rotated: false, mirrored: false }`
-- marks the base itself. Used to tell a catalog pattern that only exists
-- because of the symmetry options apart from one that's also a genuine
-- unmodified window elsewhere in the sample (see `WFC.Catalog`'s origin
-- tracking).
type VariantTag = { rotated :: Boolean, mirrored :: Boolean }

-- Up to 8 symmetry variants, toggled independently: `useRotations` adds the
-- three 90°/180°/270° rotations of a pattern (and, if `useMirror` is also
-- on, of its reflection too); `useMirror` adds the horizontal reflection.
-- Both off returns just the pattern itself, unchanged from before this
-- became configurable. Each variant is tagged with which transform(s)
-- produced it, relative to the base window `p`.
taggedVariantsFor :: forall a. Int -> Boolean -> Boolean -> Pattern a -> Array (Tuple VariantTag (Pattern a))
taggedVariantsFor n useRotations useMirror p =
  let rotationsOf base mirrored
        | useRotations =
            [ Tuple { rotated: false, mirrored } base
            , Tuple { rotated: true,  mirrored } (rotate n base)
            , Tuple { rotated: true,  mirrored } (rotate n (rotate n base))
            , Tuple { rotated: true,  mirrored } (rotate n (rotate n (rotate n base)))
            ]
        | otherwise = [ Tuple { rotated: false, mirrored } base ]
      mirroredSet = if useMirror then rotationsOf (reflect n p) true else []
  in rotationsOf p false <> mirroredSet

variantsFor :: forall a. Int -> Boolean -> Boolean -> Pattern a -> Array (Pattern a)
variantsFor n useRotations useMirror p = map (\(Tuple _ variant) -> variant) (taggedVariantsFor n useRotations useMirror p)

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
