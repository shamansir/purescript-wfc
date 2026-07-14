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

-- N, the pattern's own width/height (patterns are always square, N×N).
-- Kept distinct from `PixelIndex`/`PatternCoord` below even though all
-- three are "just an Int" — mixing up "the size" with "an offset into it"
-- is exactly the kind of bug a bare `Int` invites and a newtype rules out.
newtype PatternSize = PatternSize Int

-- A flat row-major offset into a `Pattern`'s own pixel array (`x + y*n`) —
-- distinct from `PatternCoord`, a single axis coordinate *before* that
-- flattening.
newtype PixelIndex = PixelIndex Int

-- A single-axis coordinate (x or y) within a pattern, `0 .. n-1`.
newtype PatternCoord = PatternCoord Int

unsafePatternIndex :: forall a. Pattern a -> PixelIndex -> a
unsafePatternIndex (Pattern px) (PixelIndex i) = unsafePartial (fromJust (Array.index px i))

patternGet :: forall a. PatternSize -> Pattern a -> PatternCoord -> PatternCoord -> Maybe a
patternGet (PatternSize n) (Pattern px) (PatternCoord x) (PatternCoord y) = Array.index px (x + y * n)

-- Rotate pattern 90° clockwise: new(x,y) = old(y, n-1-x)
rotate :: forall a. PatternSize -> Pattern a -> Pattern a
rotate (PatternSize n) p = Pattern $ do
  y <- Array.range 0 (n - 1)
  x <- Array.range 0 (n - 1)
  pure $ unsafePatternIndex p (PixelIndex (y + (n - 1 - x) * n))

-- Reflect pattern horizontally: new(x,y) = old(n-1-x, y)
reflect :: forall a. PatternSize -> Pattern a -> Pattern a
reflect (PatternSize n) p = Pattern $ do
  y <- Array.range 0 (n - 1)
  x <- Array.range 0 (n - 1)
  pure $ unsafePatternIndex p (PixelIndex ((n - 1 - x) + y * n))

-- Whether a variant needed a rotation and/or a mirror to be derived from
-- the base (untransformed) window — `{ rotated: false, mirrored: false }`
-- marks the base itself. Used to tell a catalog pattern that only exists
-- because of the symmetry options apart from one that's also a genuine
-- unmodified window elsewhere in the sample (see `WFC.Catalog`'s origin
-- tracking).
type VariantTag = { rotated :: Boolean, mirrored :: Boolean }

newtype UseRotations = UseRotations Boolean
newtype UseMirror = UseMirror Boolean

-- Up to 8 symmetry variants, toggled independently: `useRotations` adds the
-- three 90°/180°/270° rotations of a pattern (and, if `useMirror` is also
-- on, of its reflection too); `useMirror` adds the horizontal reflection.
-- Both off returns just the pattern itself, unchanged from before this
-- became configurable. Each variant is tagged with which transform(s)
-- produced it, relative to the base window `p`.
taggedVariantsFor :: forall a. PatternSize -> UseRotations -> UseMirror -> Pattern a -> Array (Tuple VariantTag (Pattern a))
taggedVariantsFor n (UseRotations useRotations) (UseMirror useMirror) p =
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

variantsFor :: forall a. PatternSize -> UseRotations -> UseMirror -> Pattern a -> Array (Pattern a)
variantsFor n useRotations useMirror p = map (\(Tuple _ variant) -> variant) (taggedVariantsFor n useRotations useMirror p)

newtype Agrees = Agrees Boolean

derive newtype instance eqAgrees :: Eq Agrees
derive newtype instance showAgrees :: Show Agrees

-- Check whether two patterns agree in the overlap region for direction dir.
-- Implements the "compatible" check from the original WFC overlapping model.
agrees :: forall a. Eq a => PatternSize -> Direction -> Pattern a -> Pattern a -> Agrees
agrees (PatternSize n) dir p1 p2 =
  let { dx, dy } = dirOffset dir
      xMin = max 0 dx
      xMax = min n (n + dx)
      yMin = max 0 dy
      yMax = min n (n + dy)
  in Agrees $ all identity $ do
    y <- Array.range yMin (yMax - 1)
    x <- Array.range xMin (xMax - 1)
    pure $ patternGet (PatternSize n) p1 (PatternCoord x) (PatternCoord y) == patternGet (PatternSize n) p2 (PatternCoord (x - dx)) (PatternCoord (y - dy))
