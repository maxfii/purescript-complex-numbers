module Data.Complex where

import Math (Radians, cos, sin, sqrt)
import Prelude
import Data.Traversable (class Foldable, class Traversable, sequenceDefault, foldMapDefaultR)

data Complex a = Complex a a

type R = Number
type C = Complex Number

infix 0 Complex as :+

conjugate :: ∀ a. Field a => Complex a -> Complex a
conjugate (Complex r i) = Complex r (-i)

scale :: ∀ a. Field a => a -> Complex a -> Complex a
scale a (Complex r i) = Complex (r*a) (i*a)

mapR :: ∀ a. Field a => (a -> a) -> Complex a -> Complex a   
mapR f (Complex r i) = Complex (f r) i

mapI :: ∀ a. Field a => (a -> a) -> Complex a -> Complex a   
mapI f (Complex r i) = Complex r (f i)

real :: ∀ a. Field a => Complex a -> a
real (Complex r _) = r

image :: ∀ a. Field a => Complex a -> a
image (Complex _ i) = i

toImg :: ∀ a. Field a => a -> Complex a
toImg n = Complex zero n

toReal :: ∀ a. Field a => a -> Complex a
toReal n = Complex n zero

{--abs :: ∀ a. Field a => Complex a -> a--}
abs :: C -> R
abs (Complex r i) = sqrt (sqr r + sqr i) where sqr x = x * x

{--normalize :: ∀ a. Field a => Complex a -> Complex a--}
normalize :: C -> C
normalize z = l ^* z where l = one / abs z

angle :: Radians -> Complex Number
angle θ = Complex (cos θ) (sin θ)

infix 3 scale as ^*

ap :: ∀ a b. Field a => (a -> a -> b) -> Complex a -> b
ap f (Complex r i) = f r i

instance complexRing :: Field a => Ring (Complex a) where
  sub (Complex r i) (Complex r' i') = Complex (r - r') (i - i')

instance complexSemiring :: Field a => Semiring (Complex a) where
  one = Complex one zero
  mul (Complex r i) (Complex r' i') = Complex (r'* r - i'* i) (i'* r + r'* i)

  zero = Complex zero zero
  add (Complex r i) (Complex r' i') = Complex (r'+ r) (i'+ i)

instance complexEuclideanRing :: Field a => EuclideanRing (Complex a) where
  mod (Complex r i) (Complex r' i') = 
    Complex ((r*r'+ i*i')/v) ((i*r'- r*i')/v) where v = (r*r + i'*i')
  div (Complex r i) (Complex r' i') = 
    Complex ((r*r'+ i*i')/v) ((i*r'- r*i')/v) where v = (r*r + i'*i')
  degree _ = 1

instance complexCommutativeRing :: Field a => CommutativeRing (Complex a)
instance complexField :: Field a => Field (Complex a)

instance complexFold :: Foldable Complex where
  foldr f acc (Complex r i) = f r $ f i acc
  foldl f acc (Complex r i) = f (f acc r) i
  foldMap = foldMapDefaultR

instance complexFunctor :: Functor Complex where
  map f (Complex r i) = Complex (f r) (f i)

instance complexTraversable :: Traversable Complex where
  traverse f (Complex r i) = Complex <$> f r <*> f i
  sequence = sequenceDefault

instance complexShow :: Show a => Show (Complex a) where
  show (Complex r i) = show r <> " + " <> show i <> "i "

-- Todo: sin, cos, log, diff etc
{--csqrt --}
