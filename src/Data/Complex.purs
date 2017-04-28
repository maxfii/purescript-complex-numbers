module Data.Complex where

import Math (Radians, cos, sin, sqrt)
import Prelude

data Complex = Complex Number Number

type R = Number
type C = Complex
type Point = { x :: R, y :: R }

infix 0 Complex as :!

conjugate :: C -> C
conjugate (Complex r i) = Complex r (-i)

scale :: R -> C -> C
scale a (Complex r i) = Complex (r*a) (i*a)

mapR :: (R -> R) -> C -> C
mapR f (Complex r i) = Complex (f r) i
mapI :: (R -> R) -> C -> C
mapI f (Complex r i) = Complex r (f i)

real :: C -> R
real (Complex r _) = r

toImg :: R -> C
toImg n = Complex zero n

unitIm :: R -> C
unitIm n = Complex one n

toReal :: R -> C
toReal n = Complex n zero

image :: C -> R
image (Complex _ i) = i

length :: C -> R
length (Complex r i) = sqrt (sqr r + sqr i) where sqr x = x * x

normalize :: C -> C
normalize z = l ^* z where l = one / length z

angle :: Radians -> C
angle θ = Complex (cos θ) (sin θ)

toRecord :: C -> Point
toRecord (Complex r i) = {x: r, y: i}

fromRecord :: Point -> C
fromRecord {x,y} = Complex x y

infix 3 scale as ^*

ap :: ∀ a. (R -> R -> a) -> C -> a
ap f (Complex r i) = f r i

instance complexRing :: Ring Complex where
  sub (Complex r i) (Complex r' i') = Complex (r - r') (i - i')

instance complexSemiring :: Semiring Complex where
  one = Complex one zero
  mul (Complex r i) (Complex r' i') = Complex (r'* r - i'* i) (i'* r + r'* i)

  zero = Complex zero zero
  add (Complex r i) (Complex r' i') = Complex (r'+ r) (i'+ i)

instance complexEuclideanRing :: EuclideanRing Complex where
  mod (Complex r i) (Complex r' i') = 
    Complex ((r*r'+ i*i')/v) ((i*r'- r*i')/v) where v = (r*r + i'*i')
  div (Complex r i) (Complex r' i') = 
    Complex ((r*r'+ i*i')/v) ((i*r'- r*i')/v) where v = (r*r + i'*i')
  degree _ = 1

instance complexCommutativeRing :: CommutativeRing Complex 
instance complexField :: Field Complex 

instance complexShow :: Show Complex where
  show (Complex r i) = show r <> " + " <> show i <> "i "
