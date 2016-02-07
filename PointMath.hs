{-# LANGUAGE FlexibleInstances #-}
module PointMath where


instance (Num a) => (Num (a, a)) where
    (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
    (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
    (x1, y1) * (x2, y2) = (x1*x2, y1*y2)
    abs (x, y) = (abs x, abs y)
    signum (x, y) = (signum x, signum y)
    fromInteger x = (fromInteger x, 0)

radius (x, y) = sqrt (x^2 + y^2)

normalize (x, y) = (x/r, y/r)
         where r = radius (x, y)


scalarProd (x1, y1) (x2, y2) = x1*x2 + y1*y2

toRadian = (*(pi/180))

fromRadian = (*(180/pi))

angle (x, y) | x<0       = pi + atan (y/x)
             | x>0       = atan (y/x)
             | y>0       = pi/2
             | y<0       = -pi/2
             | otherwise = error "angle: x=0. y=0"

angleDeg (x, y) = if a<0 then fromRadian (a+2*pi)
                         else fromRadian a
             where a = angle (x, y)

roundDec x = (fromIntegral $ round (10*x))/10
