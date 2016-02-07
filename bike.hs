{-# LANGUAGE FlexibleInstances #-}

import PointMath
import PostScript
import System.Process

data Frame = Frame {
    seatTube :: Double,
    topTube :: Double,
    topTubeSlope :: Double, -- this is effective top tube length
    seatAngle :: Double,
    headAngle :: Double,
    bbDrop :: Double
}

data Fork = Fork {
    rake :: Double,
    crownHeight :: Double,
    legLength :: Double -- this determines the amount of clearance (it is an effective length)
}

type ForkRender = Fork -> String

straightSchematicForkRender :: ForkRender
straightSchematicForkRender frk = line p1 p2 ++ line p2 p3
                where p1 = (0, 0)
                      p3 = forkCC frk
                      p2 = p3 - (crownHeight frk, 0)


data Wheel = Wheel {
    wheelRadius :: Double
}

data Headset = Headset {
    lowStack :: Double,
    upStack :: Double
}

data Bike = Bike {
    frame :: Frame,
    fork :: Fork,
    frontWheel :: Wheel,
    rearWheel :: Wheel,
    headset :: Headset
}

wheel700x23 = Wheel {wheelRadius = 333}
defaultHeadset = Headset {
    lowStack = 10,
    upStack = 23
}

myTrackFrame = Frame {
    seatTube = 550,
    topTube = 550,
    topTubeSlope = 0,
    seatAngle = 74,
    headAngle = 74,
    bbDrop = 60
}

myTrackFork = Fork {
    rake = 35,
    crownHeight = 19,
    legLength = 353 
}

myTrackBike = Bike {
    frame = myTrackFrame,
    fork = myTrackFork,
    frontWheel = wheel700x23,
    rearWheel = wheel700x23,
    headset = defaultHeadset
}


-- A vector from the Centre of the wheel to the Crown race. 
-- The vector is not rotated (use rotateBy headAngle for it to be correct)
forkCC :: Fork -> (Double, Double)
forkCC f = (0, -(rake f)) + (x + crownHeight f, 0)
      where x = sqrt $ (legLength f)^2 - (rake f)^2  




mulMatVec ((x1, y1), (x2, y2)) (x, y) = (x1*x + y1*y,   x2*x + y2*y)



vectorByAngle :: Double -> Double -> (Double, Double)
vectorByAngle r a = (r*cos (toRadian a), r*sin (toRadian a))

rotateBy :: Double -> (Double, Double) -> (Double, Double)
rotateBy a v = mulMatVec mat v
         where mat = ((cos th, -sin th),
                      (sin th, cos th))
               th = toRadian a


-- this gives 5 base points of a frameset (A, B, C, D, E, F):
-- A - bottom bracket
-- B - seat lug
-- C - upper head lug
-- D - lower head lug
-- E - front axle
-- F - rear axle
bikePoints :: Bike -> ((Double, Double), (Double, Double), (Double, Double), (Double, Double), (Double, Double), (Double, Double))
bikePoints bike = (a, b, c, d, e, f)
        where fr = frame bike
              ab = vectorByAngle (seatTube fr) (seatAngle fr)
              a = (0, 0)
              b = a + ab
              absCB = (sin.toRadian.seatAngle $ fr)*(topTube fr)/(sin.toRadian $ (topTubeSlope  fr + seatAngle fr))
              cb = vectorByAngle absCB (-topTubeSlope fr)
              c = b - cb
              ee' = rotateBy (headAngle fr) $ forkCC $ fork bike -- E' is the crown race point
              e'cy = snd c - snd a - bbDrop fr - snd ee'
              e'cx = e'cy/ (tan $ toRadian $ headAngle fr)
              e'c = (e'cx, e'cy)
              d = error "d undefined"
              e' = c - e'c
              e = e'-ee'
              f = error "f undefined"


showBike :: Bike -> IO ()
showBike bk = do
    let (a, b, c, _, e, _) = bikePoints bk
        ps = initPS 0.2 (842, 595) ++ 
             "700 100 translate\n" ++ 
             setFont "Times-Roman" 25 ++
             lineWithSize' a b ++ 
             lineWithSize' c b ++ 
             drawAngle b (c-b) (a-b) ++
             circle e (wheelRadius $ frontWheel bk) ++
             renderAtRot e (headAngle $ frame bk) (straightSchematicForkRender $ fork bk) ++
             "showpage\n"
    writeFile "temp.ps"  ps
    system "zathura temp.ps"
    return ()

--possibleBike = Frame {seatTube = 570, topTube = 565, topTubeSlope=0, seatAngle = 73.5, headAngle = 75}
