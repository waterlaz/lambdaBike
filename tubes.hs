{-# LANGUAGE TupleSections #-}

import Data.Maybe
import System.Environment

data Tubes = Tubes {
                cutTubeR :: Double,
                baseTubeR :: Double,
                angle :: Double,
                offset :: Double,
                wall :: Double
             }

arrowDef = unlines [
  "/arrowhead {% stack: s x1 y1, current point: x0 y0",
  "  gsave",
  "    currentpoint % s x1 y1 x0 y0",
  "    4 2 roll exch % s x0 y0 y1 x1",
  "    4 -1 roll exch % s y0 y1 x0 x1",
  "    sub 3 1 roll % s x1-x2 y0 y1",
  "    sub exch % s y0-y1 x1-x2",
  "    atan rotate % rotate over arctan((y0-y1)/(x1-x2))",
  "    dup scale % scale by factor s",
  "    -7 2 rlineto 1 -2 rlineto -1 -2 rlineto",
  "    closepath fill % fill arrowhead",
  "  grestore",
  "  newpath",
  "} def\n"]

-- The depth of the cut based on the angle on the tube
-- If the cut should be made, returns a pair of depths 
-- otherwise returns Nothing
zFromG :: Tubes -> Double -> Maybe (Double, Double)
zFromG (Tubes r1 r2 aDeg off _) g' = let x = r1 * cos g
                                         y = r1 * sin g
                                         a = aDeg*pi/180
                                         d | r2>0 = r2*r2 - (x+off)^2
                                           | otherwise = 0
                                         g = g' + pi/2
                                     in if d<0 then Nothing
                                               else Just $ ((y*(cos a) + sqrt d)/(sin a), (y*(cos a) - sqrt d)/(sin a))

showPair (x, y) = show x ++ " " ++ show y ++ " "

poly isClosed xys = unlines $
                ["newpath", (showPair (head xys) ++ "moveto")] ++
                (map (\(x, y) -> showPair (x, y) ++ "lineto") (tail xys)) ++
                [closeStr, "stroke\n"]
     where closeStr | isClosed = "closepath"
                    | otherwise = ""

polygon = poly True 

polyline = poly False

line (x1, y1) (x2, y2) = "newpath\n" ++ showPair (x1, y1) ++ "moveto\n" ++ showPair (x2, y2) ++ "lineto\nstroke\n"


arrowHead p1 p2 = "newpath\n" ++ sp2 ++ "moveto 0.5 " ++ sp1 ++ "arrowhead\nstroke\n"
          where sp1 = showPair p1
                sp2 = showPair p2

printText p a s = "gsave\n" ++ showPair p ++ "translate\n 0 0 moveto\n" ++ show a ++ " rotate\n" ++"(" ++ s ++ ") show\ngrestore\n"
printTextC p a s = "gsave\n" ++ showPair p ++ "translate\n 0 0 moveto\n" ++ show a ++ 
                   " rotate\n" ++"(" ++ s ++ ") dup stringwidth pop -2 div 1 moveto show\ngrestore\n"

dimension p1 p2 s = line p1 p2 ++ arrowHead p1 p2 ++ arrowHead p2 p1 ++ 
                    printTextC ((x1+x2)/2, (y1+y2)/2) (180/pi* atan ((y2-y1)/(x2-x1))) s
             where (x1, y1) = p1
                   (x2, y2) = p2

angleCurve tubes = zas 
    where 
        angles = [0, 0.01 .. 2*pi]
        maybeZ1s :: [Maybe Double]
        maybeZ1s = map (fmap ((*(-1)).fst) . zFromG tubes) angles
        maybeZ2s = map (fmap ((*(-1)).snd) . zFromG tubes) angles
        highZAs = map fromJust $ filter isJust $ zipWith (\mz a -> fmap (, a) mz) maybeZ1s angles
        lowZAs = map fromJust $ filter isJust $ zipWith (\mz a -> fmap (, a) mz) maybeZ2s angles
        zas | all isJust maybeZ1s = highZAs
            | otherwise           = highZAs ++ (reverse lowZAs)

drawCurve r = polyline . map (\(z, a) -> (z, r*a)) 

setFont f size = "/"++f++" findfont\n" ++ show size ++ " scalefont\nsetfont\n"

showPS tubes = 
    "%!PS-Adobe-1.0\n" ++
    "%%BeginProlog\n" ++
    arrowDef ++
    "%%EndProlog\n" ++
    "72 25.4 div\ndup\nscale\n150 100 translate\n" ++
    "0.1 setlinewidth\n" ++
    setFont "Times-Roman" 5 ++
    drawCurve (cutTubeR tubes) (angleCurve tubes) ++
    drawCurve (cutTubeR tubes) (angleCurve (tubes {cutTubeR = cutTubeR tubes - wall tubes})) ++
    polygon [(0, 0), (0, l), (-100, l), (-100, 0)] ++
    line (-100, l/4) (20, l/4) ++
    line (-100, l/2) (20, l/2) ++
    line (-100, 3*l/4) (20, 3*l/4) ++
    dimension (-100, 0) (0, 0) "100" ++
    printText (-99, l-4) 0 ("Cut tube d: " ++ show (2* cutTubeR tubes)) ++
    printText (-99, l-10) 0 ("Base tube d: " ++ show (2* baseTubeR tubes)) ++
    printText (-99, l-16) 0 ("Angle: " ++ show (angle tubes))
  where
    l = 2*pi*(cutTubeR tubes)


mitter (cutD:baseD:a:off:d:f:_) = writeFile f (showPS t ++ "showpage\n")
                    where t = Tubes {
                                cutTubeR = read cutD /2,
                                baseTubeR = read baseD /2,
                                angle = read a,
                                offset = read off,
                                wall = read d/2
                              }

main = do
    args <- getArgs
    progName <- getProgName
    if length args /= 6 then putStrLn $ "Usage: " ++ progName ++ " <cut tube D> <base tube D> <angle> <offset> <wall thikness> <result.ps>"
                        else mitter args
