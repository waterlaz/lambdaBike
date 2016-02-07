{-# LANGUAGE TupleSections #-}

module PostScript (polygon, polyline, line, lineWithSize, lineWithSize', arrowHead, 
                   printText, printTextC, dimension, setFont, circle, initPS, renderAtRot,
                   drawAngle) where

import PointMath
import Data.Maybe
import System.Environment

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


showPair (x, y) = show x ++ " " ++ show y ++ " "

poly :: Bool -> [(Double, Double)] -> String
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
                   " rotate\n" ++"(" ++ s ++ ") dup stringwidth pop -2 div 5 moveto show\ngrestore\n"

dimension p1 p2 s = lineWithSize p1 p2 s ++ arrowHead p1 p2 ++ arrowHead p2 p1

dimension' p1 p2 = dimension p1 p2 (show $ length $ p1-p2 )

lineWithSize p1 p2 s = line p1 p2 ++ 
                       printTextC ((x1+x2)/2, (y1+y2)/2) (180/pi* atan ((y2-y1)/(x2-x1))) s
             where (x1, y1) = p1
                   (x2, y2) = p2

lineWithSize' p1 p2 = lineWithSize p1 p2 (show $ round $ radius $ p1-p2 )

setFont f size = "/"++f++" findfont\n" ++ show size ++ " scalefont\nsetfont\n"

circle (x, y) r = showPair (x, y) ++ show r ++ " 0 360 arc closepath\nstroke\n"

renderAtRot (x, y) a s = "gsave\n" ++ showPair (x, y) ++ "translate\n" ++ show a ++ " rotate\n" ++ s ++ "grestore\n"

drawAngle p v1 v2 = showPair p ++ "40 " ++ show (angleDeg v1) ++ " " ++ show (angleDeg v2) ++ " arc\nstroke\n" ++ 
                      printTextC (p+v) 0 (show alpha)
                 where alpha = roundDec $ abs $ angleDeg v1 - angleDeg v2
                       v = ((70, 70)*) $  normalize $ normalize v1 + normalize v2



initPS :: Double -> (Double, Double) -> String
initPS scale (maxX, maxY) = 
    "%!PS-Adobe-1.0\n" ++
    "%%DocumentMedia: a4 "++ show maxX ++ " " ++ show maxY ++ " 80 () ()\n" ++
    "%%BeginProlog\n" ++
    arrowDef ++
    "%%EndProlog\n" ++
    "72 25.4 div\ndup\nscale\n" ++
    show scale ++"\ndup\nscale\n" ++
    "0.1 setlinewidth\n" ++
    setFont "Times-Roman" 5
