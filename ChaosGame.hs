module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import Data.Maybe
import Data.Char
import System.Random

type Pos = (Float,Float)

type State = (Int,  -- Number of Major dots
             [Pos], -- Pos where I have placed a dot
             [Int], -- Random Numbers
              Int,
	      Int)  -- Mode

uPPERLIMIT = 36000

randRange = 8*9*10*11*12*13*14*15

rad = 300

framerate = 120 

dm = InWindow "Chaos Game" (1000,1000) (0,0)

main :: IO ()
main = do gen <- newStdGen
          play dm                             -- display mode
               black                          -- côr do fundo da janela
               framerate                      -- frame rate
               (0,[((fst $ randomR (0,1000) gen)-500,(fst $ randomR (0,1000) gen)-500)],randomRs (0,randRange) gen,0,0) -- estado inicial
               drawState                      -- desenha o Estado do Mapa
               changeNumDots                  -- reage a um evento
               placeNewDot                    -- reage ao passar do tempo

drawState :: State -> Picture
drawState (0,_,_,_,_) = Blank
drawState (numPoints,dotList,_,mode,x) = Pictures $ [Color red $ Translate (-485) (333) $ Scale 0.1 0.1 $ text modeDesc] ++ 
                                                  drawPoints (getAnchorPoints $ fromIntegral numPoints) ++
                                                  drawDots dotList
                                    where
                                      modeDesc | mode == 0 = "Mid Point Jumps. \n#dots :" ++ show x
                                               | mode == 1 = "Third of the way Jumps. \n#dots: " ++ show x
                                               | mode == 2 = "Fourth of the way Jumps. \n#dots :" ++ show x
                                               | mode == 3 = "Fifth of the way Jumps. \n#dots: " ++ show x

drawPoints :: [Pos] -> [Picture]
drawPoints [] = []
drawPoints (h:t) = (Color green $ Translate (fst h) (snd h) $ ThickCircle 1 2) : drawPoints t

getAnchorPoints :: Float -> [Pos]
getAnchorPoints n = aux n
              where
                aux 0 = []
                aux i = (rad * sin((2 * pi * i) / n),rad * cos((2 * pi * i) / n)) : aux (i-1)

drawDots :: [Pos] -> [Picture]
drawDots [] = []
drawDots (h:t) = (Color white $ Translate (fst h) (snd h) $ ThickCircle 1 1) : drawDots t

changeNumDots :: Event -> State -> State
changeNumDots (EventKey (SpecialKey KeyUp) Down _ _) (numPoints,(h:t),randNums,mode,x)   | mode < 3  = (numPoints,[newStartPoint randNums],drop 2 randNums,mode+1,x)
                                                                                         | otherwise = (numPoints,(h:t),randNums,mode,x)
changeNumDots (EventKey (SpecialKey KeyDown) Down _ _) (numPoints,(h:t),randNums,mode,x) | mode > 0  = (numPoints,[newStartPoint randNums],drop 2 randNums,mode-1,x)
                                                                                         | otherwise = (numPoints,(h:t),randNums,mode,x)
changeNumDots (EventKey (Char c) Down _ _) (numPoints,(h:t),randNums,mode,x) |isHexDigit c = (digitToInt c,[newStartPoint randNums],drop 2 randNums,mode,0)
                                                                             |otherwise    = (numPoints,(h:t),randNums,mode,x)
changeNumDots _ s = s

newStartPoint :: [Int] -> Pos
newStartPoint randNums = (((a*1000) / (fromIntegral randRange)) - 500,
                          ((b*1000) / (fromIntegral randRange)) - 500)
            where
              (a,b) = (fromIntegral $ head randNums, fromIntegral $ head $ tail randNums)

placeNewDot :: Float -> State -> State
placeNewDot _ (0,a,b,c,d) = (0,a,b,c,d)
placeNewDot _ (numPoints,prevPos,randNums,mode,x) |x==uPPERLIMIT = (numPoints,prevPos++[newDot],tail randNums,mode,x)
                                                  |otherwise = (numPoints,prevPos++[newDot],tail randNums,mode,x+1)
                                    where
                                      anchorDot rNum = (getAnchorPoints nP) !! (div (rNum*numPoints) randRange)
                                      nP = fromIntegral numPoints
                                      newDot = newPoint (last prevPos) $ anchorDot $ head randNums
                                      mD = fromIntegral $ mode + 2
                                      newPoint (x1,y1) (x2,y2) = (((1-(1/mD))*x1) + ((1/mD)*x2),
                                                                  ((1-(1/mD))*y1) + ((1/mD)*y2))
