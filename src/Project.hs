{-# LANGUAGE OverloadedStrings #-}
module Project where

import           CodeWorld

data World = World { playerPosition :: Point, obstacles :: [Point], score :: Int }

initialWorld :: [Double] -> World
initialWorld _ = World { playerPosition = (0, 0), obstacles = initialObstacles, score = 0 }

handleEvent :: Event -> World -> World
handleEvent (KeyPress key) world
  | key == "Up" = world { playerPosition = (fst (playerPosition world), snd (playerPosition world) + 1) }
  | key == "Down" = world { playerPosition = (fst (playerPosition world), snd (playerPosition world) - 1) }
handleEvent (TimePassing dt) world = updateWorld dt world
handleEvent _ world = world

drawPlayer :: Point -> Picture
drawPlayer position = translated (solidCircle 1) (fst position) (snd position)

drawObstacles :: [Point] -> Picture
drawObstacles obs = pictures [translated (solidRectangle 1 2) (fst o) (snd o) | o <- obs]

drawScore :: Int -> Picture
drawScore s = translated (scaled (lettering (printed s)) 2 2) 0 10

addObstacle :: World -> World
addObstacle world = world { obstacles = newObstacle : obstacles world }
  where newObstacle = (x, y)

checkCollision :: World -> Bool
checkCollision world = any (collidesWith (playerPosition world)) (obstacles world)

collidesWith :: Point -> Point -> Bool
collidesWith (px, py) (ox, oy) = distance (px, py) (ox, oy) < safeDistance

distance :: Point -> Point -> Double
distance (x1, y1) (x2, y2) = sqrt((x1 - x2)^2 + (y1 - y2)^2)

updateWorld :: Double -> World -> World
updateWorld dt world
  | checkCollision world = world 
  | otherwise = world { obstacles = map (moveObstacle dt) (obstacles world), score = score world + 1 }

moveObstacle :: Double -> Point -> Point
moveObstacle dt (x, y) = (x - dt * speed, y) -- | move the obstacle leftwards, speed defines how fast

drawWorld :: World -> Picture
drawWorld world = pictures [drawPlayer (playerPosition world), drawObstacles (obstacles world), drawScore (score world)]
  -- * TODD Add more drawings

run :: IO ()
run = activityOf initialWorld handleEvent drawWorld