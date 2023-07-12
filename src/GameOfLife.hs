-- GameOfLife : Copyright (C) 2023 Skywalker<j.karlsson@retrocoder.se>
{-# LANGUAGE OverloadedStrings #-}
module GameOfLife (start) where

import Control.Monad (unless, when)
import Foreign.C.Types (CInt)
import System.Random (randoms, mkStdGen)
import SDL

data World = World {
    size :: (CInt, CInt),
    cells :: [Bool]
}

newWorld :: Int -> Int -> World
newWorld width height = World (fromIntegral width, fromIntegral height) $ listOfRandoms (width*height)
    where
        listOfRandoms :: Int -> [Bool]
        listOfRandoms n = take n $ randoms (mkStdGen 1)

start :: IO ()
start = do
    initializeAll
    window <- createWindow "Conway's Game of Life - 80 x 60 grid" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer
    gameLoop renderer $ newWorld 80 60
    destroyRenderer renderer
    destroyWindow window
    quit

gameLoop :: Renderer -> World -> IO ()
gameLoop renderer world = do
    events <- pollEvents
    let quitEvent = any ((== QuitEvent) . eventPayload) events
    unless quitEvent $ do
        rendererDrawColor renderer $= V4 0 0 0 255
        clear renderer
        renderWorld renderer world
        present renderer
        delay 10
        gameLoop renderer $ updateWorld world

updateWorld :: World -> World
updateWorld world =
    World (size world) [updateCell world x y | y <- [0..snd (size world) -1], x <- [0..fst (size world) -1] ]

updateCell :: World -> CInt -> CInt -> Bool
updateCell world x y =
    let
        width = fst $ size world
        cell = cells world !! fromIntegral (x + y * width)
        neighbours = map (uncurry $ getCell world)
            [ (x-1,y-1), (x,y-1), (x+1,y-1),
              (x-1,y  ),          (x+1,y  ),
              (x-1,y+1), (x,y+1), (x+1,y+1) ]
        aliveNeighbours = length $ filter id neighbours
    in
        if cell
            then aliveNeighbours == 2 || aliveNeighbours == 3
            else aliveNeighbours == 3

getCell :: World -> CInt -> CInt -> Bool
getCell world x y =
    let
        width = fst $ size world
        height = snd $ size world
    in
        if x < 0 || y < 0 || x >= width || y >= height
            then False
            else cells world !! fromIntegral (x + y * width)

renderWorld :: Renderer -> World -> IO ()
renderWorld renderer world = do
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    renderGrid renderer world
    renderCells renderer world
    present renderer

renderGrid :: Renderer -> World -> IO ()
renderGrid renderer world = do
    rendererDrawColor renderer $= V4 63 63 80 255
    let (width, height) = size world
    let cellWidth = 800 `div` width
    let cellHeight = 600 `div` height
    mapM_ (\(x,y) -> drawRect renderer $
        Just $ Rectangle (P $ V2 (x*cellWidth) (y*cellHeight)) (V2 cellWidth cellHeight)) $
        [ (x,y) | x<-[0..(width-1)], y<-[0..(height-1)] ]

renderCells :: Renderer -> World -> IO ()
renderCells renderer world = do
    rendererDrawColor renderer $= V4 255 255 255 255
    let (width, height) = size world
    let cellWidth = 800 `div` width
    let cellHeight = 600 `div` height
    mapM_ (\(x,y) ->
        let
            alive = cells world !! fromIntegral (x + y * width)
        in when alive $
            fillRect renderer $ Just $ Rectangle (P $ V2 (x*cellWidth) (y*cellHeight)) (V2 cellWidth cellHeight))
        $ [ (x,y) | x<-[0..(width-1)], y<-[0..(height-1)] ]

-- License
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License version 2 as
-- published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-- See the GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.
-- If not, see <http://www.gnu.org/licenses/> or write to the
-- Free Software Foundation, Inc.,
-- 51 Franklin Street, Fifth Floor,
-- Boston, MA 02110-1301
-- USA
