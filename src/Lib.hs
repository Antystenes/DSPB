{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Strict #-}
module Lib where

import Data.Function (fix)
import Data.List (foldl')
import Control.Lens
import Control.Comonad
import Graphics.Gloss hiding (Line)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Line a = Line { _left   :: [a]
                   , _center ::  a
                   , _right  :: [a]}

instance Functor Line where
  fmap f (Line left x right) = Line (f <$> left) (f x) (f <$> right)

instance Foldable Line where
  foldr f acc (Line left x right) = foldl' (flip f) (f x $ foldr f acc right) left

instance Show a => Show (Line a) where
  show = concatMap ((++" ").show)

data Grid a = Grid { _up    :: [Line a]
                   , _mid   ::  Line a
                   , _down  :: [Line a]}

instance Functor Grid where
  fmap f (Grid a b c) = Grid ((fmap . fmap) f a) (fmap f b) ((fmap . fmap) f c)

instance Foldable Grid where
  foldr f acc (Grid up x down) = foldl' (foldr f) (foldr f (foldr (flip . foldr $ f) acc down) x) up

instance Show a => Show (Grid a) where
  show (Grid up x down) = upSh ++ show x ++ "\n" ++ doSh
    where
      upSh = foldl' (flip(++)) [] . fmap ((++"\n").show) $ up
      doSh = concatMap ((++"\n").show) down

data Cell = Alive | Dead deriving Eq

instance Show Cell where
  show Alive = "O"
  show Dead = " "

sampleLine = Line (concat . replicate 34 $ [Alive,Dead]) Dead (concat . replicate 34 $ [Alive,Dead])

sampleGrid = changeCent Dead $ Grid (replicate 37 sampleLine) sampleLine (replicate 37 sampleLine)

changeMid i (Line l _ r) = Line l i r

changeCent x (Grid u c d) = Grid u (changeMid x c) d

moveLeftLine (Line (l:left) x right) = Line left l (x:right)
moveLeftLine x                       = x

moveRightLine (Line left x (r:right)) = Line (x:left) r right
moveRightLine x                       = x

limap :: (Line a -> Line a) -> Grid a -> Grid a
limap f (Grid up x down) = Grid (map f up) (f x) (map f down)

moveLeftGrid :: Grid a -> Grid a
moveLeftGrid = limap moveLeftLine

moveRightGrid :: Grid a -> Grid a
moveRightGrid = limap moveRightLine

moveUpGrid :: Grid a -> Grid a
moveUpGrid (Grid (y:up) x down ) = Grid up y (x:down)
moveUpGrid g                     = g

moveDownGrid :: Grid a -> Grid a
moveDownGrid (Grid up x (y:down)) = Grid (x:up) y down
moveDownGrid g                    = g

appendToLine :: Line a -> a -> Line a
appendToLine (Line y x []) z = Line y x [z]
appendToLine l@(Line _ _ z) w = flip appendToLine w. foldr (.) id (replicate (length z) moveRightLine) $ l

height :: Grid a -> Int
height (Grid up _ down) = length up + length down + 1

width :: Grid a -> Int
width (Grid _ x _) = length x

horizontalConfigurations :: Grid a -> Line (Grid a)
horizontalConfigurations g@(Grid _ (Line left _ right) _) = Line leftC g rightC
  where
    confs ls f = map ($g) . tail . scanl (.) id . map (\_ -> f) $ ls
    leftC = confs left moveLeftGrid
    rightC = confs right moveRightGrid

gridConfigurations :: Grid a -> Grid (Grid a)
gridConfigurations a@(Grid u _ d) = Grid up center down
  where
    center = horizontalConfigurations a
    up     = map horizontalConfigurations uConfs
    down   = map horizontalConfigurations dConfs
    confs ls f = map ($a) . tail . scanl (.) id . map (\_ -> f) $ ls
    uConfs = confs u moveUpGrid
    dConfs = confs d moveDownGrid

instance Comonad Grid where
  extract (Grid _ (Line _ x _) _) = x
  duplicate                       = gridConfigurations


upperNeighb :: Grid a -> [a]
upperNeighb (Grid [] _ _) = []
upperNeighb (Grid (u:_) _ _) = case u of
  Line (l:_) x (r:_) -> [l,x,r]
  Line []    x (r:_) -> [x,r]
  Line (l:_) x []    -> [l,x]
  Line []    x []    -> [x]

lowerNeighb :: Grid a -> [a]
lowerNeighb (Grid _ _ []) = []
lowerNeighb (Grid _ _ (u:_)) = case u of
  Line (l:_) x (r:_) -> [l,x,r]
  Line []    x (r:_) -> [x,r]
  Line (l:_) x []    -> [l,x]
  Line []    x []    -> [x]

horizNeighb :: Grid a -> [a]
horizNeighb (Grid _ l _) = case l of
  Line []    _ []    -> []
  Line (a:_) _ []    -> [a]
  Line []    _ (b:_) -> [b]
  Line (a:_) _ (b:_) -> [a,b]

counts :: Eq a => a -> [a] -> Int
counts = (length.).filter.(==)

mooresNeighb :: Grid a -> [a]
mooresNeighb g = upperNeighb g ++ lowerNeighb g ++ horizNeighb g

conwayUpdate :: Grid Cell -> Cell
conwayUpdate g =
  let aliveCounts = counts Alive . mooresNeighb $ g
  in case extract g of
    Dead  -> if aliveCounts == 3 then Alive else Dead
    Alive -> if aliveCounts > 3 || 2 > aliveCounts
      then Dead
      else Alive

class Drawable a where
  draw :: a -> Picture

instance Drawable Cell where
  draw Alive = Color blue $ circleSolid 5
  draw Dead  = Pictures []

instance Drawable a => Drawable (Line a) where
  draw (Line left x right) = Pictures $ draw x : leftD++rightD
    where
      leftD = zipWith update [-10,-20..] . map draw $ left
      rightD= zipWith update [10,20..] . map draw $ right
      update _ (Pictures []) = Pictures []
      update p y = Translate p 0 y


instance Drawable a => Drawable (Grid a) where
  draw (Grid up cent down) = Pictures $ draw cent : upD ++ downD
    where
      upD   = zipWith (Translate 0) [10,20..] . map draw $ up
      downD = zipWith (Translate 0) [-10,-20..] . map draw $ down

simulationGOF :: IO ()
simulationGOF = simulate (InWindow "Conway" (1366, 768) (0,0)) white 3 sampleGrid draw (\_ _ m -> m =>> conwayUpdate)

data Nucleobase = A | U | G | C

data Direction = Up | Down | Left | Right

data Nucleotide = Nucleotide { _base  :: Nucleobase,
                               _bond3 :: Maybe Direction,
                               _bond5 :: Maybe Direction,
                               _hbond :: Maybe Direction}

instance Drawable Nucleobase where
  draw g = Color c $ circleSolid 4
    where c = case g of
            A -> blue
            U -> red
            G -> green
            C -> yellow
