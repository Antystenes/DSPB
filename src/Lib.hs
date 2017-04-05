{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses#-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE Strict #-}
module Lib where

import Prelude hiding (Either(..))
import Data.Function (fix)
import Data.List (foldl')
import Control.Lens
import Control.Comonad
import Graphics.Gloss hiding (Line)
import Graphics.Gloss.Interface.IO.Simulate (simulateIO)
import Control.Arrow
import Control.Monad (join)
import Data.Maybe (catMaybes, mapMaybe, isNothing)
import Debug.Trace (trace)
import Data.List (nub,sort)
import Data.Traversable
import System.Random (randomIO)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Line a = Line { _left   :: [a]
                   , _center ::  a
                   , _right  :: [a]} deriving Eq

instance Functor Line where
  fmap f (Line left x right) = Line (f <$> left) (f x) (f <$> right)

instance Foldable Line where
  foldr f acc (Line left x right) = foldl' (flip f) (f x $ foldr f acc right) left

instance Traversable Line where
  traverse f (Line up x down) = Line <$> traverse f up <*> f x <*> traverse f down

instance Show a => Show (Line a) where
  show = concatMap ((++" ").show)

data Grid a = Grid { _up    :: [Line a]
                   , _mid   ::  Line a
                   , _down  :: [Line a]} deriving Eq

instance Functor Grid where
  fmap f (Grid a b c) = Grid ((fmap . fmap) f a) (fmap f b) ((fmap . fmap) f c)

instance Foldable Grid where
  foldr f acc (Grid up x down) = foldl' (foldr f) (foldr f (foldr (flip . foldr $ f) acc down) x) up

instance Traversable Grid where
  traverse f (Grid up x down) = Grid <$> (traverse . traverse) f up <*> traverse f x <*> (traverse . traverse) f down

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

sampleGrid = changeCent Dead $ Grid (replicate 38 sampleLine) sampleLine (replicate 38 sampleLine)

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

-- (>>=) :: Grid a -> (Grid a -> a) -> Grid a

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

countAll :: Eq a => [a] -> [(a,Int)]
countAll = (>>=flip(,)).flip ((length.).filter.(==)) >>= map

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

data Nucleobase = A | U | G | C deriving (Eq,Show)

data Direction = Up | Down | Left | Right deriving (Eq, Show)

data Nucleotide = Nucleotide { _base  :: Nucleobase,
                               _bond3 :: Maybe Direction,
                               _bond5 :: Maybe Direction,
                               _hbond :: Maybe Direction} deriving (Eq,Show)

instance Drawable Nucleobase where
  draw g = Color c $ circleSolid 4
    where c = case g of
            A -> blue
            U -> red
            G -> green
            C -> yellow

instance Drawable Nucleotide where
  draw n = pictures (b:p5)
    where b = draw . _base $ n
          p3= case _bond3 n of
            Nothing   -> []
            Just Up   -> [line [(0,0), ( 0, 5)]]
            Just Down -> [line [(0,0), ( 0,-5)]]
            Just Left -> [line [(0,0), (-5, 0)]]
            Just Right-> [line [(0,0), ( 5, 0)]]
          p5= case _bond5 n of
            Nothing   -> p3
            Just Up   -> Color violet (line [(0,0), ( 0, 5)]) : p3
            Just Down -> Color violet (line [(0,0), ( 0,-5)]) : p3
            Just Left -> Color violet (line [(0,0), (-5, 0)]) : p3
            Just Right-> Color violet (line [(0,0), ( 5, 0)]) : p3

instance Drawable a => Drawable (Maybe a) where
  draw Nothing = pictures []
  draw (Just n)  = draw n

checkColl :: Grid (Maybe a) -> [Maybe Direction]
checkColl (Grid up (Line left _ right) down) = [u,d,l,r]
  where
    u = case up of
      []           -> Just Up
      Line _ (Just x) _:_ -> Just Up
      Line _ Nothing  _:_ -> Nothing
    d = case down of
      []           -> Just Down
      Line _ (Just _) _:_ -> Just Down
      Line _ Nothing  _:_ -> Nothing
    l = case left of
      []           -> Just Left
      Just _  :_ -> Just Left
      Nothing :_ -> Nothing
    r = case right of
      []           -> Just Right
      Just _ :_    -> Just Right
      Nothing:_    -> Nothing

sampleNLine = Line (replicate 10 Nothing) Nothing ([Just $ Nucleotide G Nothing (Just Right) Nothing, bound A, bound C, bound A, bound U, bound C, bound G, Just $ Nucleotide G (Just Left) Nothing Nothing, Nothing]++replicate 13 Nothing)
--sampleNLine = Line [Nothing] Nothing ([Just $ Nucleotide G Nothing (Just Right) Nothing, bound A, bound C, bound A, bound U, bound C, bound G, Just $ Nucleotide G (Just Left) Nothing Nothing, Nothing])
      where bound a = Just $ Nucleotide a (Just Left) (Just Right) Nothing

emptyNLine = Line (replicate 10 Nothing) Nothing (replicate 22 Nothing)

--sampleNGrid = Grid [] (Line [] (Just $ Nucleotide G (Just Right) Nothing Nothing) [Just $ Nucleotide C (Just Right) (Just Left) Nothing, Just $ Nucleotide C Nothing (Just Left) Nothing]) []

sampleNGrid = Grid (sampleNLine:replicate 20 emptyNLine) sampleNLine (sampleNLine:replicate 20 emptyNLine)

sampleNGrid2 = Grid (replicate 20 emptyNLine) emptyNLine (sampleNLine : replicate 20 emptyNLine)

simulationN :: IO ()
simulationN = simulate (InWindow "RNA" (1366, 768) (0,0)) white 3 sampleNGrid2 draw (\_ _ m -> m)

moveGrid :: Direction -> Grid a -> Grid a
moveGrid Left  = moveLeftGrid
moveGrid Right = moveRightGrid
moveGrid Up    = moveUpGrid
moveGrid Down  = moveDownGrid

mToList :: Maybe a -> (a -> [b]) -> [b]
mToList Nothing  _ = []
mToList (Just a) f = f a

class M2M m1 m2 where
  (>>=>=) :: m1 a -> (a -> m2 a) -> m2 a

instance M2M Maybe [] where
  (>>=>=) = mToList

listPart :: (a -> Maybe Direction) -> (a -> Maybe Direction) -> Grid (Maybe a) -> [Grid (Maybe a)]
listPart f z g = case extract g of
  Nothing -> []
  Just n  -> let  b5 = case f n of
                    Nothing -> []
                    Just d  -> listPartMolD f (moveGrid d g)
                  b3 = case z n of
                    Nothing -> []
                    Just d  -> listPartMolD z . moveGrid d $ g
    in g: b3 ++ b5

listPartMolD :: (a -> Maybe Direction) -> Grid (Maybe a) -> [Grid (Maybe a)]
listPartMolD f g = case extract g of
  Nothing -> []
  Just n  ->
    let ng = case f n of
          Nothing -> []
          Just d  -> listPartMolD f (moveGrid d g)
    in g : ng

listPartMol = listPart _bond5 _bond3

listPartTup = listPart (_bond5 . fst) (_bond3 . fst)

biggersnd :: Ord b => (a,b) -> (a,b) -> (a,b)
biggersnd n@(_,x) m@(_,y) = if x > y then n else m

nucNeighbN :: (Nucleotide -> Maybe Direction) -> Grid (Maybe (Nucleotide,Direction)) -> [Direction]
nucNeighbN f g = case extract g of
   Nothing -> []
   Just (n,_)  -> (catMaybes . filter ((/= _bond5 n)&&&(/= _bond3 n) >>> uncurry (&&)) . checkColl $ g) ++ case f n of
     Nothing -> []
     Just d  -> nucNeighbN f . moveGrid d $ g

nucNeighb :: Grid (Maybe (Nucleotide,Direction)) -> [Direction]
nucNeighb g = case extract g of
  Nothing -> []
  Just (n,_)  -> let  b5 = case _bond5 n of
                        Nothing -> []
                        Just d  -> nucNeighbN _bond5 . moveGrid d $ g
                      b3 = case _bond3 n of
                        Nothing -> []
                        Just d  -> nucNeighbN _bond3 . moveGrid d $ g
                 in nub $ (catMaybes . filter ((/= _bond5 n)&&&(/= _bond3 n) >>> uncurry (&&)) . checkColl $ g) ++ b3 ++ b5

fl2dir :: Float -> Direction
fl2dir n
  | n < 0.25 = Left
  | n < 0.5  = Right
  | n < 0.75 = Up
  | n < 1    = Down
  | otherwise= Left

diffusion :: Grid (Maybe Nucleotide) -> IO (Grid (Maybe (Nucleotide, Direction)))
diffusion = mapM (\g -> ((,) <$> g <*>) . pure . fl2dir <$> randomIO )

molDir :: Grid (Maybe (Nucleotide, Direction)) -> Maybe (Direction, Int)
molDir = ((foldr (biggersndM.Just) Nothing.). filter .(not.). flip (elem.fst). nucNeighb) <*> (sort . countAll . mapMaybe ((snd <$>).extract) . listPartTup)

--foldr (biggersndM.Just) Nothing. filter.(not.).flip (elem.fst).nucNeighb <*>

biggersndM :: Ord b => Maybe (a,b) -> Maybe (a,b) -> Maybe (a,b)
biggersndM Nothing Nothing = Nothing
biggersndM x Nothing = x
biggersndM Nothing y = y
biggersndM x y = biggersnd <$> x <*> y

dirs :: [Direction]
dirs = [Up,Down,Left,Right]

neuNeigh :: Grid a -> [(Direction,a)]
neuNeigh (Grid up (Line left _ right) down) = catMaybes [u,d,l,r]
  where
    u = case up of
      []           -> Nothing
      Line _ x _:_ -> Just (Up,x)
    d = case down of
      []           -> Nothing
      Line _ x _:_ -> Just (Down,x)
    l = case left of
      []           -> Nothing
      x:_          -> Just (Left,x)
    r = case right of
      []           -> Nothing
      x:_          -> Just (Right,x)


neuConfs :: Eq a => Grid a -> [(Direction,Grid a)]
neuConfs g = filter ((/=g).snd) . map (\d -> (d ,moveGrid d g)) $ dirs

inv :: Direction -> Direction
inv Left = Right
inv Up   = Down
inv Right= Left
inv Down = Up

compre x y = case (x >>= molDir,y >>= molDir) of
  (Nothing, Nothing) -> Nothing
  (_  ,     Nothing) -> x
  (Nothing, _      ) -> y
  (mdx,mdy)          -> if biggersndM mdx mdy == mdx then x else y

instance (Ord a,Eq b) => Ord (Grid (Maybe (b,a))) where
  (<=) f g = (snd <$> extract f) <= (snd <$> extract g)

instance Ord Direction where
  (<=) Left Right = True
  (<=) Up Down    = True
  (<=) Left Up    = True
  (<=) Left Down  = True
  (<=) Right Up   = True
  (<=) Right Down = True
  (<=) Left Left  = True
  (<=) Right Right= True
  (<=) Down Down  = True
  (<=) Up   Up    = True
  (<=) _     _    = False

updateNuc :: Grid (Maybe (Nucleotide,Direction)) -> Maybe Nucleotide
updateNuc g = case extract g of
  Nothing -> (fst<$>). join . (extract<$>) . foldr (compre.Just . snd) Nothing . sort . filter (\(dir, gr) -> (Just $ inv dir) == (fst <$> (molDir gr))) $ neuConfs g
  Just x -> case inv . fst <$> molDir g of
    z | isNothing z -> Just . fst $ x
      | (z == (_bond3 . fst $ x)) ||  (z == (_bond5 . fst $ x)) -> z >>= (fst<$>). extract . flip moveGrid g
      | otherwise   -> Nothing


simulationNIO0 = simulateIO (InWindow "RNA" (1366, 768) (0,0)) white 1 sampleNGrid2 (return.draw) sim
  where sim _ _ g =
          (updateNuc <<=) <$> diffusion g


simulationNIO = simulateIO (InWindow "RNA" (1366, 768) (0,0)) white 1 sampleNGrid (return.draw) sim
  where sim _ _ g =
          (updateNuc <<=) <$> diffusion g
