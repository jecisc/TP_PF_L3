-- TP 2 - Dragon 
-- Ferlicot-Delbecque Cyril.
module Main where
import Graphics.Gloss


main::IO()
main = animate(InWindow "Dragon" (500, 500) (200, 200)) white (dragonAnime' (50,250) (450,250))
dragonAnime :: RealFrac a => Point -> Point -> a -> Picture
dragonAnime a b t = Line (dragon a b !! (round t `mod` 18))

-- Question 1

alterne :: [a] -> [a] 
alterne [] = []
alterne [x] = [x]
alterne (x:_:xs) = x:alterne xs

-- Question 2

combine :: (a -> b -> c) -> [a] -> [b] -> [c]
combine _ [] _ = []
combine _ _ [] = []
combine f (x:xs) (y:ys) = f x y:combine f xs ys

-- Question 3

pasPascal :: [Integer] -> [Integer] 
pasPascal [] = []
pasPascal xs = zipWith (+) (0:xs) (xs ++ [0])

-- Question 4

pascal :: [[Integer]]
pascal = iterate pasPascal [1]

-- Question 5

pointAintercaler :: Point -> Point -> Point
pointAintercaler (xA, yA) (xB, yB) = ((xA + xB)/2 + (yB - yA)/2, (yA + yB)/2 + (xA - xB)/2)

-- Question 6  

pasDragon :: Path -> Path
pasDragon [] = []
pasDragon [x] = [x]
pasDragon (x:y:xs) = x:pointAintercaler x y:y:pasDragonInv (y:xs)
                        where pasDragonInv [] = []
                              pasDragonInv [_] = []
                              pasDragonInv (x':y':xs') = pointAintercaler y' x':pasDragon (y':xs')

--Question 7

dragon :: Point -> Point -> [Path]
dragon pt1 pt2 = iterate pasDragon [pt1, pt2]

-- Question 8

dragonOrdre :: Point -> Point -> Int -> Path
dragonOrdre a b 0= [a,b]
dragonOrdre a b 1 = [a,pointAintercaler a b, b]
dragonOrdre a b n = dragonOrdre a (last (take 2 (dragonOrdre a b 1))) (n-1) ++ tail (reverse (dragonOrdre b (last (take 2 (dragonOrdre a b 1))) (n-1)))

--Question 9

listeSegment :: Point -> Point -> Int -> [Path]
listeSegment _ _ 0 = []
listeSegment a b n = listeSegment a b (n-1) ++ [dragonOrdre a b (n+1)]

dragonAnime' :: RealFrac a => Point -> Point -> a -> Picture
dragonAnime' a b t = Line (listeSegment a b 16 !! (round t `mod` 16))