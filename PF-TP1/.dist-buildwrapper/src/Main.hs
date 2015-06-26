-- TP1
main :: IO()
main = undefined

--Question 3

sommeDeXaY :: Int -> Int -> Int
sommeDeXaY x y = 
        if x > y then
                0
        else
                x + sommeDeXaY (x+1) y
                
-- Question 4
                
somme ::  [Int] -> Int
somme [] = 0
somme (x:xs) = x + somme xs
                
-- Question 5
        
last' :: [a] -> a
last' xs = head (reverse xs)

init' :: [a] -> [a]
init' xs = reverse(tail(reverse xs))
        
-- Question 6
                
-- map
selectn :: [a] -> Int -> a
selectn [] _ = error "no element in the list"
selectn (x:_) 0 = x
selectn (_:xs) n = selectn xs (n-1)

concatDeuxListes :: [a] -> [a] -> [a]
concatDeuxListes [] xs = xs
concatDeuxListes [x] xs = x:xs
concatDeuxListes (x:xs) ys = concatDeuxListes [x] (concatDeuxListes xs ys) 

concat' :: [[a]] -> [a]
concat' [] = []
concat' [xs] = xs
concat' (x:xs) = concatDeuxListes x (concat' xs)

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f [x] = [f x]
map' f (x:xs) = concatDeuxListes [f x] (map f xs) 

-- Question 7
        -- Si l est une liste [a] et que l'on a la declaration x = (!!) l alors lorsque l'on invoque x n on va retourner le n-ieme element de la liste l.
                
-- Question 8
length' :: [a] -> Int
length' [] = 0
length' xs = somme (map (const 1) xs)
                
-- Question 9

applique :: (a -> a) -> a -> Int -> [a] 
applique _ _ 0 = []
applique f x n = x:applique f (f x) (n-1)

applique' :: (a->a) -> a -> Int -> [a]
applique' f x n = take n (iterate f x)
                
-- Question 10

f' :: Int -> Int
f' x = x+1

listeEnt :: Int -> [Int]
listeEnt n = 0:applique' f' 1 n










