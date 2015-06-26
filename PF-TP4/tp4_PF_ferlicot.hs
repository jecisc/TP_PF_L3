-- Ferlicot Delbecque Cyril
-- PF TP4

module Main where
import Control.Concurrent (threadDelay)
--import Test.QuickCheck

main::IO()

main = mapM_ ecrit arbres
    where ecrit a = do writeFile "arbre.dot" a
                       threadDelay 1000000
          arbres  = arbresDot "gcfxieqzrujlmdoywnbakhpvst"

-- Question 1

data Arbre coul val = Feuille
                      |Noeud  coul val (Arbre coul val) (Arbre coul val)
                      deriving Show

-- Permet de faire des test :
arbreTest :: Arbre Couleur Integer
arbreTest = Noeud N 1 (Noeud R 3 Feuille Feuille) Feuille

-- Question 2

mapArbre :: (a -> b) -> Arbre c a -> Arbre c b
mapArbre _ Feuille = Feuille
mapArbre f (Noeud coul val ag ad) = Noeud coul (f val) (mapArbre f ag) (mapArbre f ad)

-- Permet de tester mapArbre
test :: Num a => a -> a
test a = a * 2
 
foldArbre :: (a -> b -> b -> b)-> b -> Arbre c a -> b
foldArbre _ b Feuille = b
foldArbre f b (Noeud _ val ag ad) = f val (foldArbre f b ag) (foldArbre f b ad) 

-- Permet de tester foldArbre
foldTest :: Num a => a -> a -> a -> a
foldTest a b c =  a + ( b + c )

-- Question 3 
hauteur :: Arbre c v -> Int
hauteur Feuille = 0
hauteur (Noeud _ _ ag ad) = 1 + max (hauteur ag) (hauteur ad)

hauteur' :: Arbre c v -> Int
hauteur' = foldArbre (\_ h_ag h_ad -> 1 + max h_ag  h_ad) 0 

taille ::Arbre c v -> Int
taille Feuille = 0
taille (Noeud _ _ ag ad) = 1 + taille ag + taille ad

taille' ::Arbre c v -> Int
taille' arbre = foldArbre foldTest 0 (mapArbre (const 1) arbre)

-- Question 4 
peigneGauche :: [(c,a)] -> Arbre c a
peigneGauche [] = Feuille
peigneGauche ((c, a):xs) = Noeud c a (peigneGauche xs) Feuille

-- Question 5
prop_hauteurPeigne :: [(c, v)] -> Bool
prop_hauteurPeigne xs = length xs == hauteur (peigneGauche xs)
-- Cette propriété permet de vérifier à à partir d'une liste si la longueur de la liste correspond bien à la hauteur du peigne gauche de cette liste. 

-- Question 6
prop_tailleArbre :: Arbre c a -> Bool
prop_tailleArbre arbre = taille arbre >= hauteur arbre

prop_tailleArbre' :: Arbre c a -> Bool
prop_tailleArbre' arbre = taille' arbre >= hauteur' arbre


-- Question 7 

estComplet :: Arbre c a -> Bool
estComplet Feuille = True
estComplet (Noeud _ _ ag ad) = hauteur ag == hauteur ad && estComplet ag && estComplet ad

estComplet' :: Arbre c a -> Bool
estComplet' arbre = fst (foldArbre (\ _ (jeSuisComplet, h_ag) (jeSuisComplet', h_ad) -> (h_ag == h_ad && jeSuisComplet && jeSuisComplet', fromInteger (h_ag + 1) )) (True, 0) arbre)

-- Question 8
-- Le seul peigne gauche complet est les arbres complets de hauteur 1 et 0.
-- On ne peut pas le vérifier avec Quick Check car il faudrait coder une fonction qui génére des arbres aléatoires 
-- et notre excellent professeur de PF nous a dit que c'était trop compliqué pour nous et que l'on risquait de tout casser en le faisant.

-- Question 9 
complet :: Int -> [(c, a)] -> Arbre c a
--complet 0 _ = Feuille
--complet h xs = Noeud (fst racine) (snd racine) ag ad
--        where taille'' =  2^h -1
--              racine = xs !! div taille'' 2
--              ag = complet (h-1) xsgauche
--              ad = complet (h-1) xsdroite
--              xsgauche = take ( div taille'' 2) xs
--              xsdroite = drop ( div taille'' 2 +1) xs
complet h xs = fst (completAux h xs)

  
completAux :: Int -> [(c,a)] -> (Arbre c a, [(c,a)])
completAux 0 _ = (Feuille, [])
completAux 1 ((c,v):xs) = (Noeud c v Feuille Feuille, xs)
completAux h xs = (Noeud c v ag ad, zs)
        where (ag, ys) = completAux (h-1) xs
              (ad, zs) = completAux (h-1) (tail ys)
              (c,v) = head ys
 
              
-- L'arbre n contiens 2^n -1 noeud, donc pour 20 on a 1048575 noeuds.


--Question 10 
-- La fonction est repeat.

repeat' ::a -> [a] 
repeat' x = iterate (const x) x

-- Question 11
listeExemple :: [((), Char)]
listeExemple = [((),x) | x<- ['a'..]]

-- Question 12

aplatit :: Arbre c a -> [(c, a)]
aplatit Feuille = []
aplatit (Noeud c v ag ad) = aplatit ag ++ [(c,v)] ++ aplatit ad 

--Question 13
element :: Eq a => a -> Arbre c a -> Bool
element _ Feuille = False
element x (Noeud _ a ag ad) = x == a || element x ag || element x ad  

-- Question 14
noeud :: (c -> String) -> (a -> String) -> (c,a) -> String
noeud renderColor renderValue (c,v) = renderValue v ++ renderColor c 

-- Question 15
arcs :: Arbre c a -> [(a,a)]
arcs (Noeud _ a (Noeud c b ag ad) (Noeud c' b' ag' ad')) = (a,b):(a,b'):arcs ad''++arcs ag''
        where ad'' = Noeud c b ag ad
              ag'' = Noeud c' b' ag' ad'
arcs (Noeud _ a (Noeud c b ag ad) Feuille) = (a,b):arcs ad''
        where ad'' = Noeud c b ag ad
arcs (Noeud _ a Feuille (Noeud c b ag ad)) = (a,b):arcs ag''
        where ag'' = Noeud c b ag ad
arcs _ = []

-- Question 16
arc :: (a -> String) -> (a,a) -> String 
arc renderValue (a, b) = renderValue a ++ " -> " ++ renderValue b

-- Question 17 
dotise :: String -> (c -> String) -> (a -> String) -> Arbre c a -> String
dotise name renderColor renderValue arbre = unlines (("digraph \""++name++"\" {"):"node [fontname=\"DejaVu-Sans\", shape=circle]":"":noeuds ++ [" "] ++ arcs'++ ["}"])
        where noeuds = [noeud renderColor renderValue x| x<-aplatit arbre]
              arcs' = [arc renderValue y| y<- arcs arbre]
              
charToString :: Char -> String
charToString char = [char]

couleurToString :: a -> String
couleurToString _ = "[color=red, fontcolor=red]"  

couleurToString' :: Couleur -> String
couleurToString' R = "[color=red, fontcolor=red]"
couleurToString' N = "[color=black, fontcolor =black]" 
  
         

-- Question 18 
elementR :: Ord a => a -> Arbre c a -> Bool
elementR _ Feuille = False
elementR x (Noeud _ a ag ad)| a == x = True
                            | a > x = elementR x ag
                            | otherwise = elementR x ad
                         
-- Question 19

data Couleur = N | R
                      deriving Show
                      
-- Question 20
racineNoire :: Arbre Couleur a -> Arbre Couleur a
racineNoire (Noeud _ a ag ad) = Noeud N a ag ad
racineNoire Feuille = Feuille

equilibre :: Arbre Couleur a -> Arbre Couleur a
equilibre arbre = racineNoire (equilibre' arbre)

equilibre':: Arbre Couleur a -> Arbre Couleur a
equilibre' (Noeud N z (Noeud R y (Noeud R x a b) c) d) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre' (Noeud N z (Noeud R x a (Noeud R y b c)) d) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre' (Noeud N x a (Noeud R z (Noeud R y b c) d)) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre' (Noeud N x a (Noeud R y b (Noeud R z c d))) = Noeud R y (Noeud N x a b) (Noeud N z c d)
equilibre' (Noeud c v ag ad) = Noeud c v (equilibre' ag) (equilibre' ad) 
equilibre' _ = Feuille


-- Question 21
insertArbre :: Ord a => a -> Arbre Couleur a -> Arbre Couleur a
insertArbre val arbre = equilibre (insertWithoutEq val arbre)
                                  
insertWithoutEq :: Ord a => a -> Arbre Couleur a -> Arbre Couleur a
insertWithoutEq val arbre |elementR val arbre = arbre
insertWithoutEq val Feuille = Noeud R val Feuille Feuille
insertWithoutEq val (Noeud c v ag ad) | val < v = Noeud c v (insertWithoutEq val ag) ad
                                      | otherwise = Noeud c v ag (insertWithoutEq val ad)

--Question 22
arbreCompletRetN :: Int -> [Int] -> Arbre Couleur Int
arbreCompletRetN 0 _ = Feuille
arbreCompletRetN 1 (x:_) = Noeud N x Feuille Feuille
arbreCompletRetN n (x:xs) = insertArbre x (arbreCompletRetN (n-1) xs)
arbreCompletRetN _ _ = error "The list doesn't have enought elements."

arbreRetNExemple :: Arbre Couleur Int
arbreRetNExemple = arbreCompletRetN 10 [1..1024]

arbreCompletRetN' :: Int -> String -> Arbre Couleur Char
arbreCompletRetN' 0 _ = Feuille
arbreCompletRetN' 1 (x:_) = Noeud N x Feuille Feuille
arbreCompletRetN' n (x:xs) = insertArbre x (arbreCompletRetN' (n-1) xs)
arbreCompletRetN' _ _ = error "The list doesn't have enought elements."

arbreRetNExemple' :: Arbre Couleur Char
arbreRetNExemple' = arbreCompletRetN' 10 (take 1024 ['a'..])


--Question 23

arbresDot :: String -> [String]
arbresDot xs = arbresDot' xs Feuille

arbresDot' :: String -> Arbre Couleur Char -> [String]
arbresDot' [] _ = []
arbresDot' (x:xs) arbre = (dotise "arbre" couleurToString' (\v-> [v]) newArbre) : (arbresDot' xs newArbre)
        where newArbre = insertArbre x arbre
  





   

