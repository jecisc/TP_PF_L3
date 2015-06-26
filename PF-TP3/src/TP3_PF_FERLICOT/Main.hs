
-- Ferlicot Delbecque Cyril
-- PF TP3

module Main where
import Graphics.Gloss


main::IO()
main = animate (InWindow "L-systeme" (1000, 1000) (0, 0)) white brindilleAnime

dessin :: Picture
dessin = interpreteMot (((-150,0),0),100,1,pi/3,"F+-") "F+F--F+F"

type Symbole  = Char
type Mot      = [Symbole]
type Axiome   = Mot
type Regles   = Symbole -> Mot
type LSysteme = [Mot]

-- Question 1

motSuivant :: Regles -> Mot -> Mot
motSuivant _ "" = [] 
motSuivant regle (x:xs) = regle x ++ motSuivant regle xs


motSuivant' :: Regles -> Mot -> Mot
motSuivant' regle xs = concat [regle x| x<-xs]

motSuivant'' :: Regles -> Mot -> Mot
motSuivant'' regle xs = concatMap regle xs

-- Question 2 
vanKoch:: Symbole -> Mot
vanKoch '+' = "+"
vanKoch '-' = "-"
vanKoch 'F' = "F-F++F-F"
vanKoch _ = ""

-- Question 3
lsysteme :: Axiome -> Regles -> LSysteme
lsysteme x regle = iterate (motSuivant regle) x

-- Question 4 

type EtatTortue = (Point, Float)
type Config = (EtatTortue -- État initial de la tortue
              ,Float      -- Longueur initiale d’un pas
              ,Float      -- Facteur d’échelle
              ,Float      -- Angle pour les rotations de la tortue
              ,[Symbole]) -- Liste des symboles compris par la tortue
              

etatInitial :: Config -> EtatTortue
etatInitial (etat,_,_,_,_) = etat

longueurPas :: Config -> Float
longueurPas (_,l,_,_,_) = l

facteurEchelle :: Config -> Float
facteurEchelle (_,_,f,_,_) = f

angle :: Config -> Float
angle (_,_,_,a,_) = a

symbolesTortue :: Config -> [Symbole]
symbolesTortue (_,_,_,_,t) = t

-- Question 5 

avance :: Config -> EtatTortue -> EtatTortue
avance config ((x,y),a) = ( ( x + longueur*cos a,y + longueur*sin a) , a)
        where longueur = longueurPas config

-- Question 6 
tourneAGauche :: Config -> EtatTortue -> EtatTortue
tourneAGauche config (p, a)= (p, a + angle config )

tourneADroite :: Config -> EtatTortue -> EtatTortue
tourneADroite config (p, a) = (p, a - angle config)

-- Question 7
filtreSymbolesTortue :: Config -> Mot -> Mot
filtreSymbolesTortue config mot = [x | x<-mot, x `elem` symbolesTortue config ]

-- Question 8 
type EtatDessin = (EtatTortue, Path)

--On part du principe que les symboles seront filtrés donc si ce n'est pas un '+' ou un '-' on fait comme si c'était un 'F'.
interpreteSymbole :: Config -> EtatDessin -> Symbole -> EtatDessin
interpreteSymbole config (etat, path) symb | symb == '+' = (tourneAGauche config etat , path)
                                           | symb == '-' = (tourneADroite config etat , path) 
                                           | otherwise = (avance config etat , path ++ [fst (avance config etat)])
                                           
-- Question 9 
interpreteMot :: Config -> Mot -> Picture
interpreteMot config mot = interpreteMot' config (filtreSymbolesTortue config mot)

interpreteMot':: Config -> Mot -> Picture
interpreteMot' config mot = line (snd (foldl (interpreteSymbole config) (initi,[fst initi]) mot))
        where initi = etatInitial config


-- Question 10 
lsystemeAnime :: LSysteme -> Config -> Float -> Picture
lsystemeAnime sys config t = interpreteMot newConf (sys !! iteration)
        where iteration = round t `mod` 8
              newConf = (etatInitial config, longueurPas config * (facteurEchelle config) ^ iteration, facteurEchelle config, angle config, symbolesTortue config)



vonKoch1 :: LSysteme
vonKoch1 = lsysteme "F" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

vonKoch2 :: LSysteme
vonKoch2 = lsysteme "F++F++F++" regles
    where regles 'F' = "F-F++F-F"
          regles  s  = [s]

hilbert :: LSysteme
hilbert = lsysteme "X" regles
    where regles 'X' = "+YF-XFX-FY+"
          regles 'Y' = "-XF+YFY+FX-"
          regles  s  = [s]

dragon :: LSysteme
dragon = lsysteme "FX" regles
    where regles 'X' = "X+YF+"
          regles 'Y' = "-FX-Y"
          regles  s  = [s]

vonKoch1Anime :: Float -> Picture
vonKoch1Anime = lsystemeAnime' vonKoch1 (((-400, 0), 0), 800, 1/3, pi/3, "F+-")

vonKoch2Anime :: Float -> Picture
vonKoch2Anime = lsystemeAnime' vonKoch2 (((-400, -250), 0), 800, 1/3, pi/3, "F+-")

hilbertAnime :: Float -> Picture
hilbertAnime = lsystemeAnime' hilbert (((-400, -400), 0), 800, 1/2, pi/2, "F+-")

dragonAnime :: Float -> Picture
dragonAnime = lsystemeAnime' dragon (((0, 0), 0), 50, 1, pi/2, "F+-")

-- Question 11
type EtatDessin' = ([EtatTortue], [Path])

-- Question 12

-- Adaptons tout d'abord l'interpretation des symboles. 
interpreteSymbole' :: Config -> EtatDessin' -> Symbole -> EtatDessin'
interpreteSymbole' config (etat:xs, path:ys) symb | symb == '+' = (tourneAGauche config etat:xs , path:ys)
                                                  | symb == '-' = (tourneADroite config etat:xs , path:ys) 
                                                  | symb == '[' = (etat:etat:xs, [fst etat]:path:ys)
                                                  | symb == ']' = (xs, [fst (head xs)]:path:ys)
                                                  | otherwise = (avance config etat:xs , (path ++ [fst (avance config etat)]):ys)
interpreteSymbole' _ _ _ = error "Not matching"

-- Maintenant on adapte l'interpretation des mots.

interpreteMots :: Config -> Mot -> Picture
interpreteMots config mot = interpreteMot' config (filtreSymbolesTortue config mot)

interpreteMots':: Config -> Mot -> Picture
interpreteMots' config mot = pictures (map line (snd (foldl (interpreteSymbole' config) (initi, [[fst (head initi)]]) mot)) )
        where initi = [etatInitial config]
        
lsystemeAnime' :: LSysteme -> Config -> Float -> Picture
lsystemeAnime' sys config t = interpreteMots newConf (sys !! iteration)
        where iteration = round t `mod` 6
              newConf = (etatInitial config, longueurPas config * (facteurEchelle config) ^ iteration, facteurEchelle config, angle config, symbolesTortue config)


brindille :: LSysteme
brindille = lsysteme "F" regles
    where regles 'F' = "F[-F]F[+F]F"
          regles  s  = [s]

broussaille :: LSysteme
broussaille = lsysteme "F" regles
    where regles 'F' = "FF-[-F+F+F]+[+F-F-F]"
          regles  s  = [s]

brindilleAnime :: Float -> Picture
brindilleAnime = lsystemeAnime' brindille (((0, -400), pi/2), 800, 1/3, 25*pi/180, "F+-[]")

broussailleAnime :: Float -> Picture
broussailleAnime = lsystemeAnime' broussaille (((0, -400), pi/2), 500, 2/5, 25*pi/180, "F+-[]")





