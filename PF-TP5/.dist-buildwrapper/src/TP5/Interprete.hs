-- TP5 -- Interprete -- Ferlicot-Delbecque Cyril
module Main where
import Parser


main::IO()
main = undefined


type Nom = String

data Expression = Lam Nom Expression
                | App Expression Expression
                | Var Nom
                | Lit Litteral
                deriving (Show,Eq)

data Litteral = Entier Integer
              | Bool   Bool
              deriving (Show,Eq)
              
-- Question 1
espacesP :: Parser ()
espacesP = do _ <- zeroOuPlus (car ' ')
              return ()
              
-- Question 2
estMini :: Char -> Bool
estMini = flip elem ['a'..'z']

minuscule :: Parser Char
minuscule = carCond estMini

nomP :: Parser Nom
nomP = do c <- unOuPlus minuscule
          espacesP
          return c

-- Question 3
varP :: Parser Expression
varP = do expr <- nomP
          return (Var expr)

-- Question 4
applique :: [Expression] -> Expression
applique = foldl1 App
--applique [] = error "Not define"
--applique [a] = a
--applique [a, b] = App a b
--applique xs = App (applique (init xs)) (last xs)

-- Question 5
exprP :: Parser Expression
exprP = varP ||| lambdaP ||| exprParentheseeP

exprsP :: Parser Expression
exprsP =do exprs <- unOuPlus exprP
           return (applique exprs)
           
-- Question 6
lambdaP :: Parser Expression
lambdaP = do _ <- car '\\'
             espacesP
             nom <- nomP
             _ <- car '-'
             _ <- car '>'
             espacesP
             exprs <- exprsP
             return (Lam nom exprs)

-- Question 8
exprParentheseeP :: Parser Expression
exprParentheseeP = do _ <- car '('
                      expr <- exprP
                      _ <- car ')'
                      return expr

-- Question 9
estChiffre :: Char -> Bool
estChiffre = flip elem ['0'..'9']

intOfChar :: Char -> Integer
intOfChar c = read [c]

chiffre :: Parser Integer
chiffre = do c <- carCond estChiffre 
             return (intOfChar c)
             
dec2int :: [Integer] -> Integer
dec2int = foldl (\x -> (10 * x +)) 0
             
nombre :: Parser Integer
nombre = do num <- unOuPlus chiffre
            return (dec2int num)

nombreP :: Parser Expression
nombreP = do num <- nombre
             espacesP
             return (Lit (Entier num))

-- Question 10 
boolP:: Parser Bool
boolP = (chaine "True" >>= \_ -> return True) ||| (chaine "False" >>= \_ -> return False)

booleenP :: Parser Expression
booleenP = do expr <- boolP
              espacesP
              return (Lit (Bool expr))

-- Question 11
expressionP :: Parser Expression
expressionP = do espacesP
                 exprsP






