signe x 
   | x >0 = "positif" 
   | x ==0  = "nul" 
   | x < 0 = "negatiif" 
lettres y = case y of  
   0 -> "zero"  
   1 -> "un"  
   2 -> "deux"  
   _ -> "trop grand"
nbreRacine::Integer -> Integer -> Integer -> Integer
nbreRacine a b c = if delta > 0 then 2
                   else if delta ==0 then 1
                   else 0
    where delta = b^2 - 4*a*c
{- Manipulation des listes -}
-- longueur des listes
longueur::Num a => [a] -> a
longueur [] = 0
longueur (x:y) = 1 + longueur y
-- somme des elements d'une liste
somme::Num a => [a] -> a
somme [] = 0
somme (x:y) = x + somme y
-- sommme des elements de 02 listes et renvoyant une liste
psom::Num a => [a] -> [a] -> [a]
psom [] [] = []
psom (a:b) (x:y) = [a+x]++psom b y
-- produit des elements d'une liste
produit::Num a => [a] -> a
produit [] = 1
produit (x:y) = x * produit(y)
-- minimun d'une liste
monMin::(Ord a) => [a] -> a
monMin [x] = x
monMin (x:y) = min x (monMin y)
-- maximun d'une liste
monMax::(Ord a) => [a] -> a
monMax [x] = x
monMax (x:y) = max x (monMax y) 
-- Appartenir a une liste
appartenir::Eq t => t -> [t] -> Bool
appartenir el (x:y)
  | el == x = True
  | otherwise = appartenir el (y)
-- Ajouter un a tous les elements de la liste
ajouter_1::Num a => [a] -> [a]
ajouter_1 [x] = [x+1]
ajouter_1 (x:y) = [x+1] ++ ajouter_1(y)
-- Ajouter un a tous les elements de la liste en utilisant une fonction d'ordre superieur
plus::Integer -> Integer 
plus x = x + 1
transformer plus [] = []
transformer plus (x:y) = plus x : transformer plus (y)
ajout::[Integer] -> [Integer]
ajout xs = transformer plus xs
-- Ajouter un a tous les elements de la liste en utilisant la fonction map
-- premiere approche
ajouter n liste = map(\x -> x+n) liste
-- Seconde approche
ajt liste = map(\x -> x+1) liste
-- Troisieme approche
add liste = map(+1) liste
-- Multiplier tous les elements d'une liste par 2
multiplier_2::Num a => [a] -> [a]
multiplier_2 [x] = [x*2]
multiplier_2 (x:y) = [x*2] ++  multiplier_2(y)
-- Supprimer tous les elements d'une liste
supprimer::[a] -> [a]
supprimer [] = []
supprimer (x:y) = [] ++ supprimer(y)
-- Renversr une liste
renverser::[a] -> [a]
renverser [x] = [x]
renverser (x:y) = renverser(y) ++ [x]
-- Creer une liste
creer::[a] -> [a]
creer [x] = [x]
creer (x:y) = [x] ++ creer(y)
compter x y 
  | x > y = []
  | x <= y = x:compter (x+1) y
-- Essaie
modulo_1 x y = x `mod` y
-- Parite d'un nombre
parite::Integer -> String
parite x = if x`mod`2==0
    then "pair"
    else "impair"
-- Table de multiplication par 5
table_5::Integer -> [Integer]
table_5 n 
  | n==10 = 50:[]
  | otherwise = (n*5):table_5 (n+1)
-- maximun des chiffres d'un nombre
maxi a b = if a > b
    then a
    else b
maxchiffres n
  | n==1 = 1
  | n > 10 = maxi (n`mod`10) (maxchiffres(n`div`10))
-- Somme des chiffres d'un nombre
som a b = a + b
schiffres n 
  | n==1 = 1
  | n > 10 = som (n`mod`10) (schiffres (n`div`10))
-- Verifie si un chiffre fait partir d'un nombre
estEgal x y = if x==y then True else False
verifier x n 
  | x `estEgal` (mod n 10) == True = "Appartient"
  | x `estEgal` (mod n 10) == False = verifier x ( div n 10) 
  | otherwise = "n'appartient pas"
-- Diviseur d'un nombre
-- premiere forme
diviseurNbre n = diviseur n 1
diviseur n d
  | mod n d /= 0 = diviseur n (d+1)
  | mod n d ==0 = [d] ++ diviseur n (d+1)
  | n==d = [d]
-- seconde forme en utilisant la fonction filter
d `divise` n = n `mod` d == 0
diviseurs n = filter (`divise` n) [1..n]
-- Multiplication de deux nombres en utilisant l'addition
multiplication n m
  | m==0 = 0
  | otherwise = n + multiplication n (m-1)
-- Tri par insertion
insertion el [] = [el]
insertion el (x:y) 
  | el < x = el:x:y
  | otherwise = x:insertion el y 
trieInsertion [] = []
trieInsertion (x:y) = insertion x (trieInsertion (y))
{- Utilisation des fonctions map -}
-- Fonction 
f liste = map(\x -> x*x + 2*x + 1) liste 
-- Type 
type Couleur = (Double ,Double,Double)
palette::[Couleur]
palette = [(1,0,0),(0,1,0),(0,0,1)]
inverser::Couleur->Couleur
inverser (r,g,b) = (1-r,1-g,1-b)
{- Le Glacier -}
-- Differents parfum des glaces  
data Parfum = Chocolat
    | Fraise
  | Framboise
 | Vanille
  deriving Show
-- Prix du parfum de la glace .
parfumGlace::Parfum->Double
parfumGlace Chocolat = 2.0
parfumGlace Fraise = 1.25
parfumGlace Framboise = 2.50
parfumGlace Vanille = 5.50
-- Prix des boules de glaces associees au parfum
data Glace = UneBoule Parfum
    | DeuxBoules Parfum Parfum
  deriving Show
prixGlace::Glace -> Double
prixGlace (UneBoule a) = 0.10 + parfumGlace a
prixGlace (DeuxBoules a b) = 0.20 + parfumGlace a + parfumGlace b
-- Gestion de la clientele du glacier
-- day::Int  month::Int  year::Int
data Date = Date Int Int Int deriving Show
-- client nom prenom email 
data Client = Client {
  name :: [Char],
  vorname :: [Char],
  mail :: [Char],
  datePremiereCommade :: Date,
  dateDerniereCommande :: Date,
  sommeCommande :: Float
} deriving Show
-- enregistre d'un nouveau client 
newClient nom prenom email = Client {
   name = nom,
   vorname = prenom,
   mail = email,
   datePremiereCommade = Date 0 0 2022 ,
   dateDerniereCommande = Date 0 0 2022,
   sommeCommande = 0.0
}
otherClient = Client "giovanni" "mangoua" "giovannimangoua@gmail.com" (Date 0 0 2022) (Date 0 0 2022) 0.0
{- Figure Geometrique et leurs formules -}
type Point = (Float,Float)
data Forme = Carre Float  
    | Rectangle Point 
  | Cercle Point 
 deriving Show
perimetre (Carre cote) = cote * 4
aire (Carre cote) = cote * cote
perimetre_1 (Rectangle (longueur,largeur)) = (longueur+largeur)*2
aire_1 (Rectangle (longueur,largeur)) = longueur * largeur
-- For Carelle
somL::Num a => [a] -> a
somL (x:xs) = sm (xs)
sm::Num a => [a] -> a 
sm [y] = 0
sm (xs:y) = xs + sm(y)
{- PREPAS CC INF 112 -}
{- EXERCICE 1 -}
-- Fonction max
maxim (x,y) = if x > y then x else y
-- Fonction impair
oddnumber x = x `mod` 2 /= 0
-- Fonction succimpair
succimpair (x,y) 
  |oddnumber (maxim(x,y) + 1) = maxim(x,y) + 1
  |otherwise = succimpair(x,y+1)	
{- EXERCICE 2 -}
fg (a,b) = if b == 0
   then 0
   else a + fg(a,b-1)
{- EXERCICE 3 -}
myFunction x = if x < 3 then 2*x
   else if x < 5 then x + 5
   else 2*x + 5
maFonction x
  | x < 3 = 2*x
  | x < 5 = x + 5
  | otherwise = 2*x + 5
{- EXERCICE 4 -}
-- Somme des diviseurs propres d'un nombre
somDiviseur n = diviser n 1
diviser n d
  | n`mod`d ==0 = d + diviser n (d+1)
  | n`mod`d /=0 = d + diviser n (d+1)
  | n==d = 0
d `divis` n = n `mod` d == 0
somdiviseurs 1 = 1
somdiviseurs n = sum (filter (`divis` n) [1..n]) - n
totalParfait n 
  | n `estEgal` somdiviseurs n = n:totalParfait (n-1)
  | n == 1 = 1:[]
  |n `estEgal` somdiviseurs n == False = totalParfait (n-1)
ami(a,b) 
  | somdiviseurs b == a && somdiviseurs a ==  b = "Amis"
  | otherwise = "Pas d'amis"
{- EXERCICE 5 -}
pgdc (a,b) 
  | a > b = pgdc (a-b,b)
  | b > a = pgdc (a,b-a)
  | a*b == 0 = a+b
  | otherwise = a
-- Types recursifs
data SomeInt = SomeInt Int [Int] deriving Show
somIntToList::SomeInt -> [Int]
somIntToList (SomeInt x xs) = x:xs
addIntToList::Int -> SomeInt -> SomeInt
addIntToList i s = SomeInt i (somIntToList s )
-- New type of my list
data MyList a = Nil | Cons a (MyList a) deriving Show
-- taille de la liste
len::MyList a -> Int
len Nil = 0
len (Cons x xs) = 1 + len (xs)
-- concatenation de deux listes 
conc::MyList a -> MyList a -> MyList a
conc Nil ys = ys
conc ys Nil = ys
conc (Cons x xs) ys = Cons x (conc xs ys)
-- First Element of the Liste
first::MyList a -> a
first Nil = error "Une liste vide ne possede de premier element"
first (Cons x xs) = x 
-- Dernier Element d'une liste
dernier::MyList a -> a
dernier (Cons y (Nil)) = y
dernier (Cons x y) = dernier (y)
-- Renverser une liste

{-- Examen 2017 - 2018 --}
type AdressIP = [Int]
type Octect = [Int]
verify::AdressIP -> Bool
verify [] = True
verify (x:xs) = (x>=0 && x<=255) && verify xs
norm8::Octect -> Octect
norm8 xs
   | length xs == 8 = xs
   | length xs > 8 = error "Plus de 8 bits"
   | length xs < 8 = norm xs (8 - length xs)
norm::Octect -> Int -> Octect
norm xs 0 = xs
norm xs n = [0] ++ norm xs (n-1)
isAdress::AdressIP -> Bool
isAdress xs
  | length xs == 4 && verify xs == True = True
bin::Int -> Octect
bin 1 = [1]
bin n = bin (n`div`2) ++ [n`mod`2] 
classAdr::AdressIP -> Char
classAdr (x:xs)
   | verify(x:xs) == True && head(norm8(bin x)) == 1 && head(tail(norm8(bin x))) == 1 = 'C'
   | verify(x:xs) == True && head(norm8(bin x)) == 1 && head(tail(norm8(bin x))) == 0 = 'B'
   | verify(x:xs) == True && head(norm8(bin x)) == 0 = 'A'
mask::AdressIP -> AdressIP
mask (x:xs)
   | classAdr (x:xs) == 'A' = [255,0,0,0]
   | classAdr (x:xs) == 'B' = [255,255,0,0]
   | classAdr (x:xs) == 'C' = [255,255,255,0]
{- Examen 2015-2016 -}
-- Exercice 2
type Vecteur = [Int]
type Matrice = [Vecteur]
h::Vecteur -> Int
h [] = 0
h (x:xs) = head (x:xs)
secon::Vecteur -> Vecteur
secon [] = []
secon (x:xs) = tail (x:xs)
fstcol::Matrice -> Vecteur
fstcol [] = []
fstcol (x:xs) = h x : fstcol xs
multiply::Vecteur -> Vecteur -> Vecteur
multiply [] [] = []
multiply (x:xs) (y:ys) = [x*y] ++ multiply xs ys
g::Vecteur -> Vecteur -> Int
g [] [] = 0
g (x:xs) (n:m) = (x*n) + g (xs) (m)
lnmat::Matrice -> Vecteur -> Vecteur
lnmat [] (y:ys) = []
lnmat (x:xs) (y:ys)
   | length xs == length ys  || length xs <= length ys = g (x) (y:ys) : lnmat xs (y:ys)
   | otherwise = error "Impossible"
-- Somme de deux matrices
l::Vecteur -> Vecteur -> Vecteur
l [b] [m] = [b+m]
l (a:b) (n:m) = [a+n] ++ l (b) (m)
somMat::Matrice -> Matrice -> Matrice
somMat [] [] = []
somMat (x:xs) (y:ys)
   | length xs == length ys = l (x) (y) : somMat (xs) (ys)
   | otherwise = error "Pas possible"
{- Rattrapage 2015-2016 -}
type Lettre = Char
type Mot = [Lettre]
type Dictionnaire = [Mot]
-- fonction qui compte le nombre de mots d'un dictionnaire
nbreMots::Dictionnaire -> Int
nbreMots [] = 0
nbreMots (x:xs) = 1 + nbreMots (xs)
-- fonction qui compte le nombre de lettres d'un mots
tailleMot::Mot -> Int
tailleMot xs = length xs
-- fonction qui compte qui compte le nombre de mots de taille k
nbkmot::Dictionnaire -> Int -> Int
nbkmot [] 0 = 0
nbkmot (x:xs) k = if tailleMot x == k 
  then 1 + nbkmot xs k
  else nbkmot xs k 
-- fonction qui ajoute un mot dans un dictionnaire
ajoutMot::Mot -> Dictionnaire -> Dictionnaire
ajoutMot xs ys = xs : ys
-- fonction qui verifie qu'un mot est dans un dico
estDansDico::Mot -> Dictionnaire -> Bool
estDansDico [] _ = True
estDansDico _ [] = False
estDansDico x (y:ys)
  | x == y = True
  | otherwise = estDansDico x (ys)
-- fonction qui retire un mot dans un dictionnaire
supprimeMot::Mot -> Dictionnaire -> Dictionnaire
supprimeMot [] _ = []
supprimeMot x [] = []
supprimeMot x (y:ys) 
  | x == y = ys
  | otherwise = y:supprimeMot x (ys)
-- Arbre Binaire
data Arbre a = Branche a (Arbre a) (Arbre a)
  | Feuille 
  deriving Show
profondeur Feuille = 0
profondeur (Branche _ gauche droite) = 1 + max (profondeur gauche) (profondeur droite)
-- Somme des feuilles d'un arbre
somFeuille::Arbre a  -> Int
somFeuille Feuille = 1
somFeuille (Branche _ gauche droite) = somFeuille gauche + somFeuille droite
-- Somme des branches d'un arbre
somBranche::Arbre a -> Int
somBranche Feuille = 0
somBranche (Branche _ gauche droite) = 1 + somBranche gauche + somBranche droite
-- Somme des elements d'une arbre
somElArbre::Num a => Arbre a -> a
somElArbre Feuille = 0
somElArbre (Branche el gauche droite) = el + somElArbre gauche + somElArbre droite 
-- Arbre binaire de recherche (RBA)
-- Taille des elements de la gauche ou de la droite de l'arbre
tailleEl::Num a => Arbre a -> a
tailleEl Feuille = 0
tailleEl (Branche el gauche droite) = 1 + tailleEl gauche + tailleEl droite
-- Rechecher un element dans un ABR
rechercher::Ord a => a -> Arbre a -> Bool
rechercher _ Feuille = False
rechercher n (Branche el gauche droite)
   | n == el = True
   | n < el = rechercher n gauche
   | n > el = rechercher n droite
menu xs = concat (map f (zip[1..length xs] xs))
   where f (n,x) = "(" ++ show n ++ ")" ++show x++ "\n"
   
c::(Int,Int) -> Bool
c (n,a)
  | n`mod`10 == a = True
  | n`mod`10 /= a = False
  | n`mod`10 /= a = c (n`div`10,a)
-- Representation des expressions mathematiques
data Expr = Litt Integer
  | Var String
  | Add Expr Expr
  | Mul Expr Expr
 deriving (Eq,Show)
test = Add (Mul (Var "x") (Var "x")) (Add (Mul (Litt 7) (Var "x")) (Litt 1))

palindrome::Eq a => [a] -> [Char]
palindrome [] = "palindrome"
palindrome xs
  | length xs == 1 = "palindrome"
  | reverse xs == xs = "palindrome"
  | otherwise = "pas un palindrome"
  
data Racine = Simple Float
  | Double (Float,Float)
  | Complexe (Float,Float) (Float,Float)
  deriving Show
roots::(Float,Float,Float) -> Racine
roots (a,b,c)
  | delta > 0 = Double (x1,x2)
  | delta == 0 = Simple (-b/(2*a))
  | otherwise = Complexe ((-b/(2*a)),r) ((-b/(2*a)),r1)
  where delta = (b*b) - (4*a*c)
	x1 = (-b - sqrt(delta)) / (2*a)
	x2 = (-b + sqrt(delta)) / (2*a)
	r = (-sqrt(abs(delta))) / (2*a)
	r1 = (sqrt(abs(delta))) / (2*a)
an::[Bool] -> Bool
an [] = True
an (x:xs) = x == True && an (xs)
dernierElt::[a] -> a
dernierElt [x] = x
dernierElt (x:xs) = dernierElt (xs)
conct::[[a]] -> [a]
conct [[]] = []
conct [] = []
conct (x:xs) = x ++ conct (xs)
replicat::Int -> a -> [a]
replicat 0 a = []
replicat n a = a : replicat (n-1) a 

data Flex a = Null | Leaf a | Fork Int (Flex a) (Flex a) deriving Show
isEmpty::Flex a -> Bool
isEmpty Null = True
isEmpty _ = False
isLeafy::Flex a -> Bool
isLeafy Null = False
isLeafy (Leaf a) = True
isLeafy (Fork el xt yt) = isLeafy (xt) && isLeafy (yt)
height::Flex a ->Int
height Null = 0
height (Leaf a) = 1
height (Fork el xt yt) = height (xt) + height (yt)