{-Exercices de TD sur 
     le Haskell
	  Code ecrit par Giovanni
	       pseudo:Haskell
	 -}
-- Signature de la fonction signe
signe::Integer -> String
signe x
  | x > 0 = "Positif" 
  | x < 0 = "Negatif"
-- signature de la fonction parite
parite::Integer -> String
parite x = if x `mod` 2 ==0 then "Pair" else "Impair"
--Signature de pythagoricien 
square::Integer -> Integer
square x = x * x
pyth::Integer -> Integer -> Integer -> String 
pyth x y z = if square x + square y == square z then "Pythagoricien" else "Pas pythagoricien"
--Signature de la fonction signe de deux nombres
signe_1::Integer -> Integer -> String
signe_1 x y 
  | x > 0 && y >0 = "meme signe"
  | x > 0 && y < 0 || x < 0 && y > 0 = "signe contraire"
-- Signature de la fonction puissance
puiss::(Integer,Integer) -> Integer
puiss (x,n) = if n==0 then 1 else  x * puiss(x,(n-1))
-- Signature de la fonction combinaison 
comb::(Integer,Integer) -> Integer
comb (p,n) 
  | p==0 || p == n = 1
  | p < n = comb(p,n-1) + comb(p-1,n-1)
-- Signature de la fonction de fibonacci
fib::Integer -> Integer
fib n 
  | n==0 || n==1 = 1
  | n >= 2 = fib(n-1) + fib(n-2)
-- Signature de la fonction pgdc
pgdc::(Integer,Integer) -> Integer
pgdc (a,b) 
  | b ==0 = a 
  | b /= 0 = pgdc(b,(a`mod`b))
-- Signature de la fonction de la somme des n premiers termes d'une serie
somme n = if n==0 
  then 0 
  else 1/n + somme (n-1)
-- Signature de la fonction somme des nombres compris entre a et b
som a b = if a==b
  then a
  else a + som (a+1) b
-- Signature de la fonction somme des cubes des nombres compris entre a et b
cube x = x*x*x
somm a b = if a==b
  then cube a
  else cube a + somm (a+1) b
-- Signature de la fonction qui determine si un nombre est premier ou pas
premier::Integer -> Bool
premier n 
  | n < 2 = False
  | n==2 || n==3 = True
  | n`mod`1 ==0 && n`mod`2 ==0 && n`mod`n ==0 || n`mod`1 ==0 && n`mod`3 ==0 && n`mod`n ==0 = False
  | n`mod`1 ==0 && n`mod`n ==0 = True
-- Fonction sommant les nombres premiers compris entre a et b
-- Signature d'une fonction pour les jours de la semaine
data Day = Lundi
    | Mardi
    | Mercredi 
    | Jeudi
    | Vendredi
    | Samedi
    | Dimanche 
 deriving Show
data Booleen = Vrai | Faux deriving Show
jour::Day -> Booleen
jour Samedi = Vrai
jour Dimanche = Vrai
jour otherwise = Faux
-- Nombres Rationnels
type Rationnel = (Integer,Integer)
qAdd::Rationnel -> Rationnel -> Rationnel
qAdd (a,b) (c,d) = ((a*d)+(b*c),(b*d))
qSub (a,b) (c,d) = ((a*d)-(b*c),(b*d))
qMul (a,b) (c,d) = ((a*c),(b*d))
qDiv (a,b) (c,d) = ((a*d),(b*c))
-- Nombres complexes
type Complexe = (Integer,Integer)
neutre (a,b) (0,0) = (a+0,b+0)
csom (a,b) (c,d) = (a+c,b+d)
cmodule (a,b) = sqrt((a*a)+(b*b))
coppose (a,b) = (-a,-b)
-- II- Les Listes
-- Signature de la fonction d'appartenance a une liste (premier forme)
appartenir::([Integer],Integer) -> String
appartenir (xs,x)
  | x `elem` xs = "Appartient"
-- Appartenir a une liste (deuxieme forme)
appart::Eq t => t -> [t] -> Bool
appart el [] = False
appart el (x:y)
  | el == x = True
  | otherwise = appart el (y)
-- Signature de la fonction qui determine le minimun d'une liste non vide
monMin::(Ord a) => [a] -> a
monMin [x] = x
monMin (x:y) = min x (monMin y)
-- Fonction supprimant une donne de la liste
creer::[a] -> [a]
creer [x] = [x]
creer (x:y) = [x] ++ creer(y)
-- Fonction supprimant une donne de la liste 2
a `estDifferent` b = a/=b
delete el liste = filter (`estDifferent`el) liste
-- Fonction de duplication des elements d'une liste
duplicate [] = []
duplicate (x:y)= x:(x:duplicate(y))
-- Fonction qui concate deux listes
-- Renversr une liste
renverser::[a] -> [a]
renverser [x] = [x]
renverser (x:y) = renverser(y) ++ [x]
-- Fonction qui renvoie une liste privee de son dernier element
prive [y] = []
prive (x:y) = x:prive(y)
-- Polynomes
pow x n = puiss(x,n)
evalPoly::[Integer] -> (Integer,Integer) -> Integer 
evalPoly [] (x,n) = error "Polynome Invalide"
evalPoly (xs:y) (x,n)
  | n==0 = xs 
  | otherwise = xs * pow x n + evalPoly (y) (x,(n-1))
-- Derive d'un polynome
derivePoly (x:y) n 
  | n==0 = 0:[]
  | otherwise = (x*n):derivePoly (y) (n-1)
-- Somme de deux polynomes
sumPoly [x] 0 [a] 0 = [x+a]
sumPoly (x:y) m (a:b) n = if m==n then (x+a):sumPoly (y) (m-1) (b) (n-1)
  else if m>n then (x+0):sumPoly (y) (m-1) (a:b) n
  else (a+0):sumPoly (x:y) m (b) (n-1)
-- Tri par insertion
insertion el [] = [el]
insertion el (x:y) 
  | el < x = el:x:y
  | otherwise = x:insertion el y 
trieInsertion [] = []
trieInsertion (x:y) = insertion x (trieInsertion (y))
-- Codage RLE
duplique (x,n)
  |n==1 = [x]
  |otherwise = [x] ++ duplique (x,(n-1))
decompression [] = []
decompression (x:y) = duplique (snd(head(x:y)),fst(head(x:y))) ++ decompression(y)
compression [] = [] 
-- Module1: Le Quadtree


-- Somme arithmetique
-- 1 er forme
somArith::Integer -> Integer
somArith n = if n == 0
   then 0
   else n + somArith (n-1)
