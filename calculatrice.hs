{- Ma calculatrice -}
-- Fonction plus
plus::Num a => a -> a -> a
plus x y = x + y
-- Fonction moins
moins::Num a => a -> a -> a
moins a b = a - b
-- Fonction fois
fois::Num a => a -> a -> a
fois a b  = a * b
-- Fonction division entiere
divise::Integer -> Integer -> Integer
divise x y = x `div` y 
-- Fonction division reelle
division x y = x / y
-- Fonction modulo
modulo::Integer -> Integer -> Integer
modulo x y = x `mod` y
-- Fonction racine carre
racine::Floating a => a -> a
racine x = sqrt x
-- Fonction puissance
puiss::(Integer,Integer) -> Integer
puiss (x,n) = if n==0 then 1 else x * puiss(x,(n-1))
-- Fonction factorielle
fact::Integer -> Integer
fact n  
  | n==0 || n==1 = 1
  | n>=2 = n*fact(n-1)
-- Fonction combinaison 
comb::(Integer,Integer) -> Integer
comb (p,n) 
  | p==0||p==n = 1
  | p < n = comb(p,n-1) + comb(p-1,n-1)
-- Fonction arrangement
arrang (p,n) = if p < n then fact (p) + fact (n-1) else error "Pas possible"
-- Fonction logarithme neperien
logarithme x = log x
-- Fonction exponentielle
exponentielle x = exp x
-- Fonction cosinus
cosinus x = cos(x)
-- Fonction sinus
sinus x = sin(x)
-- Fonction tangente
tangente x = tan(x)
-- Fonction pour la resolution de degre 2
type Coeffs = (Float,Float,Float)
type Roots = (Float,Float)
roots :: Coeffs -> Roots
roots (a,b,c)
  | a == 0 = error "Pas du second degré"
  | e < 0 = error "Racines complexes"
  | otherwise = ((-b-r)/d,(-b+r)/d)
  where r = sqrt e
        d = 2*a
        e = b*b - 4*a*c
inv::Int -> Int
inv 0 = 1
inverser::[[Int]] -> [[Int]]
inverser image = map(map inv) image
refleter::[[Int]] -> [[Int]]
refleter image = reverse image
eln::Int -> [a] -> a
eln n xs = xs !! n
colonne image n = map(eln n) image
tranposer::[[Int]] -> [[Int]]
tranposer image = map(colonne image)[0..(length(head image) - 1)]