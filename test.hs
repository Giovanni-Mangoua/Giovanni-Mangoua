 {-Exercices du site du zero 
  Code ecrit par Giovanni
    pseudo:Haskell
	-}
-- Script qui determine le minimun de deux nombres
myMin::Integer -> Integer -> Integer
myMin x y = if x < y then x else y
-- Script qui determine le maximun de deux nombres
myMax::Integer -> Integer -> Integer
myMax x y = if x > y then x else y 
-- Script qui determine le minimun de 04 nombres
min_4 a b c d = if myMin a b < myMin c d then myMin a b else myMin c d
-- Script qui determine le maximun de 04 nombres
max_4 a1 b1 c1 d1 = if myMax a1 b1 > myMax c1 d1 then myMax a1 b1 else myMax c1 d1
-- Fonction bornerDans
-- Appartenance a un intervalle
app x y z = if x<z && y>z then "Appartient a l'intervalle" else "n'appartient pas a l'intervalle"
-- Trier une paire ou un tuple
trier y = case y of  
   (x,_) > (_,y) -> (y,x)  
   (_,y> > (x,_) -> (x,y) 
-- norme d'un vecteur
vect::[Integer]
vect = [1,2]
square::Integer -> Integer
square x = x*x
a = square (vect !! 0)
b = square (vect !! 1)
s = a + b
d = sqrt s