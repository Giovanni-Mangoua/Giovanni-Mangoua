signe::Integer -> [Char]
signe x
   | x > 0 = "positif"
   | x == 0 = "nul"
   | x < 0 = "négatif"
