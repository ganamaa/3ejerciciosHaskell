mayor:: [Int]->Int
mayor [x] = x
mayor (x:xs)
 | x > mayor(xs) = x
 | otherwise = mayor(xs)

mayor_listas::[[Int]]->[Int] -- ejercicio numero1
mayor_listas x = [mayor z | z <- x]

menor:: [Int]->Int
menor [x] = x
menor (x:xs)
 | x < menor(xs) = x
 | otherwise = menor(xs)
 
multiplos:: [Int]->Int->Int
multiplos lista y= length [x | x <- lista, mod x y ==0, x>y || x<y]

divisibles::[Int]->Int->Int
divisibles lista y= length [x | x <- lista, mod y x ==0, x>y || x<y]

listaTuplas:: [[Int]]->[(Int,Int,Int,Int)] -- ejercicio numero 3
listaTuplas x = [(menor z,mayor z,multiplos z y,divisibles z a)|z <- x, let y = menor z, let a = mayor z]

prom :: [Int]->Int
prom [] = 0
prom xs = div sum xs length xs 

prom_de_lista:: [[Int]]->[Int]
prom_de_lista x = [prom z | z <- x]

