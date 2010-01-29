odds = filter odd [1..]

repeatn n (x:xs) = replicate n x ++ repeatn n xs

diagonals = scanl (+) 1 (map (+1) $ repeatn 4 odds)

main = putStrLn (show $ sum $ take (1001 + (1001-1)) diagonals)