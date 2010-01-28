import Data.List (intersperse)

newtype Spoken = MS { ui :: Int } deriving (Eq)

breakN :: Int -> [Int]
breakN x = map (read . (:[])) (show x)

instance Show Spoken where
    show (MS 0) = "zero"
    show (MS 1) = "one"
    show (MS 2) = "two"
    show (MS 3) = "three"
    show (MS 4) = "four"
    show (MS 5) = "five"
    show (MS 6) = "six"
    show (MS 7) = "seven"
    show (MS 8) = "eight"
    show (MS 9) = "nine"
    show (MS 10) = "ten"
    show (MS 11) = "eleven"
    show (MS 12) = "twleve"
    show (MS 13) = "thirteen"
    show (MS 14) = "fourteen"
    show (MS 15) = "fifteen"
    show (MS 16) = "sixteen"
    show (MS 17) = "seventeen"
    show (MS 18) = "eighteen"
    show (MS 19) = "nineteen"
    show (MS 20) = "twenty"
    show (MS 30) = "thirty"
    show (MS 40) = "forty"
    show (MS 50) = "fifty"
    show (MS 60) = "sixty"
    show (MS 70) = "seventy"
    show (MS 80) = "eighty"
    show (MS 90) = "ninety"
    show (MS 100) = "one hundred"
    show (MS x) | x > 99 = let (h:t:o:_) = breakN x
                           in (show $ MS h) ++ " hundred and " ++ (show $ MS (t*10+o))
                | x > 20 = let (t:o:_) = breakN x in (show $ MS $ t*10) ++ "-" ++ (show $ MS o)
    show (MS 1000) = "one thousand"

list = length $ concat $ intersperse " " $ map (show . MS) [1..1000]


{- 
   one, two, three
   fifteen, sixteen, seventeen
   twenty-one, twenty-two, twenty-three
   one hundred, one hundred and one, one hundred and two
-}
                  