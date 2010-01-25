
import Data.List (sort)
import Data.Char (ord, toLower)
import qualified Data.ByteString as B
import Control.Parallel.Strategies

nameScore name n = n * sum (map letterScore name)
    where letterScore a = ord a - 96

main = do s <- readFile "names.txt"
          let ss = sort $ lines $ map toLower s
              out = sum $ parBuffer 100 rwhnf $ zipWith nameScore ss [1..]
          putStrLn (show out)