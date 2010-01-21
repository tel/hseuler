import Data.STRef (newSTRef, modifySTRef, readSTRef)
import Control.Monad.ST (runST)
import Control.Monad (forM_)
import Control.Parallel.Strategies

tris = scanl1 (+) [1..]

numDivis :: Integer -> Int
numDivis n = runST $ do
               c <- newSTRef 0
               forM_ [(1 :: Integer) .. (ceiling $ sqrt $ fromIntegral n)] $ \i -> do
                       if (rem n i == 0) then modifySTRef c (+2) else return ()
               readSTRef c

main = let (gt:_) = dropWhile ((<500) . fst) (zip nd tris)
           nd = map numDivis tris
       in putStrLn (show gt)

pairs = let nd = map numDivis tris in zip nd tris