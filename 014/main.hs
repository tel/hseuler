import Data.STRef
import Control.Monad.ST
import Data.List (maximumBy)

seeds = [1..10^6]

untilM :: (Monad m) => (a -> Bool) -> (a -> m a) -> a -> m a
untilM p f x | p x       = return x
             | otherwise = f x >>= untilM p f

collatzLength :: Integer -> (Integer, Integer)
collatzLength n = runST $ do
                    l <- newSTRef 1
                    untilM (==1) (\s -> 
                                      do
                                        modifySTRef l (+1)
                                        return $ if (even s) then s `div` 2 else 3*s+1
                                 ) n
                    len <- readSTRef l
                    return (len, n)

main = putStrLn . show $ maximumBy (\a b -> compare (fst a) (fst b)) $ map collatzLength seeds