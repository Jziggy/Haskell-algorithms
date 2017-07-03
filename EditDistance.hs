import           Data.Array as Array

--edit distance
main :: IO()
main = print $ editDist "something" "nothing"

editDist :: String -> String -> Int
editDist a b = d m n --find distance for last indices
  where
    m = length a
    n = length b
    a' = Array.listArray (1,m) a --converting to array w/ 1-based indexing
    b' = Array.listArray (1,n) b -- ^_^
    d :: Int -> Int -> Int
    d i 0 = i --difference between empty string and any length is just additions
    d 0 j = j -- same
    d i j
      | a' ! i == b' ! j = ds ! ((i - 1) , (j - 1)) --no operation needed
      | otherwise  = minimum [
      ds ! ((i - 1), j) + 1,
      ds !  (i, (j - 1)) + 1,
      ds ! ((i - 1) , (j - 1)) + 1
      ]
    ds = Array.listArray bounds
      [d i j | (i,j) <- Array.range bounds] --created an array of all possible function calls
    bounds = ((0,0),(m,n))
