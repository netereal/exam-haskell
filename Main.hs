module Main where

---------------------------------------------

testArray = [1, 2, 3, 4, 5]

sum_array_func :: Int -> [Int] -> [Int]
sum_array_func _ [] = []
sum_array_func prev (x:xs) = [prev + x] ++ sum_array_func (prev + x) xs

task2 :: [Int] -> [Int]
task2 input = sum_array_func 0 input

-----------------------------------------

data BinTree a = Leaf a | Branch a (BinTree a) (BinTree a) deriving Show

evenList :: Integral a => BinTree a -> [a]
evenList tree = filter even $ inorder tree
  where inorder :: BinTree a -> [a]
        inorder (Leaf x) = [x]
        inorder (Branch x l r) = (inorder l) ++ [x] ++ (inorder r)




main :: IO ()
main = do
  putStrLn "Exam haskell"
  print (task2 testArray)

  let tree = Branch 10 (Branch 4 (Leaf 3) (Leaf 5)) (Leaf 12)
  print (evenList tree)