prime_factors :: Int -> [Int]
prime_factors 1 = []
prime_factors n
  | factors == []  = [n]
  | otherwise = factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

is_prime :: Int -> Bool
is_prime n
  | n_factors <= 1 && n /= 1 = True
  | otherwise = False
  where
    n_factors = (length . prime_factors) n
    
digits :: Int -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)
  
all_digits :: [Int] -> [Int]
all_digits []     = []
all_digits (x:xs) = digits x ++ all_digits xs

is_smith :: Int -> Bool
is_smith n
  | not is_n_prime && n_digit_sum == n_factor_digit_sum = True
  | otherwise = False
  where 
    is_n_prime = is_prime n
    n_digit_sum = (sum . digits) n
    n_factor_digit_sum = (sum . all_digits . prime_factors) n
    
all_smiths_below_n :: Int -> [Int]
all_smiths_below_n n
  | n <= 0    = []
  | otherwise = filter is_smith [0..n]
    
main :: IO()
main = putStrLn (show(all_smiths_below_n 1000))