module Sample01 where
data Maybe' a = Nothing' | Just' a deriving (Eq, Ord, Show, Read)

apply' :: a -> Maybe' a
apply' = Just'

remve'::Maybe' a -> a
remve' (Just' a) = a

fmap'::(a -> b) -> Maybe' a -> Maybe' b
fmap' f a = apply' (f (remve' a))

-- hoge = fmap' (+3) (Just' 5)
--fmap' :: (a -> b) -> Maybe' a -> Maybe' b
--fmap' f a = Maybe' f a
--odd' n = map (\x -> x*2 + 1) [0 .. n - 1]
--odd'' n = [x*2 + 1|x <- [0 .. n]]

fib n   | n == 1 = 1
        | n == 2 = 1 
        | otherwise = fib (n - 1) + fib (n - 2)

abs n   | n > 0 = n
        | n == 0 = 0
        | otherwise = -n

signum' n   | n >  0 = 1
            | n == 0 = 0
            | n < 0 = -1

fst'::(a, b) -> a
fst' (a, _) = a
prime::Integer -> Bool
prime n     | n == 1 = True
            | n >  1 = all (\x -> mod n x /= 0) [2 .. n - 1]
divisor n = [x| x <- [1 .. n], mod n x == 0]
primes n = [x| x <- [1 .. n], prime x]
reverse' [] = []
reverse' (x:xs) = reverse' xs +++ [x]
(+++) :: [a] -> [a] -> [a]
(+++) [] y = y
(+++) x [] = x
(+++) (x:xs) y = x:(xs +++ y)
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : (zip' xs ys)

sum' = sum'' 0 where
    sum'' v [] = v
    sum'' v (x:xs) = sum'' (v + x) xs

fl::(a -> b -> b) -> b -> [a] -> b
fl f v [] = v
fl f v (x:xs) = x `f` (fl f v xs)
fr::(a -> b -> b) -> b -> [a] -> b
fr f v [] = v
fr f v (x:xs) = fr f (x `f` v) xs
odd' = not . even
twice' f = f . f
sumsqreven' ns = sum (map (^2) (filter even ns))
sumsqreven'' = sum . map (^2) . filter even