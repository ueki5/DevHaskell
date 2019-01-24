module Sample01 where
data MaybeU a = NothingU | JustU a deriving (Eq, Ord, Show, Read)

applyU :: a -> MaybeU a
applyU = JustU

remveU::MaybeU a -> a
remveU (JustU a) = a

fmapU::(a -> b) -> MaybeU a -> MaybeU b
fmapU f a = applyU (f (remveU a))

hoge = fmapU (+3) (JustU 5)
--fmapU :: (a -> b) -> MaybeU a -> MaybeU b
--fmapU f a = MaybeU f a
oddU n = map (\x -> x*2 + 1) [0 .. n - 1]
oddU' n = [x*2 + 1|x <- [0 .. n]]

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
lambda = \x -> x + x
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