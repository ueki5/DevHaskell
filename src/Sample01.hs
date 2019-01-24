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
