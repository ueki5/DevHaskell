module Sample02 where

type Bit = Int
bin2int::[Bit] -> Int
bin2int = bin2int' . reverse where
    bin2int' bits = sum [w * b | (w, b) <- zip weights bits]
    weights = iterate (*2) 1
int2bin::Int -> [Bit]
int2bin = reverse . int2bin' where
    int2bin' 0 = []
    int2bin' n = mod n 2 : int2bin' (n `div` 2)