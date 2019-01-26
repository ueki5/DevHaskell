module Sample02 where

type Bit = Int
bin2int::[Bit] -> Int
bin2int bits = sum [w * b | (w, b) <- zip weights bits]
    where weights = iterate (*2) 1