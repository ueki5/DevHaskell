{-# LANGUAGE GADTs #-}
module Sample04 where
data Zero
data NonZero
data Binary a where
    I :: Binary a -> Binary NonZero
    O :: Binary a -> Binary a
    Z :: Binary Zero
    N :: Binary NonZero
    
instance Show (Binary a) where
    show (I x) = "1" ++ show x
    show (O x) = "0" ++ show x
    show (Z)   = "0" 
    show (N)   = "1"    
