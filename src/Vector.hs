{-# Language NamedFieldPuns #-}
module Vector
(Vector, makeVector, vecRead, vecWrite)
where

data Vector a = Vec { dat :: [a], len :: Int}

makeVector :: Int -> a -> Vector a 
makeVector len v = Vec { dat = makeLst len v, len }

makeLst :: Int -> a -> [a] 
makeLst 0 _ = [] 
makeLst n x = x : makeLst (n-1) x

vecRead :: Int -> Vector a -> Maybe a
vecRead i Vec { len, dat } = if i < len then Just $ dat !! i else Nothing

vecWrite :: Int -> Vector a -> a -> Maybe (Vector a)
vecWrite i vec@Vec { len, dat} v = 
    if len < i then do
        dat' <- writeLst i dat v
        return vec { dat = dat' }
    else 
        Nothing
        

writeLst :: Int -> [a] -> a -> Maybe [a]
writeLst _ [] _ = Nothing
writeLst 0 (x:xs) v = Just $ v:xs
writeLst n (x:xs) v = do 
    xs' <- writeLst (n-1) xs v
    return (x:xs')

