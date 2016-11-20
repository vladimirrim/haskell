head' (x:_) = x

tail' (_:xs) = xs

take' 0 _          = []
take' _ []         = []
take' n (x:xs)     = x : take' (n-1) xs

drop' n []      = []
drop' n (x:xs)
    | n == 1    = xs
    | otherwise = drop' (n-1) xs

filter' f xs = [y | y<-xs, f y]

foldl' f z []     = z
foldl' f z (x:xs) = foldl' f (f z x) xs

concat' [] l2     = l2
concat' (x:xs) l2 = x : (concat' xs l2)

quicksort' []     = []  
quicksort' (x:xs) =  
    let smaller   = [a |a<-xs,a<=x]  
        larger    = [a |a<-xs,a>x]  
    in  concat' (quicksort' smaller)  (x : (quicksort' larger))
