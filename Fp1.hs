head' (x:_) = x

tail' (_:xs) = [y | y<-xs]

take' 0 (x:xs) = []
take' 1 (x:xs) = x : []
take' n (x:xs) = x : take' (n-1) xs

drop' n []     = []
drop' 0 (x:xs) = x : [y | y<-xs]
drop' n (x:xs) = drop' (n-1) xs

filter' f xs = [y | y<-xs, f y == True]

foldl' f z []     = z
foldl' f z (x:xs) = foldl' f (f z x) xs

concat' [] l2     = l2
concat' (x:xs) l2 = x : (concat' xs l2)

quicksort' []     = []  
quicksort' (x:xs) =  
    let smaller   = [a |a<-xs,a<=x]  
        larger    = [a |a<-xs,a>x]  
    in  concat' (concat' (quicksort' smaller) [x])  (quicksort' larger)
