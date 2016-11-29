import Prelude hiding (lookup)

data BinaryTree k v = Nil | Branch k (BinaryTree k v) (BinaryTree k v) v

lookup :: Ord k => k -> BinaryTree k v -> Maybe v 
lookup x Nil = Nothing
lookup x (Branch a left right v)  
    | x == a       = Just v  
    | x < a        = lookup x left  
    | x > a        = lookup x right 


insert :: Ord k => k -> v -> BinaryTree k v -> BinaryTree k v
insert key val Nil     = Branch  key Nil Nil val
insert key val (Branch a left right v)   
     | key == a        = Branch key left right val 
     | key < a         = Branch a (insert key val left) right  v 
     | key > a         = Branch a left (insert key val right)  v
   

delete :: Ord k => k -> BinaryTree k v -> BinaryTree k v
delete _ Nil  = Nil
delete x (Branch  k left right v)  
	| x == k = deleteX (Branch k left right v)
	| x  < k = Branch k (delete x left) right v
	| x  > k = Branch k left (delete x right) v
 
deleteX (Branch k Nil right v)  = right
deleteX (Branch k left Nil v)   = left
deleteX (Branch k left right v) = (Branch k2 left (delete k2 right) v2)
        where 
             k2                 = leftistKey right
             v2                 = leftistValue right

leftistKey (Branch k Nil _ _)  = k
leftistKey (Branch _ left _ _) = leftistKey left

leftistValue (Branch _ Nil _ v)  = v
leftistValue (Branch _ left _ _) = leftistValue left

