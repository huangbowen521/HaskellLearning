map' :: (a -> b) -> [a] -> [b]

map' f [] = []
map' f (x:xs) =f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]

filter' f [] = []
filter' f (x:xs) 
	| f x       = x : filter' f xs
	| otherwise = filter' f xs

head' (x:xs) = x

tail' (x:xs) = xs

last' (x:xs) 
	| null xs = x
	| otherwise = last' xs

init' (x:xs)
	| null xs = []
	| otherwise = x : init' xs

foldl' :: (a -> b -> a) -> a -> [b] -> a

foldl' f s [] = s
foldl' f s (x:xs) = foldl' f (f s x) xs 

foldr' :: (a -> b -> a) -> a -> [b] -> a
foldr' f s [] = s
foldr' f s (x:xs) = f (foldr' f s xs) x 

foldr2 f s [] = s
foldr2 f s (x:xs) = f s (foldl' f x xs) 

map2 :: (a -> b) -> [a] -> [b]

map2 f xs =foldl' (\s x -> s ++ [f x]) [] xs


map3 :: (a -> b) -> [a] -> [b]

map3 f xs =foldr' (\s x -> f x : s) [] xs

filter2 :: (a -> Bool) -> [a] -> [a]

filter2 f [] = []
filter2 f (x:xs) = foldl' (\s x -> if f x then s ++ [x] else s ) [] xs 
	
filter3 :: (a -> Bool) -> [a] -> [a]
filter3 f [] = []
filter3 f (x:xs) = foldr' (\s x -> if f x then x : s else s) [] xs

main = 
	--print $ foldl' (\s x -> s + x) 1 [2,3]
	--print $ map3 (\ x -> x) [1,2,3]
	print $ filter3 (\x -> x > 2) [1,3,5]


