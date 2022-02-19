-- Your code here!
import qualified Data.Map as Map
import Data.List
import Data.Char
import TestModule as Tm

--loop (x:xs) (y:ys) = (x:y) : loop x ys : loop xs (y:ys)
loop :: [a] -> [a] ->[(a,a)]
loop _ [] = []
loop [] _ = []
--loop _ [y] = [[y]]
loop (x:xs) (y:ys) = (x,y) : loop [x] ys ++ loop xs (y:ys)

digitSum::Int -> Int
digitSum n = sum $ map digitToInt $ show(n)
wickedNumber = find (\x -> digitSum x == 40) [1..]

dic = 
    [(1,"test")
    ,(2,"170-0000")
    ,(3,"175-0092")
    ,(4,"999-9999")]

myFoldr :: (a->b->b) -> b ->[a] -> b
myFoldr _ ini [] = ini
myFoldr f ini (x:xs) = f x $ myFoldr f ini xs  

my_close :: String -> (Int, String)
my_close (')':left) = (1,left)
my_close _          = error "no my_close parenthesis"

my_paren_seq :: String -> (Int, String)
my_paren_seq ('(':left0) = my_open_seq left0
my_paren_seq left0       = (0,left0)

my_open_seq :: String -> (Int, String)
my_open_seq left0 = (cnt1+cnt2, left2)
  where
    (cnt1,left1) = my_close     left0
    (cnt2,left2) = my_paren_seq left1

my_paren_rec :: String -> (Int, String)
my_paren_rec ('(':left0) = my_open_rec left0
my_paren_rec left0       = (0,left0)

my_open_rec :: String -> (Int, String)
my_open_rec left0 = (cnt1+cnt2, left2)
  where
    (cnt1,left1) = my_paren_rec left0
    (cnt2,left2) = my_close     left1

my_paren :: String -> (Int, String)
my_paren ('(':left0) = my_paren_rec left0
my_paren left0       = (0,left0)

my_gcd_fast :: Integer -> Integer -> Integer
my_gcd_fast a 0 = a
my_gcd_fast a b = my_gcd_fast b (a `mod` b)

my_lcm_fast :: Integer -> Integer -> Integer
my_lcm_fast a b = a `div` my_gcd_fast a b * b 

my_add x y = iter x y x
iter _ 0 acc = acc
iter x y acc = iter x (y-1) (acc+1)

pack [] = []
pack (x:xs) = (x:takeWhile (==x) xs) : pack (dropWhile (==x) xs)

encode xs = map (\a -> (length a,head a)) $ pack xs

--encodeModified :: [Char] -> [[Char]]
--encodeModified xs = map (\(n,v) -> if n > 1 then "Multiple " ++ show(n) ++ v else "Single " ++ v) $ encode xs
encodeModified xs = map (\(n,v) -> if n > 1 then "Multiple " ++ show(n) ++ [v] else "Single " ++ [v]) $ encode xs

split xs 0 = [[],(xs)]
split xs n = [(take n xs),(drop n xs)]
-- splitHelper (x:xs) 0 = []
-- splitHelper (x:xs) n = [x] ++ splitHelper xs (n-1)
slice  _ 0 0 = []
--slice (x:xs) 0 m = x : slice xs 0 (m-1)
slice (x:xs) n m = if n > 1 then slice xs (n-1) (m-1) else x : slice xs 0 (m-1)

removeAt n xs = (head $ drop (n-1) xs,take (n-1) xs++drop n xs)

--21
insertAt w xs n = take (n-1) xs ++ [w] ++ drop (n-1) xs
--22
range n m 
        | n > m = []
        | n <= m = n : range (n+1) m

main = do
    print $ insertAt 'X' "abcd" 2
    -- print $ removeAt 1 "abcd"
    -- print $ removeAt 2 "abcd"
    -- print $ removeAt 3 "abcd"
    -- print $ removeAt 4 "abcd"
    -- print $ slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
    -- print $ slice ['a','b','c','d','e','f','g','h','i','k'] 0 7
    -- print $ split "abcdefghik" 0
    -- print $ split "abcdefghik" 3
    -- print $ encodeModified "aaaabccaadeeee"
    --print $ pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
    --print $ zip [1..3] ['a','b','c']
    --print $ encode ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']

        
data Person = Person {
    firstName :: String
    , lastame :: String
    , age :: Int    
    } deriving (Eq,Show,Read)
    
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
    deriving (Eq,Ord,Show,Read,Bounded,Enum)

data LockerState = Taken | Free deriving (Show,Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)


lockers :: LockerMap
lockers = Map.fromList
    [(100,(Taken,"ZD390"))
    ,(101,(Free,"AD391"))
    ,(103,(Free,"BD393"))
    ,(104,(Free,"ZD394"))
    ,(105,(Taken,"ZD395"))
    ,(106,(Free,"ZD396"))
    ,(107,(Free,"ZD397"))
    ,(108,(Taken,"ZD398"))
    ]
{-lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp lockerNumber map = 
    case Map.lookup lockerNumber map of 
        Nothing -> Left $ "Locker " ++ show lockerNumber
        ++ " doesn't exist!"
        Just (state,code) -> if state \= Taken then Right code
        else Left $ "Locker " ++ show lockerNumber ++ "si already taken!"
-}

data List a = Empty | Cons a (List a) deriving (Show,Read,Eq,Ord)

data TrafficLight = Red | Green | Yellow

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Green = "Green light"
    show Yellow = "Yellow light"

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

sentence = "hey hey these are the words in this sentence"

mul _ 0 = 0
mul m n = m + mul m (n-1)

tails' [] = []
tails' (x:xs) = (x:xs) : tails' xs

findToX val = find (\x -> digitSum x == val) [0..]

--insert2 :: a -> [a] ->[a] ->[a]
--insert2 _ _ [] = []
--insert2 val (y:ys) (x:xs) = (y:ys:val:x:xs) : insert2 val (y:ys:x) xs 
--insert' val (x:xs) = (val:x:xs) : insert2 val x xs
