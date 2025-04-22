-- Imports must be at the start of the file.
import Data.Function (on)
import Data.List
-- This enforces all functions in Data.Map to be called like this M.{funcName}, this is usually used to avoid clashes between functions that have the same name between different modules
import Data.Map qualified as M
import Data.Char
import qualified Data.Set as S

import qualified Geometry.Sphere as Sphere
import qualified Geometry.Cuboid as Cuboid -- Importing our own modules
import qualified Geometry.Cube as Cube

-- Functions (can't begin with capital letters)
doubleMe :: (Num a) => a -> a
doubleMe x = x + x

tripleMe :: (Num a) => a -> a
tripleMe x = x + doubleMe x

-- Definitions (It's just a function without parameters)
myAge :: Integer
myAge = 23

-- if statements (else is mandatory)
doubleSmallNumber :: (Ord a, Num a) => a -> a
doubleSmallNumber x =
  if x > 100
    then x
    else x * 2

-- lists (homogenous, strings are lists)
myList :: [Integer]
myList = [0, 1, 2, 3, 4, 5]

modifiedList :: [Integer]
modifiedList = myList ++ [6, 7, 8] -- ++ makes haskell walk through the whole left list

myName :: String
myName = "any"

modifiedName :: String
modifiedName = 'H' : myName -- but cons operator is instantaneous

listSugar :: Bool
listSugar = [1, 2, 3] == 1 : 2 : 3 : [] -- list literal is syntatic sugar to list plus cons operator fro elements

specificInt :: Integer
specificInt = myList !! 3

nestedLists :: [[Double]]
nestedLists = [[1.5, 2.4, 3], [5.5, 6.4, 8.1], [2.6, 9.9, 5.7]]

comparingLists :: Bool
comparingLists = nestedLists > [[5, 3, 2]] -- lexicographical order, doesn't care about length of lists

sumList :: Double
sumList = sum (nestedLists !! 1)

productList :: Double
productList = product (last nestedLists)

elementExists :: (Eq a) => a -> ([[a]] -> Bool) -- here we we take a number and return a function that takes a 2D list and return a boolean
elementExists x = any (elem x) -- elem checks if element exists in array and any applies it to each sublist

rangeIntList :: [Integer]
rangeIntList = [1 .. 20] -- should not use floating point numbers in range as they are not precise

rangeCharList :: String
rangeCharList = ['A' .. 'z']

infiniteList :: [Integer]
infiniteList = [13, 26 ..] -- Haskell is lazy so it only generates what it needs, so we can use take to specify number of elements

infiniteCycleList :: [Integer]
infiniteCycleList = cycle [1, 2, 3]

infiniteRepeatList :: [Integer]
infiniteRepeatList = repeat 5

-- List Comprehensions

listComp :: [Integer]
listComp = [x * 2 + 5 | x <- [0 .. 10]]

listCompMod :: [Integer]
listCompMod = [x | x <- [50 .. 100], x `mod` 7 == 3]

boomBangs :: (Integral a) => [a] -> [String]
boomBangs xs = [if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]

vecProductComb :: (Num a) => [a] -> [a] -> [a]
vecProductComb xs ys = [x * y | x <- xs, y <- ys]

removeNonUppercase :: String -> String
removeNonUppercase st = [c | c <- st, c `elem` ['A' .. 'Z']]

nestedListComp :: (Integral a) => [[a]] -> [[a]]
nestedListComp xxs = [[x | x <- xs, even x] | xs <- xxs]

-- tuples (non-homogenous, fixed size)

tuplesList :: [(Integer, Integer)]
tuplesList = [(1, 2), (3, 4), (5, 6)]

primitvePythagoreanTriples :: (Integral c) => c -> [(c, c, c)]
primitvePythagoreanTriples x = [(a, b, c) | c <- [1 .. x], b <- [1 .. c], a <- [1 .. b], a ^ 2 + b ^ 2 == c ^ 2, gcd a (gcd b c) == 1]

-- Types and TypeClasses (static typing, type inference)

addThreeInt :: Int -> Int -> Int -> Int -- Currying leads to that weird type expression, last Int is the return type, the other are types of parameter values
addThreeInt x y z = x + y + z

lessThanTen :: (Ord a, Num a) => a -> Bool -- Typeclasses are like interfaces they describe behavior of a type variable
lessThanTen x = x < 10

fVarRead :: Float
fVarRead = (read "5" :: Float) * 4 -- explicit type annotations is used when compiler can't infer type.

-- Pattern Matching (Very useful for recursive functions)

lucky :: (Integral a) => a -> String
lucky 7 = "Congrats, you're lucky!"
lucky x = "Sorry, you're unlucky!"

factorial :: (Integral a) => a -> a
factorial 0 = 1 -- most specific conditions always first as pattern matching works top down
factorial n = n * factorial (n - 1)

add2DVectors :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
add2DVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) -- pattern matching can be used to deconstruct (and bind to another variables) arguments

listCompPatMat :: (Num a) => [(a, a)] -> [a]
listCompPatMat xs = [a + b | (a, b) <- xs]

head' :: [a] -> a
head' [] = error "Can't call head on an empty list." -- generates a runtime error
head' (x : _) = x -- when bindng with more than one variable we have to surround them in parantheses, this is not to be confused with tuples

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell [x] = "The list has one element: " ++ show x
tell [x, y] = "The list has two elements: " ++ show x ++ " and " ++ show y
tell (x : y : z : _) = "This list is long. The first three elements are: " ++ show x ++ " , " ++ show y ++ " and " ++ show z

length' :: (Integral b) => [a] -> b
length' [] = 0
length' (_ : xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a -- This can be replaced with foldr as we will see later
sum' [] = 0
sum' (x : xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x : xs) = "The first letter of " ++ all ++ " is " ++ [x] -- you can define a pattern using @ to access a whole structure after deconstructing it.

-- Guards (Usually used with pattern matching, much like big if else trees)

-- It's better not to use inline guards for bigger functions
max' :: (Ord a) => a -> a -> a -- Beware no = sign after function name and params only after conditions
max' a b | a > b = a | otherwise = b -- otherwise is a catch all condition

-- Where Clauses (syntactic construct, higher scope than let)

bmiTell :: (Fractional a, Ord a) => a -> a -> String
bmiTell weight height
  | bmi <= 18.5 = "You're underweight, you emo, you!"
  | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"
  | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"
  | otherwise = "You're a whale, congratulations!"
  where
    bmi = weight / height ^ 2 -- where is visible across the guards but not across different patterns

initials :: String -> String -> String -- Actually using pattern matching here is more concise but this is just a demo of using where clauses.
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where
    (f : _) = firstname
    (l : _) = lastname

calcBmisV1 :: (Fractional a) => [(a, a)] -> [a]
calcBmisV1 xs = [bmi w h | (w, h) <- xs]
  where
    bmi w h = w / h ^ 2

-- Let Clauses (actual expressions but local)

cylinder :: (Floating a) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
   in sideArea + 2 * topArea -- let bindings are only available in the block next to the in keyword

numLet :: Integer
numLet = 4 * (let a = 9 in a + 1) + 2

listLet :: [(Integer, Integer, Integer)]
listLet = [let square x = x * x in (square 5, square 3, square 2), let cube x = x ^ 3 in (cube 5, cube 3, cube 2)]

listLet2 :: (Integer, [Char])
listLet2 = (let a = 100; b = 200; c = 300 in a * b * c, let foo = "Hey "; bar = "there!" in foo ++ bar)

listLet3 :: Integer
listLet3 = (let (a, b, c) = (1, 2, 3) in a + b + c) * 100

calcBmisV2 :: (Fractional a) => [(a, a)] -> [a] -- Visibility of let is in all parts of list comp. except defining vars from arguments, so we don't need the in part
calcBmisV2 xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- Case Expressions (Actually Pattern Matching is just syntactic sugar for case expressions, can be used in the middle of expressions)

head'' :: [a] -> a
head'' xs = case xs of
  [] -> error "No head for empty lists!"
  (x : _) -> x

describeList :: [a] -> String -- Case expressions are more general and flexible than using pattern matching on function params
describeList xs =
  "The list is " ++ case xs of
    [] -> "empty."
    [x] -> "a singleton list."
    xs -> "a longer list."

-- Recursion

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x : xs)
  | x > maxTail = x
  | otherwise = maxTail
  where
    maxTail = maximum' xs

maximum'' :: (Ord a) => [a] -> a
maximum'' [] = error "maximum of empty list"
maximum'' [x] = x
maximum'' (x : xs) = max x (maximum' xs)

replicate' :: (Ord t1, Num t1) => t1 -> t2 -> [t2]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: (Ord t, Num t) => t -> [a] -> [a]
take' n _
  | n <= 0 = [] -- note there is no otherwise part here as we want it to still pattern match if n is > 0
take' _ [] = []
take' n (x : xs) = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x : xs)
  | a == x = True
  | otherwise = a `elem'` xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x : xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
   in smallerSorted ++ [x] ++ biggerSorted

-- Higher Order Functions (A function that takes a function as parameter or returns a function)

-- Currying (we partially apply functions and each param is evaluated separatly)
multThree :: (Num a) => a -> (a -> (a -> a))
multThree x y z = x * y * z

multNine :: (Num a) => a -> (a -> a)
multNine = multThree 9

multTwoWithNine :: (Num a) => a -> a
multTwoWithNine = multNine 2

multTwoWithNineWithThree :: (Num a) => a
multTwoWithNineWithThree = multTwoWithNine 3

divideByTen :: (Floating a) => a -> a
divideByTen = (/ 10) -- Infix functions can be partially applied by using sections

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A' .. 'Z'])

applyTwice :: (t -> t) -> t -> t -- Here parantheses in the type declaration are required to show that the first parameter is actually a function
applyTwice f x = f (f x)

zipWith' :: (t1 -> t2 -> t3) -> [t1] -> [t2] -> [t3]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (t1 -> t2 -> t3) -> t2 -> t1 -> t3 -- Here we don't need parantheses in the right side as -> is right associative
flip' f x y = f y x -- and this type expression is equivalen to (t1 -> t2 -> t3) -> (t2 -> (t1 -> t3))

flippedListDivide :: (Integral a) => [a] -> [a] -> [a]
flippedListDivide = zipWith (flip div)

-- Maps and Filters

square2DList :: (Num a) => [[a]] -> [[a]]
square2DList = map (map (^ 2)) -- inner map is partially applied and returns a function with this type [a] -> [a]
-- outer map applies that function to each element of the list which itself is a list and that's why the double map works for 2D Lists

listNotNull :: [[a]] -> [[a]]
listNotNull = filter (not . null) -- Here we filter a 2D list and remove all the empty 1D lists inside using function composition (.) which we will get to later.

largestDivisible :: (Integral a) => [a] -> a
largestDivisible l = head (filter p l)
  where
    p x = x `mod` 3829 == 0

sumSmallOddSquares :: (Integral a) => [a] -> a
sumSmallOddSquares l = sum (takeWhile (< 10000) (filter odd (map (^ 2) l)))

-- can also be written like this :
sumSmallOddSquares' :: (Integral a) => [a] -> a
sumSmallOddSquares' l = sum (takeWhile (< 10000) [sq | n <- l, let sq = n ^ 2, odd sq])

collatzChain :: (Integral t) => t -> [t]
collatzChain 1 = [1]
collatzChain n
  | even n = n : collatzChain (n `div` 2)
  | otherwise = n : collatzChain (n * 3 + 1)

longCollatzChains :: (Integral t) => [t] -> Int
longCollatzChains l = length (filter (\x -> length x > 15) (map collatzChain l)) -- Here we use lamda functions to refrence elements of the on-the-fly created list (map collatzChain l).

listOfFuncs :: (Integral t) => [t -> t]
listOfFuncs = map (*) [0 ..] -- We could also create a list of partially applied functions like this where it will create this [0*, 1*, 2* ..]

addDeZip :: (Num t) => [(t, t)] -> [t]
addDeZip = map (\(a, b) -> a + b) -- Pattern matching can be used in lambdas

-- Folds (reduce lists to some single value, takes binary function, accumulator, and a list)

sum'' :: (Num a) => [a] -> a
sum'' = foldl (+) 0

elem'' :: (Foldable t, Eq a) => a -> t a -> Bool
elem'' y = foldl (\acc x -> (x == y) || acc) False -- starting value and accumulator are the same type

-- foldl has the accumulator as the first parameter (\acc x -> ..), while foldr has the accumulator as the second parameter (\x acc -> ..)
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) [] -- usually foldr is used with lists where we want to preserve order to use the cons operator (:) instead of appending elements (++) as it's more efficient
-- Also foldr can work with infinite lists but not foldl

reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- scanl and scanr are like fold but report all the intermediate accumulator states
sqrtSums :: Int
sqrtSums = length (takeWhile (< 1000) (scanl (+) 0 (map sqrt [1 ..])))

-- Function application operator ($) type: (a -> b) -> a -> b

-- $ has the lowest precedence and is right associative unlike normal func. applic. which is left-associative
-- When a $ is encountered, the expression on its right is applied as the parameter to the function on its left.
-- It can also be used to map function application over a list of functions

funcAppList :: [Double]
funcAppList = map ($ 3) [(+ 4), (* 10), (^ 2), sqrt] -- This creates this list [($3) (+4) = (+4) $ 3 = 3+4, ($3) (*10) = (*10) $ 3 = 3*10, ($3) (^2) = (^2) $ 3 = 3^2, ($3) sqrt = sqrt $ 3 = sqrt 3]
-- Remember partially applying infix functions using sections this is what creates the confusing order that happens here.

applyListOfFuncs :: (Integral t) => [t]
applyListOfFuncs = zipWith ($) listOfFuncs [1, 5, 6, 8, 9] -- This creates this list [($) (*) 0 1, ($) (*) 1 5, ..]

-- Function Composition operator (.) type : (b -> c) -> (a -> b) -> (a -> c)
multByNegThree = negate . (3 *)

turnListNeg = map (negate . abs)

fn x = ceiling (negate (tan (cos (max 50 x))))

-- can be transformed to
fn' = ceiling . negate . tan . cos . max 50 -- point free style

dotProduct :: (Num a) => [a] -> [a] -> a
dotProduct = (sum .) . zipWith (*) -- Nested composition can be used to compose functions that have more than one parameter like zipWith

-- Modules (Prelude is imported by default and contains common functions and types)
-- We can selectively import functions from a module like this: import Data.List (nub, sort)
-- We can import a module while excluding specific functions like this: import Data.List hiding (nub)

numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub -- nub weeds out duplicate elements

-- Data.List has many interesting functions you can use Hoogle (https://hoogle.haskell.org/) to search for modules and functions

sortByLength2D :: [[a]] -> [[a]]
sortByLength2D = sortBy (compare `on` length) -- On is a function in Data.Function that does this: f `on` g = \x y -> f (g x) (g y)

encode ::  Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in  map chr shifted

encode' :: Int -> String -> String
encode' shift = map (chr . (+ shift) . ord) -- Point-Free Style

decode :: Int -> String -> String
decode shift = encode (negate shift)

-- Data.Map is the equivalent to dictionaries and uses trees internally.
basicMap = M.fromList [("betty","555-2938"),("bonnie","452-2928"),("lucille","205-2928")]

createListedMap :: Ord k => [(k, a)] -> M.Map k [a]
createListedMap = M.fromListWith (++) . map (\(k,v) -> (k,[v])) -- This creates a map from a list that has duplicate keys by creating lists in the value and adding each value that had a duplicate key to that list.

-- Data.Set is an ordered Set that uses trees internally.
basicSet = S.fromList "I found out that I am out of this world."

-- Creating Our Own Modules (We create folders and files and usually use qualified imports, check Geometry folder for details)

-- Types and Typeclasses

-- data Bool = False | True (This is how the Bool data type is defined)
-- data Int = -2147483648 | .. | 2147483647 (This is how the Int data type is approximatly defined)

-- To create our own data type:
-- Here Point is a data type and also a value constructor which are functions that take fields and return a value of our data type
data Point = Point Float Float deriving (Show) -- deriving (Show) makes our data type part of the Show typeclass so it can be printed in the console.
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)
-- Shape is our data type, Circle and Rectangle are value constructors, and we used our previously defined type point to abstract away the two Floats
-- :t Circle => Circle :: Float -> Float -> Float -> Shape    :t Rectangle => Rectangle :: Float -> Float -> Float -> Float -> 

surface :: Shape -> Float -- Here we pattern match against constructors which we actually do all the time with values like True and 5 as they are constructors with no fields.
surface (Circle _ r) = pi * r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs (x2 - x1) * abs (y2 - y1)

-- We can use value constructors as any other function
concentricCircles :: [Shape]
concentricCircles = map (Circle $ Point 10 20) [4, 5, 6, 7]

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x+a) (y+b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- You can conisder those like default constructors where the center point of the shape is the zero coordinate point
baseCircle :: Float -> Shape
baseCircle = Circle (Point 0 0)

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Point 0 0) (Point width height)
-- To export types we can use this Shape (..) this exports all value constructors for Shape (Circle, Rectangle) we could export only some value constructors or none by using Just Shape this forces us to use the baseCircle and baseRect to create default shapes
-- not exporting the value constructors of a data type makes them more abstract and we can't pattern match against them if imported in another module

-- Records (an alternative way of writing data types that is more readable and automatic as it creates functions that lookup fields in the data type)
-- Usually we use the record syntax when a constructor has several fields and it's not ovious which is which
data Person = Person {
  firstName :: String,
  lastName :: String,
  age :: Int,
  height:: Float,
  phoneNumber :: String,
  flavor :: String
} deriving (Show)

-- Type Constructors (Like Value Constructors take some value parameters and produce a new value, type constructors take type parameters to produce new types)
-- This is an example of a type constructor with type parameters:  data Maybe a = Nothing | Just a  (Nothing and Just are type constructors, and a is the type parameter)
-- :t Nothing => Nothing :: Maybe a   as you can see Nothing is polymorphic and we can use it as a parameter with any maybe type like maybe Int or Maybe Char
-- The list type is a type constructor as it takes a type parameter to produce a concrete type like [Int] or [Char]
-- :t [] => Nothing :: Maybe a   It's also polymorphic

-- Using Type Constructors is like generics in Java or templates in C++

-- Here the Vector in the LHS is the type Constructor with one parameter, while the Vector in the RHS is the value constructor with 3 parameters
data Vector a = Vector a a a deriving (Show) -- We can use 3D vector with any type
-- We don't usually put typeclass constrains in data declarations as we will already do them in functions and we could benefit from the polymorphic nature of the declaration
-- Here we restrict the use of 3D vectors with Numbers as they are used when doing sum and product
vectPlus :: (Num t) => Vector t -> Vector t -> Vector t
vectPlus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

vectMult :: (Num t) => Vector t -> t -> Vector t
vectMult (Vector i j k) m = Vector (i*m) (j*m) (k*m)

vectDot :: (Num t) => Vector t -> Vector t -> t
vectDot (Vector i j k) (Vector l m n) = i*l + j*m + k*n

-- Derived Instances
-- Haskell can automatically make our type an instance of any of the following typeclasses: Eq, Ord, Enum, Bounded, Show, Read by using deriving keyworad after our data type
data Car = Car {
  company :: String,
  model :: String,
  year :: Int
} deriving (Eq, Show, Read)  -- Here when we derive the EQ instance for a type haksell will see if the value constructros match and all the data inside matches, but the types of all the fields must be part of the Eq typeclass

  -- data Bool = False | True deriving (Ord) here since False is defined before True then False < True
  -- same with data Maybe a = Nothing | Just a  Nothing < Just anything, but if we use Just 3 `compare` Just 2 then we compare the two values, but the value of just must be an instance of Ord to compare them

-- All value constructors are nullary (no parameters) so it can be part of the Enum typeclass which will make it have a predecessor and a successor
data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
            deriving (Eq, Ord, Show, Read, Bounded, Enum) -- Bounded are for things that have a lowest and highest possible value

-- Type synonyms (Equivalent types)
-- Example type String = [Char]

-- We could also do this:
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name,PhoneNumber)]

-- Type synonyms can also be parameterized
type AssocList k v = [(k,v)]

-- We could partially apply type parameters just like functions
type IntMap = M.Map Int -- This is the same as type IntMap v = M.Map Int v

-- Either data type is used when we want to encapsulate something that can fail in different ways and we want to show the failures not just use nothing like Maybe does
-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)   This is how it's roughly defined Left is for failing results and Right for correct results

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = M.Map Int (LockerState, Code)

lockers :: LockerMap
lockers = M.fromList 
    [(100,(Taken,"ZD39I"))
    ,(101,(Free,"JAH3I"))
    ,(103,(Free,"IQSA9"))
    ,(105,(Free,"QOTSA"))
    ,(109,(Taken,"893JJ"))
    ,(110,(Taken,"99292"))
    ]

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = 
    case M.lookup lockerNumber map of 
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
        Just (state, code) -> if state /= Taken 
                                then Right code
                                else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"

-- We used Either here to know why we couldn't get the code is it because there is no locker with this number or is it already taken


-- Recursive data structures

-- Here we create our own recursive List type
data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
-- We could use it like this Cons 4 (Cons 5 Empty) or like this to resemble the infix : cons operator   4 `Cons` (5 `Cons` Empty)

-- Here we can create an infix value constructor and we define their assosication and order using fixity declaratins
infixr 5 :-:
data ListInfix a = EmptyInf | a :-: (ListInfix a) deriving (Show, Read, Eq, Ord)
-- We could use it like this 3 :-: 4 :-: 5 :-: EmptyInf

-- Here we create our own adding lists operator
infixr 5  .++
(.++) :: ListInfix a -> ListInfix a -> ListInfix a 
EmptyInf .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)
-- We could pattern match here because :-: is a value constructor just like the regular cons :

-- We'll create our own binary tree recursive type
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Utility function that creats a singleton tree
treeSingleton :: a -> Tree a
treeSingleton x = Node x EmptyTree EmptyTree

-- Function that inserts an element in the tree
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = treeSingleton x
treeInsert x (Node a left right) 
    | x == a = Node x left right
    | x < a  = Node a (treeInsert x left) right
    | x > a  = Node a left (treeInsert x right)

-- Function that checks whether element exists in the tree
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a left right)
  | x == a = True
  | x < a = treeElem x left
  | x > a = treeElem x right

-- Function that takes a list and creates a tree from it
initTree :: (Ord a) => [a] -> Tree a
initTree = foldr treeInsert EmptyTree


-- TypeClasses 102

-- This is how the Eq typeclass is defined
-- class Eq a where
--    (==) :: a -> a -> Bool
--    (/=) :: a -> a -> Bool
--    x == y = not (x /= y)
--    x /= y = not (x == y)

{-
  When we use class we are defining a typeclass and the parameter is our type variable,
  and we don't need to implement functions inside just like an interface we only define a contract,
  with functions having type declarations, but in this case we implemented them with mutual recursion.
-} 

data TrafficLight = Red | Yellow | Green

-- this makes our type an instance of the Eq typeclass, this can help us doing custom equality check logic and not using the default way of using deriving.
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

{- since our Eq type class used mutual recursion then we can implement only one of those functions
   == , /= , but if there was no implementation then we had to implement both functions when making
   a type an instance of the Eq typeclass, this is called the minimal complete definition
-}

-- Again we are using custome instance to modify the default implementation of deriving show
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- We could also have typeclasses that are subclasses of other typeclasses like the Num declaration: class (Eq a) => Num a where ...

-- Here we want to make an instance of a non-concrete type like maybe so we introduce a type variable m and we make sure that implements the Eq typeclass
{-
  instance (Eq m) => Eq (Maybe m) where
    Just x == Just y = x == y
    Nothing == Nothing = True
    _ == _ = False
-}

-- class constraints in class declarations are used for making a typeclass a subclass of another typeclass and class constraints in instance declarations are used to express requirements about the contents of some type

-- implementing javascript-ish weak typing

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno :: Int -> Bool
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno :: [a] -> Bool
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno :: Bool -> Bool
    yesno = id  -- id is a function that takes a parameter and returns it exactly the same.

instance YesNo (Maybe a) where
    yesno :: Maybe a -> Bool
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where
    yesno :: Tree a -> Bool
    yesno EmptyTree = False
    yesno _ = True

instance YesNo TrafficLight where
    yesno :: TrafficLight -> Bool
    yesno Red = False
    yesno _ = True

{-  What we have done here is basically created a typeclass YesNo and a function yesno that returns true-ish and
    false-ish values and created instances for different concrete types and how would they return
    values if they implement the YesNo typeclass.
-}

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult

-- Functor typeclass

-- A functor typeclass is for things that can be mapped over like a list or a binary tree

-- It's implemented like this: 
{-
class Functor f where
    fmap :: (a -> b) -> f a -> f b
-}

-- Here f is not a concrete type but a type constructor that takes one type parameter

-- how lists are implemented as functors is like this
{-
instance Functor [] where
    fmap = map
-}
-- and map has the following type definition: map :: (a -> b) -> [a] -> [b]

-- here is how Maybe is implemented as a functor:
{-
instance Functor Maybe where
    fmap f (Just x) = Just (f x)
    fmap f Nothing = Nothing
-}

-- This is how our own tree implementation gets instanced as a functor:
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)


-- Now how to make the Either a b an instance of functor, the problem it doesn't have one but two type parameters,
-- we can partially apply Either by feeding it only one type parameter so it has a free parameter:
-- here we applied the type of the Left since it's the error or the empty box and we don't need it's type
{-
instance Functor (Either a) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x
-}

-- Kinds of Types

-- There is something called a kind of a type and we can find the kind of a type in ghci using :k

-- in the case of :k Int it returns Int :: * which means that it is a concrete type
-- in the case of :k Maybe it returns Maybe :: * -> * which means that that the Maybe type constructor takes one concrete type and then returns a concrete type
-- doing :k Maybe Int return Maybe Int :: * since it's now a concrete type because we provided the type parameter to Maybe
-- in the case of :k Either it returns Either :: * -> * -> it means that it takes two concrete types as type parameters to produce a concrete type
-- type constructors are curried like functions and can be partially applied just like we did in the Functor instance of Either
