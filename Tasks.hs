module Tasks where
  import Data.Char
  import Data.Function
  import Data.Time.Clock
  import Data.Time.Format
  import System.Locale
  import Data.Functor

  lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

  sign x = if x > 0 then 1 else if x < 0 then -1 else 0

  infixl 6 |-|
  x |-| y = abs (x - y)

  twoDigits2Int :: Char -> Char -> Int
  twoDigits2Int x y = if isDigit x && isDigit y then (digitToInt x * 10) + digitToInt y else 100

  dist :: (Double, Double) -> (Double, Double) -> Double
  dist p1 p2 = sqrt ((fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2)


  fibonacci :: Integer -> Integer
  fibonacci n | n == 0      = 0
              | n == 1      = 1
              | n == (-1)   = 1
              | n > 1       = fibonacci(n-1) + fibonacci(n-2)
              | n < (-1)    = fibonacci(n+2) - fibonacci(n+1)
              | otherwise   = error "incorrect n"

  fibonacciAcc :: Integer -> Integer -> Integer -> Integer
  fibonacciAcc n prev current | n == 0        = prev
                              | n > 0         = fibonacciAcc (n-1) current (prev + current)
                              | n < 0         = fibonacciAcc (n+1) current (prev - current)
                              | otherwise     = error "incorrect n"

  fibonacci' :: Integer -> Integer
  fibonacci' n = fibonacciAcc n 0 1

  seqA :: Integer -> Integer
  seqA n | n == 0 = 1
         | n == 1 = 2
         | n == 2 = 3
         | n > 2 = let
                     seqAIn f s t n | n == 0 = f
                                    | otherwise = seqAIn (f + s - 2 * t) f s (n-1)
                   in seqAIn 3 2 1 (n-2)

  sum'n'count :: Integer -> (Integer, Integer)
  sum'n'count x = (sumX x 0, countX x 0) where
      sumX x acc    | x == 0      = acc
                    | x < 0       = sumX ((-1) * x) acc
                    | otherwise   = sumX (x `div` 10) (acc + x `mod` 10)
      countX x acc  | x < 0     = countX ((-1) * x) acc
                    | x < 10    = acc + 1
                    | otherwise = countX (x `div` 10) (acc + 1)

  integration :: (Double -> Double) -> Double -> Double -> Double
  integration f a b = let
    h = (b - a) / 1000
    f0 = f(a)
    fn = f(b)
    iterator ai acc | ai == 1000     = acc
                    | otherwise      = iterator (ai + 1) (acc + f(a + h * ai))
    in (h * ((f0 + fn) / 2 + iterator 1 0))

  getSecondFrom :: a -> b -> c -> b
  getSecondFrom x y z = y

  multSecond = (*) `on` snd

  on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
  on3 op f x y z = op (f x) (f y) (f z)

  doItYourself = f . g . h
  f = \x -> logBase 2 x
  g = \x -> x ^ 3
  h = \x -> max x 42

  class Printable a where
    toString :: a -> String

  instance Printable Bool where
    toString True = "true"
    toString False = "false"

  instance Printable () where
    toString () = "unit type"
    
  instance (Printable a, Printable b) => Printable (a, b) where
    toString p = '(' : toString (fst p) ++ "," ++ toString (snd p) ++ ")"

  class KnownToGork a where
      stomp :: a -> a
      doesEnrageGork :: a -> Bool

  class KnownToMork a where
      stab :: a -> a
      doesEnrageMork :: a -> Bool

  class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
      stompOrStab :: a -> a
      stompOrStab a | doesEnrageMork a && doesEnrageGork a              = stomp (stab a)
                    | not (doesEnrageMork a) && not (doesEnrageGork a)  = a
                    | not (doesEnrageMork a) && doesEnrageGork a        = stab a
                    | otherwise                                         = stomp a


  a = 127.2
  b = 24.1
  c = 20.1
  d = 2
  ip = show a ++ show b ++ show c ++ show d

  class (Bounded a, Enum a, Eq a) => SafeEnum a where
    ssucc :: a -> a
    ssucc a | a == maxBound   = minBound
            | otherwise       = succ a

    spred :: a -> a
    spred a | a == minBound   = maxBound
            | otherwise       = pred a

  avg :: Int -> Int -> Int -> Double
  avg x y z = (fromIntegral x + fromIntegral y + fromIntegral z) / 3.0

  addTwoElements :: a -> a -> [a] -> [a]
  addTwoElements x y list = x : y : list

  nTimes:: a -> Int -> [a]
  nTimes e n = let
    nTimesInner e n lst | n == 0    = lst
                        | n > 0     = nTimesInner e (n-1) (e : lst)
                        | otherwise = error "Incorect n"
    in (nTimesInner e n [])

  oddsOnly :: Integral a => [a] -> [a]
  oddsOnly xs = let
    oddsOnlyInner [] os       = reverse os
    oddsOnlyInner (i : is) os | odd i     = oddsOnlyInner is (i : os)
                              | otherwise = oddsOnlyInner is os
    in (oddsOnlyInner xs [])

  isPalindrome :: Eq a => [a] -> Bool
  isPalindrome xs = xs == reverse xs

  sum3 :: Num a => [a] -> [a] -> [a] -> [a]
  sum3 (x:xs) (y:ys) (z:zs) = (x + y + z) : sum3 xs ys zs
  sum3 (x:xs) []      []    = x           : sum3 xs [] []
  sum3 []     (y:ys)  []    = y           : sum3 [] ys []
  sum3 []     []      (z:zs)= z           : sum3 [] [] zs
  sum3 (x:xs) []      (z:zs)= x + z       : sum3 xs [] zs
  sum3 (x:xs) (y:ys)  []    = x + y       : sum3 xs ys []
  sum3 []     (y:ys)  (z:zs)= y + z       : sum3 [] ys zs
  sum3 []     []      []    = []

  groupElems :: Eq a => [a] -> [[a]]
  groupElems (x:xs) = let
    spanLst = span (==x) (x:xs)
    in (fst spanLst : groupElems (snd spanLst))
  groupElems []     = []

  readDigits :: String -> (String, String)
  readDigits xs = span (isDigit) xs

  filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
  filterDisj p1 p2 (x:xs) | p1 x = x : filterDisj p1 p2 xs
                          | p2 x = x : filterDisj p1 p2 xs
                          | otherwise = filterDisj p1 p2 xs
  filterDisj _ _ [] = []

  qsort :: Ord a => [a] -> [a]
  qsort (x : xs) = let
    spanLst = (filter ( < x) xs, filter (>= x) xs)
    in (qsort (fst spanLst) ++ [x] ++ qsort (snd spanLst))
  qsort [] = []

  squares'n'cubes :: Num a => [a] -> [a]
  squares'n'cubes xs = concatMap (\x -> [x^2, x^3]) xs

  delAllUpper :: String -> String
  delAllUpper = unwords . filter (not . all isUpper) . words

  max3 :: Ord a => [a] -> [a] -> [a] -> [a]
  max3 x y z = let
    zipWithMax = zipWith max
    in (zipWithMax (zipWithMax x y) z)

  fibStream :: [Integer]
  fibStream = 0 : 1 : zipWith (+) (fibStream) (tail fibStream)

  concatList :: [[a]] -> [a]
  concatList = foldr (++) []

  lengthList :: [a] -> Int
  lengthList = foldr (\x s -> s + 1) 0

  sumOdd :: [Integer] -> Integer
  sumOdd = foldr (\x s -> if odd x then s + x else s) 0

  meanList :: [Double] -> Double
  meanList xs = foldr (+) 0 xs / fromIntegral(length xs)

  evenOnly :: [a] -> [a]
  evenOnly xs = foldr (\x s -> if even (snd x) then (fst x) : s else s) [] (zip xs [1..])

  second x y = y
  lastElem = foldl1 second

  data Color = Red | Green | Blue

  instance Show Color where
      show Red = "Red"
      show Green = "Green"
      show Blue = "Blue"

  charToInt :: Char -> Int
  charToInt '0' = 0
  charToInt '1' = 1
  charToInt '2' = 2
  charToInt '3' = 3
  charToInt '4' = 4
  charToInt '5' = 5
  charToInt '6' = 6
  charToInt '7' = 7
  charToInt '8' = 8
  charToInt '9' = 9

  stringToColor :: String -> Color
  stringToColor "Red" = Red
  stringToColor "Green" = Green
  stringToColor "Blue" = Blue
  stringToColor _ = error "Incorrect color"

  data LogLevel = Error | Warning | Info
  cmp :: LogLevel -> LogLevel -> Ordering
  cmp Error Error = EQ
  cmp Error _ = GT
  cmp _ Error = LT
  cmp Warning Warning = EQ
  cmp Warning _ = GT
  cmp _ Warning = LT
  cmp Info Info = EQ

  data Result = Fail | Success

  data Point = Point Double Double

  origin :: Point
  origin = Point 0.0 0.0

  distanceToOrigin :: Point -> Double
  distanceToOrigin (Point x y) = sqrt (x ^ 2 + y ^ 2)

  distance :: Point -> Point -> Double
  distance (Point x1 y1) (Point x2 y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

  data Shape = Circle Double | Rectangle Double Double

  area :: Shape -> Double
  area (Circle r)       = pi * (r ^ 2)
  area (Rectangle a b)  = a * b


  square :: Double -> Shape
  square a = Rectangle a a

  isSquare :: Shape -> Bool
  isSquare (Rectangle a b) = a == b
  isSquare _ = False

  timeToString :: UTCTime -> String
  timeToString = formatTime defaultTimeLocale "%a %d %T"

  data LogEntry = LogEntry { timestamp :: UTCTime, logLevel :: LogLevel, message :: String }

  logLevelToString :: LogLevel -> String
  logLevelToString Error = "Error"
  logLevelToString Warning = "Warning"
  logLevelToString Info = "Info"

  logEntryToString :: LogEntry -> String
  logEntryToString (LogEntry time level mes) = (timeToString time) ++ ": " ++ (logLevelToString level) ++ ": " ++ mes

  data Person = Person { firstName :: String, lastName :: String, age :: Int }

  updateLastName :: Person -> Person -> Person
  updateLastName person1 person2 = person2 { lastName = lastName person1 }

  abbrFirstName :: Person -> Person
  abbrFirstName p = p { firstName = let
   name  = firstName p
   in (if 2 > length name then name else name !! 0 : "." ) }

  data Coord a = Coord a a

  distance' :: Coord Double -> Coord Double -> Double
  distance' (Coord x1 y1) (Coord x2 y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)

  manhDistance' :: Coord Int -> Coord Int -> Int
  manhDistance' (Coord x1 y1) (Coord x2 y2) = abs(x2 - x1) + abs(y2 - y1)

  findDigit :: [Char] -> Maybe Char
  findDigit x = let
    digit = dropWhile (not . isDigit) x
    in (if (length digit == 0) then Nothing else Just (head digit))

  findDigitOrX :: [Char] -> Char
  findDigitOrX s = case findDigit s of
    Nothing -> 'X'
    Just x -> x

  maybeToList :: Maybe a -> [a]
  maybeToList (Nothing) = []
  maybeToList (Just x)  = [x]

  listToMaybe :: [a] -> Maybe a
  listToMaybe [] = Nothing
  listToMaybe xs = Just (head xs)

  eitherToMaybe :: Either a b -> Maybe a
  eitherToMaybe (Left a) = Just a
  eitherToMaybe (Right _) = Nothing

  data List a = Nil | Cons a (List a)

  fromList :: List a -> [a]
  fromList Nil = []
  fromList (Cons x y) = x : fromList(y)


  toList :: [a] -> List a
  toList [] = Nil
  toList (x:xs) = Cons x (toList xs)

  data Nat = Zero | Suc Nat deriving Show

  fromNat :: Nat -> Integer
  fromNat Zero = 0
  fromNat (Suc n) = fromNat n + 1

  add :: Nat -> Nat -> Nat
  add Zero n  = n
  add n Zero  = n
  add (Suc x) y = add x (Suc y)

  mul :: Nat -> Nat -> Nat
  mul Zero _  = Zero
  mul _ Zero  = Zero
  mul x y     = mulInner x y Zero where
    mulInner Zero n acc   = acc
    mulInner n Zero acc   = acc
    mulInner (Suc x) y acc      = mulInner x y (add acc y)

  fac :: Nat -> Nat
  fac x       = innerFac x (Suc Zero) where
    innerFac Zero acc       = acc
    innerFac (Suc x) acc    = innerFac x (mul (Suc x) acc)

  data Point3D a = Point3D a a a deriving Show

  instance Functor Point3D where
      fmap f (Point3D a b c) = Point3D (f a) (f b) (f c)

  data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

  instance Functor Tree where
      fmap f (Leaf a)       = Leaf (f <$> a)
      fmap f (Branch l x r) = Branch (f <$> l) (f <$> x) (f <$> r)

  data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
  data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

  instance Functor (Entry k1 k2) where
      fmap f (Entry y x) = Entry y (f x)

  instance Functor (Map k1 k2) where
      fmap f (Map x) = Map (map (fmap f) x)