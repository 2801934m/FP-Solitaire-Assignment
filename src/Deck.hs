module Deck where
import System.Random
import Data.List (sort)
import System.Console.ANSI (setSGRCode)
import System.Console.ANSI.Types
{- Card definitions and instances -}
data Suit = Spades | Clubs | Diamonds | Hearts
    deriving (Eq, Ord) -- Arbitrary ordering, doesn't really matter

data Value = 
      Ace | Two | Three | Four | Five | Six | Seven
    | Eight | Nine | Ten | Jack | Queen | King
    deriving (Eq, Enum, Bounded)

instance Ord Value where
    compare c1 c2 = compare (fromEnum c1) (fromEnum c2)


{- Card representation, creation, and helper functions -}
data Card = MkCard { cardSuit :: Suit, cardValue :: Value }
    deriving (Eq, Ord)

mkCard :: Suit -> Value -> Card
mkCard suit value = MkCard { cardSuit = suit, cardValue = value }

isRed :: Card -> Bool
isRed c = cardSuit c == Hearts || cardSuit c == Diamonds

isBlack :: Card -> Bool
isBlack = not . isRed


{- Card Show instances -}
redStr :: String -> String
redStr str = (setSGRCode [SetColor Foreground Vivid Red]) ++ str ++ (setSGRCode [])

instance Show Suit where
    show Spades   = "♠"
    show Clubs    = "♣"
    show Diamonds = redStr "♦"
    show Hearts   = redStr "♥"

instance Show Value where
    show Ace = "A"
    show Jack = "J"
    show Queen = "Q"
    show King = "K"
    show other = show (fromEnum other + 1)

instance Show Card where
    show card = padding ++ str
        where 
            padding = replicate (2 - length (show (cardValue card))) ' '
            str = show (cardSuit card) ++ show (cardValue card)

{- Deck construction -}

type Deck = [Card]

{- EXERCISE 1: Deck Creation -}

deckOf52 :: [Card]
deckOf52 = [mkCard x y | x <- [Spades, Clubs, Diamonds, Hearts], y <- [Ace .. King]]

{- You can use this to check whether your shuffled deck contains the correct
 - cards -}
deckCorrect :: Deck -> Bool
deckCorrect deck = sort deck == sort deckOf52

{- Incorrect deck to check deckCorrect function is accurate -}
incorrectDeck :: [Card]
incorrectDeck = zipWith mkCard [Spades, Clubs, Diamonds, Hearts] [Ace .. King]

{- Shuffling -}

{- EXERCISE 2: Fisher-Yates Shuffle 

To shuffle an array a of n elements (indices 0..n-1):
    
    for i from 0 to n - 2 do
         j <- random integer such that i <= j < n
         exchange a[i] and a[j]

-}
shuffle :: StdGen -> Deck -> Deck
shuffle rng [] = []
shuffle rng deck = go deck rng 0 (length deck)
  where
    go deck rng i n
      | i >= n - 1 = deck  -- Base case: Stop when i reaches n - 1
      | otherwise  =
          let (j, rng') = randomR (i, n - 1) rng  -- Generate random index j
              swappedDeck = swap deck i j         -- Swap elements at indices i and j
          in go swappedDeck rng' (i + 1) n       -- Recurse with updated deck and generator


swap :: [Card] -> Int -> Int -> [Card]
swap deck i j
  | i == j    = deck  -- No need to swap if indices are the same
  | otherwise = [ if k == i then deck !! j  -- Replace element at i with element at j
                  else if k == j then deck !! i  -- Replace element at j with element at i
                  else x  -- Leave all other elements unchanged
                | (x, k) <- zip deck [0..] ]  -- Pair each element with its index


{- shuffleDeck is called by Main.hs when setting up -}
shuffleDeck :: IO Deck
shuffleDeck = do
    gen <- initStdGen
    return $ shuffle gen deckOf52
