module Game where
import Deck
import Error

{- Commands and instructions, representing moves to be made -}
type StackIndex = Int
type Count = Int
type FromStack = StackIndex
type ToStack = Int

-- An instruction is a top-level command.
data Instruction = Quit | Undo | GameCommand Command

-- A Command is a move to be played in the game.
data Command = Move Count FromStack ToStack
             | MoveStack FromStack ToStack
             | Draw
             | MoveFromDiscard StackIndex
             | MoveToPillar CardSource
             | MoveFromPillar Suit StackIndex
             | Solve

data CardSource = FromStack StackIndex | FromDiscard

{- Board representation -}

-- A column is a list of (Card, Bool) pairs, where the Bool
-- represents visibility: true for visible, false for hidden
type Column = [(Card, Bool)]

-- The pillars / foundation stacks are represented as Maybe Card
-- values, where Nothing represents an empty pillar and Just c 
-- denotes that card 'c' is at the top of the pillar.
-- Note that we don't need to store all cards since they are stored
-- in ascending order.
data Pillars = MkPillars {
        spades :: Maybe Value,
        clubs :: Maybe Value,
        hearts :: Maybe Value,
        diamonds :: Maybe Value
  }
  deriving (Show, Eq)

emptyPillars :: Pillars
emptyPillars = MkPillars {
        spades = Nothing,
        clubs = Nothing,
        hearts = Nothing,
        diamonds = Nothing
    }

-- The board consists of a deck, discard pile, pillars, and 7 columns.
data Board = MkBoard {
    boardDeck :: [Card],
    boardDiscard :: [Card],
    boardPillars :: Pillars,
    boardColumns :: [Column]
}
    deriving (Eq)



{- EXERCISE 3: Show instance for the board -}
{- We recommend writing helper functions. -}
{-instance Show Board where
    show b = error "fill in 'Show' instance for Board data type in Game.hs" -}
instance Show Board where
    show board = separator ++ body ++ separator
        where 
            body = "Board:\n\nDeck size: " ++ show (length (boardDeck board)) ++
                   "\n\nDiscard: " ++ show (boardDiscard board) ++
                   "\n\nPillars:\n" ++ show (boardPillars board) ++
                   "\n\nColumns:\n" ++ formatColumnsSideBySide (boardColumns board)
            separator = "\n --------------------------------- \n"

-- Format columns side by side without padding
formatColumnsSideBySide :: [Column] -> String
formatColumnsSideBySide columns =
    unlines (headerRow : map formatRow [0..maxHeight - 1])
  where
    -- Find the maximum column height
    maxHeight = maximum (map length columns)

    -- Create the column headers
    headerRow = "  " ++ unwords (map (\i -> " " ++ show i ++ " ") [0 .. length columns - 1])

    -- Extract a row of cards from all columns, indexed by `rowIndex`
    formatRow rowIndex = "  " ++ unwords [formatCard (getCardAt col rowIndex) | col <- columns]

    -- Safely get a card at a specific index in a column
    getCardAt :: Column -> Int -> Maybe (Card, Bool)
    getCardAt col idx = if idx < length col then Just (col !! idx) else Nothing

    -- Format a single card, hiding its value if visibility is False
    formatCard :: Maybe (Card, Bool) -> String
    formatCard Nothing = "   "  -- Empty space for missing cards
    formatCard (Just (card, visible))
        | not visible = "???"
        | otherwise   = show card



{- EXERCISE 4: Board Setup -}
setup :: Deck -> Board
setup deck = MkBoard {
    boardDeck = remainingDeck,
    boardDiscard = [],
    boardPillars = emptyPillars,
    boardColumns = columns
  }
  where
    -- Deal cards for the columns
    (columns, remainingDeck) = dealColumns 1 [] deck

    -- Recursive function to deal cards for columns
    dealColumns :: Int -> [Column] -> Deck -> ([Column], Deck)
    dealColumns 8 cols remaining = (reverse cols, remaining) -- Stop when 7 columns are created
    dealColumns n cols remaining =
        let (colCards, rest) = splitAt n remaining  -- Take `n` cards for this column
            column = zip colCards (replicate (n - 1) False ++ [True]) -- Make last card visible
        in dealColumns (n + 1) (column : cols) rest



{- EXERCISE 5: Win checking -}
isWon :: Board -> Bool
isWon board =
    all (== Just King) [spades pillars, clubs pillars, hearts pillars, diamonds pillars]
  where
    pillars = boardPillars board


{- Pillar helper functions -}
-- Gets the pillar for a given suit.
getPillar :: Pillars -> Suit -> Maybe Value
getPillar ps Spades = spades ps
getPillar ps Clubs = clubs ps
getPillar ps Hearts = hearts ps
getPillar ps Diamonds = diamonds ps

-- Decrements a pillar. 
decValue :: Maybe Value -> Maybe Value
decValue Nothing = Nothing
decValue (Just Ace) = Nothing
decValue (Just x) = Just (pred x)

-- Increments a pillar.
incValue :: Maybe Value -> Maybe Value
incValue Nothing = Just Ace
incValue (Just x) = Just (succ x)

-- Increments the pillar for a given suit.
incPillar :: Pillars -> Suit -> Pillars
incPillar ps Spades = ps { spades = incValue (spades ps) }
incPillar ps Clubs = ps { clubs = incValue (clubs ps) }
incPillar ps Hearts = ps { hearts = incValue (hearts ps) }
incPillar ps Diamonds = ps { diamonds = incValue (diamonds ps) }

-- Decrements the pillar for a given suit.
decPillar :: Pillars -> Suit -> Pillars
decPillar ps Spades = ps { spades = decValue $ spades ps }
decPillar ps Clubs = ps { clubs = decValue $ clubs ps }
decPillar ps Hearts = ps { hearts = decValue $ hearts ps }
decPillar ps Diamonds = ps { diamonds = decValue $ diamonds ps }

{- EXERCISE 6: Helper Functions -}

-- Flips the top card of all columns, if not already flipped
flipCards :: Board -> Board
flipCards board = board { boardColumns = map flipTopCard (boardColumns board) }
  where
    -- Flip the top card of a column if it is not already flipped
    flipTopCard :: Column -> Column
    flipTopCard [] = []  -- An empty column remains empty
    flipTopCard (x:xs) = (card, True) : xs
      where (card, _) = x  -- Deconstruct the top card


-- Checks whether it's possible to stack the first card onto the second.
canStack :: Card -> Card -> Bool
canStack card onto = error "fill in 'canStack' in Game.hs"

-- Updates a column at the given index
updateColumn :: Int -> Column -> [Column] -> [Column]
updateColumn n c cs = error "fill in 'updateColumn' in Game.hs"

-- Checks whether it's possible to place a card onto a pillar.
canStackOnPillar :: Card -> Maybe Value -> Bool
canStackOnPillar c mv = error "fill in 'canStackOnPillar' in Game.hs"

{- EXERCISE 7: Draw -}
draw :: Board -> Either Error Board
draw b = error "fill in 'draw' in Game.hs" 

{- EXERCISE 8: Move -}
move :: Int -> Int -> Int -> Board -> Either Error Board
move count from to b = error "fill in 'move' in Game.hs"

{- EXERCISE 9: Move Stack -}
moveStack :: Int -> Int -> Board -> Either Error Board
moveStack from to b = error "fill in 'moveStack' in Game.hs"

{- EXERCISE 10: Move from Discard -}
moveFromDiscard :: Int -> Board -> Either Error Board
moveFromDiscard idx b = error "fill in 'moveFromDiscard' in Game.hs"

{- EXERCISE 11: Move to Pillar -} 
moveToPillar :: CardSource -> Board -> Either Error Board
moveToPillar cs b = error "fill in 'moveToPillar' in Game.hs"
            
{- EXERCISE 12: Move from Pillar -}
moveFromPillar :: Suit -> Int -> Board -> Either Error Board
moveFromPillar suit idx b = error "fill in 'moveFromPillar' in Game.hs"

{- EXERCISE 13: Solve -}
solve :: Board -> Board
solve board = error "fill in 'solve' in Game.hs"




{- Scaffolding: This checks input indexes and calls the relevant functions -}
checkStackIndex :: Int -> Either Error ()
checkStackIndex x | x >= 0 && x <= 6 = return ()
                  | otherwise = Left InvalidStack

makeMove' :: Command -> Board -> Either Error Board
makeMove' (Move count from to) b = do
    checkStackIndex from
    checkStackIndex to
    move count from to b
makeMove' (MoveStack from to) b = do
    checkStackIndex from
    checkStackIndex to
    moveStack from to b
-- If deck nonempty, move a card from the top of the deck to the top of the discard pile
-- If deck empty, reverse discard pile and put it back as deck
makeMove' Draw b = draw b
makeMove' (MoveFromDiscard idx) b = checkStackIndex idx >> moveFromDiscard idx b
-- Take the top card from the given stack and move to pillar -- if possible
makeMove' (MoveToPillar source) b =
    case source of
        FromDiscard -> moveToPillar source b
        FromStack idx -> checkStackIndex idx >> moveToPillar source b
makeMove' (MoveFromPillar suit idx) b = checkStackIndex idx >> moveFromPillar suit idx b
makeMove' Solve b = Right $ solve b

makeMove :: Command -> Board -> Either Error Board
makeMove cmd b = fmap flipCards (makeMove' cmd b)






-- SCRATCH AREA FOR CONSTRUCTING TEST BLOCKS AND CODE FRAGMENTS

discard = [mkCard Spades Two, mkCard Diamonds Ace]

testColumn = [(mkCard Spades Five, False), (mkCard Hearts Four, False), (mkCard Spades Three, False), (mkCard Diamonds Two, True)]

winningPillars = MkPillars (Just King) (Just King) (Just King) (Just King)



-- Create board instance with facedown card to test flipCards method
testFlipCards = MkBoard deckOf52 