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
-- Should there be checks to make sure other structures are empty or is that redundant?



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
    -- Flip the last card in a column if it is not already flipped
    flipTopCard :: Column -> Column
    flipTopCard [] = []  -- An empty column remains empty
    flipTopCard col =
        let (initCards, [lastCard]) = splitAt (length col - 1) col
        in initCards ++ [reveal lastCard]

    -- Reveal a card (set its visibility to True)
    reveal :: (Card, Bool) -> (Card, Bool)
    reveal (card, _) = (card, True)




-- Checks whether it's possible to stack the first card onto the second.
canStack :: Card -> Card -> Bool
canStack c1 c2 = oppositeColor c1 c2 && oneLessValue c1 c2

-- Check if two cards are of opposite colors
oppositeColor :: Card -> Card -> Bool
oppositeColor c1 c2 = isRed c1 /= isRed c2

-- Check if the value of the first card is one less than the second card
oneLessValue :: Card -> Card -> Bool
oneLessValue c1 c2 = fromEnum (cardValue c1) + 1 == fromEnum (cardValue c2)


-- Updates a column at the given index
updateColumn :: Int -> Column -> [Column] -> [Column]
updateColumn idx c cs = take idx cs ++ [c] ++ drop (idx + 1) cs
-- Add guards for invalid input?

-- Checks whether it's possible to place a card onto a pillar.
canStackOnPillar :: Card -> Maybe Value -> Bool
canStackOnPillar c mv = case mv of
    Nothing        -> cardValue c == Ace
    Just pillarVal -> fromEnum (cardValue c) == fromEnum pillarVal + 1


{- EXERCISE 7: Draw -}
draw :: Board -> Either Error Board
draw board
    | not (null deck) =
        -- Deck is not empty: draw top card from deck
        Right board { boardDeck = tail deck, boardDiscard = head deck : discard }
    | null deck && not (null discard) =
        -- Deck is empty, but discard is not: reverse discard into a new deck
        let newDeck = reverse discard
        in Right board { boardDeck = tail newDeck, boardDiscard = head newDeck : [] }
    | otherwise =
        -- Both deck and discard are empty
        Left DeckEmpty
  where
    deck = boardDeck board
    discard = boardDiscard board


{- EXERCISE 8: Move -}
move :: Int -> Int -> Int -> Board -> Either Error Board
move n s1 s2 board
    | n < 1 = Left InvalidCount
    | length visibleCards < n = Left MovingTooManyCards
    | null column2 && not (isKing (fst (head visibleCards))) = Left ColumnKing
    | not (null column2) && not (canStack (fst (head visibleCards)) (fst (last column2))) = Left WrongOrder
    | otherwise = Right board {
        boardColumns = updateColumn s2 (column2 ++ movedCards)
                     $ updateColumn s1 (hiddenCards ++ drop n visibleCards)
                     $ boardColumns board
      }
  where
    columns = boardColumns board
    column1 = columns !! s1
    column2 = columns !! s2

    -- Separate visible and hidden cards in column1
    (hiddenCards, visibleCards) = span (not . snd) column1

    -- Cards to be moved
    movedCards = take n visibleCards

    -- Helper function to check if a card is a King
    isKing card = cardValue card == King



{- EXERCISE 9: Move Stack -}
moveStack :: Int -> Int -> Board -> Either Error Board
moveStack s1 s2 board = 
    if null visibleCards 
        then Left InvalidCount -- No visible cards to move
        else move (length visibleCards) s1 s2 board
  where
    columns = boardColumns board
    column1 = columns !! s1

    -- Extract visible cards (cards with True in the Bool field)
    visibleCards = filter snd column1


{- EXERCISE 10: Move from Discard -}
moveFromDiscard :: Int -> Board -> Either Error Board
moveFromDiscard colIdx board
    | null discard = Left DiscardEmpty
    | null targetCol && not (isKing cardToMove) = Left ColumnKing
    | not (null targetCol) && not (canStack cardToMove (fst (last targetCol))) = Left WrongOrder
    | otherwise = Right board {
        boardDiscard = tail discard, -- Remove the top card from discard pile
        boardColumns = updateColumn colIdx (targetCol ++ [(cardToMove, True)]) (boardColumns board)
      }
  where
    discard = boardDiscard board
    targetCol = boardColumns board !! colIdx
    cardToMove = head discard

    -- Helper to check if a card is a King
    isKing card = cardValue card == King


{- EXERCISE 11: Move to Pillar -} 
moveToPillar :: CardSource -> Board -> Either Error Board
moveToPillar source board = case source of
    FromDiscard ->
        if null discard
        then Left DiscardEmpty
        else moveCardToPillar (head discard) (board { boardDiscard = tail discard })

    FromStack colIdx ->
        let column = columns !! colIdx
        in if null column
           then Left ColumnEmpty
           else let (card, _) = last column
                in moveCardToPillar card (board { boardColumns = updateColumn colIdx (init column) columns })
  where
    discard = boardDiscard board
    columns = boardColumns board

    -- Move the card to the appropriate pillar and validate the move
    moveCardToPillar :: Card -> Board -> Either Error Board
    moveCardToPillar card b =
        case cardSuit card of
            Spades   -> tryAddToPillar card spadesTop (\p -> b { boardPillars = p { spades = Just (cardValue card) } })
            Clubs    -> tryAddToPillar card clubsTop (\p -> b { boardPillars = p { clubs = Just (cardValue card) } })
            Hearts   -> tryAddToPillar card heartsTop (\p -> b { boardPillars = p { hearts = Just (cardValue card) } })
            Diamonds -> tryAddToPillar card diamondsTop (\p -> b { boardPillars = p { diamonds = Just (cardValue card) } })
      where
        pillars = boardPillars b
        spadesTop = spades pillars
        clubsTop = clubs pillars
        heartsTop = hearts pillars
        diamondsTop = diamonds pillars

    -- Validate and apply the move to the pillar
    tryAddToPillar :: Card -> Maybe Value -> (Pillars -> Board) -> Either Error Board
    tryAddToPillar card currentValue updatePillar
        | isValidPillarMove card currentValue = Right (updatePillar (boardPillars board))
        | otherwise = Left WrongPillarOrder

    -- Check if the move to the pillar is valid
    isValidPillarMove :: Card -> Maybe Value -> Bool
    isValidPillarMove card Nothing = cardValue card == Ace
    isValidPillarMove card (Just topValue) = fromEnum (cardValue card) == fromEnum topValue + 1

            
{- EXERCISE 12: Move from Pillar -}
moveFromPillar :: Suit -> Int -> Board -> Either Error Board
moveFromPillar suit idx board =
    case getPillarTop suit (boardPillars board) of
        Nothing -> Left PillarEmpty  -- The pillar is empty
        Just cardToMove ->
            let columns = boardColumns board
                targetCol = columns !! idx
            in if null targetCol && not (isKing cardToMove)
               then Left ColumnKing  -- Cannot place a non-King card on an empty column
               else if not (null targetCol) && not (canStack cardToMove (fst (last targetCol)))
                    then Left WrongOrder  -- Cannot stack due to Solitaire rules
                    else Right board {
                        boardPillars = removeFromPillar suit (boardPillars board),
                        boardColumns = updateColumn idx (targetCol ++ [(cardToMove, True)]) columns
                    }
  where
    -- Helper function to get the top card of the specified pillar
    getPillarTop :: Suit -> Pillars -> Maybe Card
    getPillarTop s ps = case getPillar ps s of
        Nothing -> Nothing
        Just value -> Just (MkCard s value)

    -- Helper function to remove the top card from the specified pillar
    removeFromPillar :: Suit -> Pillars -> Pillars
    removeFromPillar s ps = decPillar ps s

    -- Helper function to check if a card is a King
    isKing :: Card -> Bool
    isKing card = cardValue card == King


{- EXERCISE 13: Solve -} 
solve :: Board -> Board
solve board =
    let updatedBoard = tryMoveAllToPillars board
    in if updatedBoard == board
       then board  -- No changes, stop recursion
       else solve updatedBoard  -- Keep solving until no more moves are possible

-- Attempt to move all possible cards to the pillars
tryMoveAllToPillars :: Board -> Board
tryMoveAllToPillars board = foldl tryMoveFromColumn (tryMoveFromDiscard board) [0..length (boardColumns board) - 1]

-- Attempt to move a card from the discard pile to the pillars
tryMoveFromDiscard :: Board -> Board
tryMoveFromDiscard board =
    case moveToPillar FromDiscard board of
        Right updatedBoard -> updatedBoard  -- Successful move
        Left _ -> board  -- No move possible, return unchanged board

-- Attempt to move the top card of a specific column to the pillars
tryMoveFromColumn :: Board -> Int -> Board
tryMoveFromColumn board colIdx =
    case moveToPillar (FromStack colIdx) board of
        Right updatedBoard -> updatedBoard  -- Successful move
        Left _ -> board  -- No move possible, return unchanged board





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

testEmptyPillars = MkPillars Nothing Nothing Nothing Nothing



-- Create board instance with facedown card to test flipCards method
testFlipCards = MkBoard deckOf52 