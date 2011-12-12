{- skyline -}

type Row = [Integer]
type Skyline = [Row]

{- 1a -}

{- Checks if the given skyline is an (n+2)x(n+2) matrix,
 - each row and column contains all values [ 10 .. 10 * n ],
 - and all visibility infos are correct. Returns true if all conditions
 - are satisfied, else false. -}

-- isValid :: Skyline -> Bool

{- 1b -}

{- Given a skyline *without* visibility information (an n x n matrix),
 - calculates the visibility and returns the skyline *with* visibility
 - infos. If the given skyline is not valid, it is returned without change. -}

-- compVisibility :: Skyline -> Skyline

{- 1c -}

{- Given a (partially) empty skyline, returns a complete valid skyline if possible
 - or Nothing otherwise. Empty fields are set to 0. All existing skyline infos
 - need to be valid. Maximal size of passed skylines is 5 x 5. -}

-- buildSkyscrapers :: Skyline -> Maybe Skyline

{- sudoku -}

type Row = [Integer]
type Sudoku = [Row]

data Variant = Basic | Cross | Color deriving (Eq,Show)

{- 2a -}

{- Given a sudoku and a variant argument, returns true if the sudoku is a valid
 - sudoku of the specified variant and false otherwise. Empty fields are any
 - fields not containing a number from 1-9.
 -
 - Basic: 1-9 in each subsquare, row and column
 - Cross: Basic + in both diagonals
 - Color: Basic + in each position of subsquares -}

-- isValid :: Sudoku -> Variant -> Bool

{- 2b -}

{- Given a sudoku and a variant argument, returns a solved version of possible
 - or Nothing otherwise. -}

-- solve :: Sudoku -> Variant -> Maybe Sudoku
