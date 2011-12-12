{- skyline -}

type Row = [Integer]
type Skyline = [Row]

{- 1a -}

-- isValid :: Skyline -> Bool

{- 1b -}

-- compVisibility :: Skyline -> Skyline

{- 1c -}

-- buildSkyscrapers :: Skyline -> Maybe Skyline

{- sudoku -}

type Row = [Integer]
type Sudoku = [Row]

data Variant = Basic | Cross | Color deriving (Eq,Show)

{- 2a -}

-- isValid :: Sudoku -> Variant -> Bool

{- 2b -}

-- solve :: Sudoku -> Variant -> Maybe Sudoku
