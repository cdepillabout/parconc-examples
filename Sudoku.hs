--
-- Sudoku solver using constraint propagation.  The algorithm is by
-- Peter Norvig http://norvig.com/sudoku.html; the Haskell
-- implementation is by Manu and Daniel Fischer, and can be found on
-- the Haskell Wiki http://www.haskell.org/haskellwiki/Sudoku
--
-- The Haskell wiki license applies to this code:
--
-- Permission is hereby granted, free of charge, to any person obtaining
-- this work (the "Work"), to deal in the Work without restriction,
-- including without limitation the rights to use, copy, modify, merge,
-- publish, distribute, sublicense, and/or sell copies of the Work, and
-- to permit persons to whom the Work is furnished to do so.
-- 
-- THE WORK IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
-- NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
-- LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
-- OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
-- WITH THE WORK OR THE USE OR OTHER DEALINGS IN THE WORK.

module Sudoku where

import Data.List hiding (lookup)
import Data.Array
import Control.Monad

-- Types
type Digit  = Char
type Square = (Char,Char)
type Unit   = [Square]

-- We represent our grid as an array
type Grid = Array Square [Digit]


-- Setting Up the Problem
rows :: String
rows = "ABCDEFGHI"

cols :: String
cols = "123456789"

digits :: [Digit]
digits = "123456789"

box :: ((Char, Char), (Char, Char))
box = (('A','1'),('I','9'))

cross :: String -> String -> [Square]
cross rows' cols' =
  [ (r,c)
  | r <- rows'
  , c <- cols'
  ]

-- |
-- >>> squares
-- [('A', '1'), ('A', '2'), ...]
squares :: [Square]
squares = cross rows cols

-- | This is just 'units' will all duplicates removed.
peers :: Array Square [Square]
peers =
  array
    box
    [ ( s
      , nub $ concat (units!s)
      )
    | s <- squares
    ]

-- |
-- >>> unitlist
-- [ [('A','1'),('B','1'),('C','1'),('D','1'),...]
-- , [('A','2'),('B','2'),('C','2'),('D','2'),...]
-- , ...
-- , [('A','1'),('A','2'),('A','3'),('A','4'),...]
-- , [('B','1'),('B','2'),('B','3'),('B','4'),...]
-- , ...
-- , ('A','1'),('A','2'),('A','3'),('B','1'),('B','2'),('B','3'),('C','1'),...]
-- , ('A','4'),('A','5'),('A','6'),('B','4'),('B','5'),('B','6'),('C','4'),...]
-- ]
unitlist :: [Unit]
unitlist =
  let allCols =
        [ cross rows [c]
        | c <- cols
        ]
      allRows =
        [ cross [r] cols
        | r <- rows
        ]
      all3Boxes =
        [ cross rs cs
        | rs <- ["ABC","DEF","GHI"]
        , cs <- ["123","456","789"]
        ]
  in allCols ++ allRows ++ all3Boxes

-- | this could still be done more efficiently, but what the heck...
--
-- >>> units
-- array (( 'A' , '1') , ( 'I' , '9'))
-- [ ( ( 'A' , '1')
--   , [ [('B','1'),('C','1'),('D','1'),('E','1'),('F','1'),('G','1'),('H','1'),('I','1')]
--     , [('A','2'),('A','3'),('A','4'),('A','5'),('A','6'),('A','7'),('A','8'),('A','9')]
--     , [('A','2'),('A','3'),('B','1'),('B','2'),('B','3'),('C','1'),('C','2'),('C','3')]
--     ]
--   )
-- , ( ( 'A' , '2')
--   , ...
--   )
-- , ...
-- ]
units :: Array Square [Unit]
units =
  array
    box
    [ ( s
      , [ filter (/= s) u
        | u <- unitlist
        , s `elem` u
        ]
      )
    | s <- squares
    ]


-- |
-- >>> allPossibilities
-- array (( 'A' , '1') , ( 'I' , '9'))
-- [ (('A','1'), "123456789")
-- , (('A','2'), "123456789")
-- , (('A','3'), "123456789")
-- , ...
-- , (('I','9'), "123456789")
-- ]
allPossibilities :: Grid
allPossibilities = array box [ (s,digits) | s <- squares ]

-- Parsing a grid into an Array
parsegrid :: String -> Maybe Grid
parsegrid g = do
  _ <- regularGrid g
  foldM assign allPossibilities (zip squares g)

-- | Check if all the characters in a 'String' are in @0.-123456789@.
regularGrid :: String -> Maybe String
regularGrid g =
  if all (`elem` "0.-123456789") g
    then Just g
    else Nothing

-- Propagating Constraints
assign :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s,d) =
  if d `elem` digits
    -- check that we are assigning a digit and not a '.'
    then
      let ds = g ! s
          toDump = delete d ds
      in foldM eliminate g (zip (repeat s) toDump)
    else return g
    -- else error
    --      ("Trying to assign something not a digit: " ++ [d] ++ "\n" ++
    --       "Square: " ++ show s ++ "\n" ++
    --       "Grid:\n" ++ gridToString g)

eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s,d) =
  let cell = g ! s in
  if d `notElem` cell
    -- already eliminated
    then return g
    -- else d is deleted from s' values
    else do
      let newCell = delete d cell
          newV = g // [(s,newCell)]
      newV2 <-
        case newCell of
          -- contradiction : Nothing terminates the computation
          []   -> Nothing
          -- if there is only one value left in s, remove it from peers
          [d'] -> do
            let peersOfS = peers ! s
            foldM eliminate newV (zip peersOfS (repeat d'))
          -- else : return the new grid
          _    -> return newV
      -- Now check the places where d appears in the peers of s
      foldM (locate d) newV2 (units ! s)

locate :: Digit -> Grid -> Unit -> Maybe Grid
locate d g u =
  case filter ((d `elem`) . (g !)) u of
    []  -> Nothing
    [s] -> assign g (s,d)
    _   -> return g

-- Search
search :: Grid -> Maybe Grid
search g =
  let ls = gridWithLengthNot1 g in
  case ls of
    [] -> return g
    _ -> do
      let (_,s,ds) = minimum ls
      msum
        [ assign g (s,d) >>= search
        | d <- ds
        ]

-- | Given a grid, return list with the first 'Int' the number of possibilities
-- remaining for each square.
gridWithLengthNot1 :: Grid -> [(Int, Square, [Digit])]
gridWithLengthNot1 g =
  [ (l,s,xs)
  | (s,xs) <- assocs g
  , let l = length xs
  , l /= 1
  ]

solve :: String -> Maybe Grid
solve str = do
    grd <- parsegrid str
    search grd

-- Display solved grid
printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToString

gridToString :: Grid -> String
gridToString g =
  let l0 = elems g
      -- [("1537"),("4"),...]
      l1 = (map (\s -> " " ++ s ++ " ")) l0
      -- ["1 "," 2 ",...]
      l2 = (map concat . sublist 3) l1
      -- ["1  2  3 "," 4  5  6 ", ...]
      l3 = (sublist 3) l2
      -- [["1  2  3 "," 4  5  6 "," 7  8  9 "],...]
      l4 = (map (concat . intersperse "|")) l3
      -- ["1  2  3 | 4  5  6 | 7  8  9 ",...]
      l5 = (concat . intersperse [line] . sublist 3) l4
  in unlines l5
     where sublist _ [] = []
           sublist n xs = ys : sublist n zs
             where (ys,zs) = splitAt n xs
           line = hyphens ++ "+" ++ hyphens ++ "+" ++ hyphens
           hyphens = replicate 9 '-'
