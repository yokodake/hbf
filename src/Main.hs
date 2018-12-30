module Main where

import Data.Char (ord)
import Data.Word (Word8 (..), Word16 (..))

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V (new, write, read)
import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = putStrLn "hello"

data Ins = Succ -- ^ '+' increment byte
         | Pred -- ^ '-' decrement byte
         | Inc -- ^ '>' increment pointer
         | Dec -- ^ '<' decrement pointer
         | Put -- ^ '.' putByte
         | Get -- ^ ',' getByte
         | Jmp Int -- ^ '[' skip block if byte is zero
         | End Int -- ^ ']' jnz on [
         deriving Show

data InterpState = InterpState { memory :: Vector Word8
                               , code :: Vector Ins
                               , ptr :: Int -- use of Int for indexing without conversion
                               , ip :: Int
                               } deriving (Show)
maxMem :: Int
maxMem = 30000
initMem :: Word8
initMem = 0

-- | use a stack to assign a label associated to our jumps (] and [)
-- this makes it easier to deal with nestings, I think
data Labels = Labels { stack :: [Int]
                     , next :: Int
                     }

mkLabel :: Labels -> (Int, Labels)
mkLabel (Labels ns n) = (n, Labels (n:ns) (succ n))

rmLabel :: Labels -> Maybe (Int, Labels)
rmLabel labels = case stack labels of
                   [] -> Nothing
                   x:xs -> Just (x, labels {stack=xs})

parse :: Text -> Vector Ins
parse txt = V.fromList $ go 0 (Labels [] 0)
    where
      go i lbl | i >= T.length txt = []
      go i lbl | otherwise =
                 let i' = succ i
                 in case T.index txt i of
                      '+' -> Succ : go i' lbl
                      '-' -> Pred : go i' lbl
                      '>' -> Inc : go i' lbl
                      '<' -> Dec : go i' lbl
                      '.' -> Put : go i' lbl
                      ',' -> Dec : go i' lbl
                      '[' -> case mkLabel lbl of (l, lbl') -> Jmp l : go i' lbl'
                      ']' -> case rmLabel lbl of
                               -- FIXME better error handling
                               Nothing -> errorWithoutStackTrace $ "unexpected ']' at " ++ show i
                               Just (l, lbl') -> End l : go i' lbl'
                      _ -> go i' lbl -- non valid characters are considered comments
