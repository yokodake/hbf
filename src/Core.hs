module Core where

import Debug.Trace

import Data.Char (ord)
import Data.Maybe (fromMaybe)
import Data.Word (Word8 (..), Word16 (..))

import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V (new, write, read)
import Data.Text (Text)
import Data.Text.IO as T (getLine)
import qualified Data.Text as T

data Ins = Succ -- ^ '+' increment byte
         | Pred -- ^ '-' decrement byte
         | Inc -- ^ '>' increment pointer
         | Dec -- ^ '<' decrement pointer
         | Put -- ^ '.' putByte
         | Get -- ^ ',' getByte
         | Jmp Int -- ^ '[' skip block if byte is zero
         | End Int -- ^ ']' jnz on [
         deriving (Show, Eq)

data InterpState = InterpState { memory :: Vector Word8
                               , code :: Vector Ins
                               , stack :: [Int] -- jmp stack for faster ]
                               , ptr :: Int -- use of Int for indexing without conversion
                               , ip :: Int
                               } deriving (Show)

maxMem :: Int
maxMem = 30000

initMem :: Word8
initMem = 0

-- | use a stack to assign a label associated to our jumps (] and [)
-- this makes it easier to deal with nestings, I think
data Labels = Labels { curr :: [Int]
                     , next :: Int
                     }

mkLabel :: Labels -> (Int, Labels)
mkLabel (Labels ns n) = (n, Labels (n:ns) (succ n))

rmLabel :: Labels -> Maybe (Int, Labels)
rmLabel labels = case curr labels of
                   [] -> Nothing
                   x:xs -> Just (x, labels {curr=xs})

-- | the parser of instructions
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

-- | make our initial interpState
initState :: Vector Ins -> InterpState
initState code' =  InterpState { memory = V.replicate 64 0
                               , code = code'
                               , stack = []
                               , ptr = 0
                               , ip = 0 }

run_ s0 = run step s0 >> putStrLn ""

run f = untilM finish f
  where
    finish s = ip s == V.length (code s)

step :: InterpState -> IO InterpState
step s0 = incip <$> step' s0 (code s0 ! ip s0)
             where
              incip s = s{ip=succ $ ip s}
              -- we don't use succ and pred because they throw an exception on overflow
              -- where as the (+) and (-) do a wraparound, which is what we want
              step' s Succ = return $ mmodify (+1) s
              step' s Pred = return $ mmodify (flip (-) 1) s
              step' s Inc = return $ incPtr s
              step' s Dec = return $ decPtr s
              step' s Put = s <$ putWord (memory s ! ptr s)
              step' s Get = (\c -> mmodify (const c) s) <$> getWord
              step' s (Jmp i) = return $ if value s == 0
                                          then jmp s
                                          else push s
              step' s (End _) = return $ if value s /= 0
                                          then s{ip=unsafePeek s}
                                          else pop s

-- | read value in currently pointing cell
value :: InterpState -> Word8
value s = memory s ! ptr s

-- | helper function to modify the cell at the current pointer
-- FIXME handle out of bounds reading
mmodify :: (Word8 -> Word8) -> InterpState -> InterpState
mmodify f s = s{memory = modify' f (ptr s) (memory s)}
  where
    modify' f i = V.modify (\v -> V.read v i >>= V.write v i . f)

-- | increase the pointer, grow memory if needed (until @maxMem@)
incPtr :: InterpState -> InterpState
incPtr s@InterpState{memory=m, ptr=p}
  | p < V.length m || p >= maxMem = s{ptr=succ p}
  | otherwise = s{memory=realloc m, ptr=succ p}
  where
    realloc old = V.update_ new idx old
      where
        size = min maxMem $ V.length old * 2
        new = V.replicate size initMem
        idx = V.enumFromN 0 $ V.length old

-- | decrease the pointer
-- NOTE should we consider freeing memory?
decPtr :: InterpState -> InterpState
decPtr s = s{ptr=pred (ptr s)}

-- TODO ASCII/hex
-- TODO command line arg to for choice between decimal,hex,etc.
putWord :: Word8 -> IO ()
putWord = putStr . show

getWord :: IO Word8
getWord = fromIntegral . ord <$> getChar

-- | if we're on a Jmp instruction we set the ip on the index of
-- corresponding End instruction. Else we do nothing
jmp :: InterpState -> InterpState
jmp s@InterpState{ip=i, code=c} = s{ip=jmp' $ c ! i}
  where
    slice = V.drop i c
    jmp' (Jmp idx) = fromMaybe failure $ V.findIndex (==End idx) slice
    jmp' _ = i -- if we're not on a jz instruction, we don't jump...

    failure = errorWithoutStackTrace "exception: the parser is broken"

-- | get the the index on the top of the jumps stack, crash if empty
unsafePeek :: InterpState -> Int
unsafePeek = head . stack

push, pop :: InterpState -> InterpState
-- | push the ip on the jumps stack
push s@InterpState{ip=i, stack=is} = s{stack=i:is}
-- | remove the ip from the jumps stack
pop s@InterpState{stack=[]} = s
pop s@InterpState{stack=i:is} = s{stack=is}


-- | @until@ but with a monadic function
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM pred f s0 = if pred s0 then
                     return s0
                   else
                     f s0 >>= untilM pred f
