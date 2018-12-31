module Core where

import           Debug.Trace

import           Data.Char (ord, chr)
import           Data.Maybe (fromMaybe)
import           Data.Word (Word8 (..), Word16 (..))

import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.IO as T (getLine)
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as V (new, write, read)

data Ins = Succ -- ^ '+' increment byte
         | Pred -- ^ '-' decrement byte
         | Inc -- ^ '>' increment pointer
         | Dec -- ^ '<' decrement pointer
         | Put -- ^ '.' putByte
         | Get -- ^ ',' getByte
         | Jmp Int -- ^ '[' skip block if byte is zero
         | End Int -- ^ ']' jnz on [
         deriving (Show, Eq)

data IState m = IState { memory :: m
                       , code :: Vector Ins
                       , stack :: [Int] -- ^ jmp stack for faster ]
                       , ptr :: Int -- ^ use of Int for indexing without conversion
                       , ip :: Int -- ^ intruction pointer
                       , out :: [Word8] -- ^ what has been outputed so far
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
initState :: Vector Ins -> IState VMemory
initState code' =  IState { memory = mcreate 64
                          , code = code'
                          , stack = []
                          , ptr = 0
                          , ip = 0
                          , out = []
                          }

run_ :: BFMemory a => IState a -> IO ()
run_ s0 = run step s0 >> putStrLn ""

run :: BFMemory a => (IState a -> IO (IState a)) -> IState a -> IO (IState a)
run = untilM finish
  where
    finish s = ip s == V.length (code s)

step :: BFMemory a => IState a -> IO (IState a)
step s0 = incip <$> step' s0 (code s0 ! ip s0)
  where
    -- updates the cell at current pointer with function
    modifyMem f s = s{memory = mmodify f (ptr s) (memory s)}

    incip s = s{ip=succ $ ip s}

    -- we don't use succ and pred because they throw an exception on overflow
    -- where as the (+) and (-) do a wraparound, which is what we want
    step' s Succ = return $ modifyMem (+1) s
    step' s Pred = return $ modifyMem (flip (-) 1) s
    step' s Inc = return $ incPtr s
    step' s Dec = return $ decPtr s
    step' s Put = putWord s (memory s %! ptr s)
    step' s Get = (\w -> modifyMem (const w) s) <$> getWord
    step' s (Jmp i) = return $ if value s == 0
                               then jmp s
                               else push s
    step' s (End _) = return $ if value s /= 0
                               then s{ip=unsafePeek s}
                               else pop s

-- | read value in currently pointing cell
value :: BFMemory a => IState a -> Word8
value s = memory s %! ptr s

-- | increase the pointer, grow memory if needed (until @maxMem@)
incPtr :: BFMemory a => IState a -> IState a
incPtr s@IState{memory=m, ptr=p}
  | p < msize m || p >= maxMem = s{ptr=succ p}
  | otherwise = s{memory=newMem, ptr=succ p}
  where
    newMem = mrealloc m $ max (msize m * 2) maxMem

-- | decrease the pointer
-- NOTE should we consider freeing memory?
decPtr :: BFMemory a => IState a -> IState a
decPtr s = s{ptr=pred (ptr s)}

-- TODO ASCII/hex
-- TODO command line arg to for choice between decimal,hex,etc.
-- FIXME faster append to output (probably change output for something else)
putWord :: BFMemory a => IState a -> Word8 -> IO (IState a)
putWord s c = do putChar . chr . fromIntegral $ c
                 return s{out=out s ++ [c]}

getWord :: IO Word8
getWord = fromIntegral . ord <$> getChar

-- | if we're on a Jmp instruction we set the ip on the index of
-- corresponding End instruction. Else we do nothing
jmp :: BFMemory a => IState a -> IState a
jmp s@IState{ip=i, code=c} = s{ip=jmp' $ c ! i}
  where
    slice = V.drop i c
    jmp' (Jmp idx) = fromMaybe failure $ V.findIndex (==End idx) slice
    jmp' _ = i -- if we're not on a jz instruction, we don't jump...

    failure = errorWithoutStackTrace "exception: the parser is broken"

-- | get the the index on the top of the jumps stack, crash if empty
unsafePeek :: BFMemory a => IState a -> Int
unsafePeek = head . stack

push, pop :: BFMemory a => IState a -> IState a
-- | push the ip on the jumps stack
push s@IState{ip=i, stack=is} = s{stack=i:is}
-- | remove the ip from the jumps stack
pop s@IState{stack=[]} = s
pop s@IState{stack=i:is} = s{stack=is}


-- | @until@ but with a monadic function
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM pred f s0 = if pred s0 then
                     return s0
                   else
                     f s0 >>= untilM pred f


-- | Separate Memory class. I've been thinking about different ways of
-- implementing this memory array, and I'd like to benchmark them all
-- so might as well lay the groundwork for easy swapping?
-- TODO : bytestring impl
--      , group of memblocks (make new block instead of reallocing)
class BFMemory a where
  msize :: a -> Int         -- ^ get capacity
  (%!) :: a -> Int -> Word8 -- ^ get indexing
  mmodify :: (Word8 -> Word8) -> Int -> a -> a -- ^ element updating
  mrealloc :: a -> Int -> a -- ^ copy elements in new memory of given size
  mcreate :: Int -> a       -- ^ create new (zeroed) memory of given size
  empty :: a                -- ^ create empty memory
  slice :: Int -> Int -> a -> [Word8] -- ^ FIXME linked lists suck ass

  mcreate = mrealloc empty
  empty = mcreate 0
  {-# MINIMAL msize, (%!), mmodify, mrealloc, slice, (mcreate | empty) #-}

newtype VMemory = VMemory { getVec :: Vector Word8
                          } deriving (Show, Eq)

instance BFMemory VMemory where
  (%!) = (!) . getVec

  mmodify f i = VMemory . V.modify (\v -> V.read v i >>= V.write v i . f) . getVec

  mrealloc (VMemory old) size = VMemory $ V.update_ new idx old
    where
      new = V.replicate size initMem
      idx = V.enumFromN 0 $ V.length old

  mcreate = VMemory . flip V.replicate 0

  msize = V.length . getVec

  slice s e = V.toList . V.slice s e . getVec
