{-# LANGUAGE TemplateHaskell #-}
-- | All types and morphisms used by the application

module Types
  ( Name(..)
  , UserForm(..)
  , blockSize
  , numberBlocks
  , restingTime
  , Label(..)
  , Progress(..)
  , totalTime
  , timerList
  , progress
  , Tick(..)
  , toProgress
  , ShowableProgress(..)
  , totalProgress
  , pLabel
  , pProgress
  , toSP
  , State(..)
  )
where

import           Lens.Micro.TH

import qualified Brick.Forms                   as F


-- Module Data and Module Utils
data Name = BlockSize
          | NumberBlocks
          | RestingTime
          deriving (Eq, Ord, Show, Enum)

data UserForm = UserForm { _blockSize    :: Int
                         , _numberBlocks :: Int
                         , _restingTime  :: Int
                         } deriving (Show, Read, Eq, Ord)

makeLenses ''UserForm

data Label = Working | Resting deriving (Show, Read, Eq, Ord, Enum)

data Progress = Progress { _totalTime :: Int
                         , _timerList :: Int -> (Label, Float)
                         , _progress  :: Int
                         }

makeLenses ''Progress

data Tick = Tick deriving (Show, Read, Eq)

genL :: (a -> a) -> (a -> a) -> a -> [a]
genL f g a = let r = f a in a : genL g f r

timeConst :: Int
timeConst = 60

genTimerList :: Int -> Int -> [(Int, Int, Label)]
genTimerList b r = zipWith f ls $ cycle [Working ..]
 where
  ls' = map (* timeConst) $ genL (+ b) (+ r) 0
  ls  = zip ls' $ tail ls'
  f (a, b) l = (a, b, l)

divP :: Int -> Int -> Int -> Float
divP p x y = fromIntegral (p - x) / fromIntegral (y - x)

timerListGen :: Int -> Int -> Int -> (Label, Float)
timerListGen b r p = (l, res)
 where
  ls = genTimerList b r
  f (x', y', _) = x' <= p && p < y'
  (x, y, l) = head $ dropWhile (not . f) ls
  res       = divP p x y

toProgress :: UserForm -> Progress
toProgress (UserForm b n r) = Progress
  { _totalTime = timeConst * (b * n + r * (n - 1))
  , _timerList = timerListGen b r
  , _progress  = 0
  }

data ShowableProgress = SP { _totalProgress :: Float
                           , _pLabel :: Label
                           , _pProgress :: Float
                           } deriving (Show, Read, Eq, Ord)

makeLenses ''ShowableProgress

toSP :: Progress -> ShowableProgress
toSP (Progress t tr p) = SP { _totalProgress = divP p 0 t
                            , _pLabel        = l
                            , _pProgress     = res
                            }
  where (l, res) = tr p


data State = FormSt (F.Form UserForm Tick Name) | ProgressSt Progress

makeLenses ''State
