{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad                  ( forever
                                                , void
                                                )
import           Control.Concurrent             ( threadDelay
                                                , forkIO
                                                )

import           Lens.Micro
import           Lens.Micro.TH

import           Brick
import qualified Brick.Forms                   as F
import qualified Brick.Focus                   as Fc
import qualified Brick.Widgets.Edit            as E
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C
import qualified Brick.Widgets.ProgressBar     as P
import           Brick.BChan                    ( newBChan
                                                , writeBChan
                                                )

import qualified Graphics.Vty                  as V

import           Text.Read                      ( readMaybe )

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
divP p x y = (fromIntegral (p - x) / fromIntegral (y - x))

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

-- Module UI

mkForm :: UserForm -> F.Form UserForm Tick Name
mkForm =
  let label s w =
          padBottom (Pad 1) $ (vLimit 1 $ hLimit 18 $ str s <+> fill ' ') <+> w
  in
    F.newForm
      [ label "Block size: " F.@@= F.editShowableField blockSize BlockSize
      , label "Number of Blocks: "
        F.@@= F.editShowableField numberBlocks NumberBlocks
      , label "Resting Time: " F.@@= F.editShowableField restingTime RestingTime
      ]


theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ (E.editAttr              , V.white `on` V.black)
  , (E.editFocusedAttr       , V.black `on` V.yellow)
  , (F.invalidFormInputAttr  , V.white `on` V.red)
  , (F.focusedFormInputAttr  , V.black `on` V.yellow)
  , (P.progressCompleteAttr  , V.yellow `on` V.black)
  , (P.progressIncompleteAttr, V.black `on` V.yellow)
  ]

draw :: State -> [Widget Name]
draw (FormSt st) = [C.vCenter $ C.hCenter form <=> C.hCenter help]
 where
  form = B.border $ padTop (Pad 1) $ hLimit 50 $ F.renderForm st
  help = padTop (Pad 1) $ B.borderWithLabel (str " Help ") body
  body =
    str
      $  "- All values are integers! The values are the minutes!\n"
      <> "- Esc quit, mouse interacts with fields"
draw (ProgressSt st) = [C.vCenter $ C.hCenter $ prog]
 where
  prog =
    B.borderWithLabel (str " Progress ") $ padTop (Pad 2) $ padLeftRight 4 body
  sh   = toSP st
  body = vBox $ map
    (padBottom (Pad 2) . hLimit 50)
    [ P.progressBar (Just "Total progress") (sh ^. totalProgress)
    , P.progressBar (Just (show $ sh ^. pLabel)) (sh ^. pProgress)
    , str "Press Esc to quit"
    ]


handleEvent :: State -> BrickEvent Name Tick -> EventM Name (Next State)
handleEvent s          (VtyEvent (V.EvKey V.KEsc [])) = halt s
handleEvent (FormSt s) (VtyEvent ev                 ) = case ev of
  V.EvResize{}        -> continue (FormSt s)
  V.EvKey V.KEnter [] -> if F.allFieldsValid s
    then
      let contents = F.formState s
      in  continue $ ProgressSt $ toProgress contents
    else continue (FormSt s)
  _ -> do
    form' <- F.handleFormEvent (VtyEvent ev) s
    let s' = FormSt form'
    continue s'
handleEvent (FormSt s) ev = do
  form' <- F.handleFormEvent ev s
  let s' = FormSt form'
  continue s'
handleEvent (ProgressSt p) (AppEvent Tick) =
  let p' = over progress (+ 1) p
  in  if p' ^. progress >= p' ^. totalTime
        then halt $ ProgressSt p'
        else continue $ ProgressSt $ over progress (+ 1) p

handleEvent s _ = continue s

app :: App State Tick Name
app = App
  { appDraw         = draw
  , appHandleEvent  = handleEvent
  , appChooseCursor = \st -> case st of
                        FormSt     s -> Fc.focusRingCursor F.formFocus $ s
                        ProgressSt s -> neverShowCursor (ProgressSt s)
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

initialUserInfo :: UserForm
initialUserInfo =
  UserForm { _blockSize = 0, _numberBlocks = 0, _restingTime = 0 }

initialState :: State
initialState = FormSt $ mkForm initialUserInfo

-- Main.hs

main :: IO ()
main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay $ 1 * 1000 * 1000
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  initalVty <- buildVty
  f         <- customMain initalVty buildVty (Just chan) app $ initialState
  return ()
