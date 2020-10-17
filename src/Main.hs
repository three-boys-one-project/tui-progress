{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad                  ( forever )
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

import           Types

-- Module UI

mkForm :: UserForm -> F.Form UserForm Tick Name
mkForm =
  let label s w =
          padBottom (Pad 1) $ vLimit 1 (hLimit 18 $ str s <+> fill ' ') <+> w
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
      <> "- Enter starts progress bar"
draw (ProgressSt st) = [C.vCenter $ C.hCenter $ prog]
 where
  prog =
    B.borderWithLabel (str " Progress ") $ padTop (Pad 2) $ padLeftRight 4 body
  sh   = toSP st
  body = vBox $ map
    (padBottom (Pad 2) . hLimit 50)
    [ P.progressBar (Just "Total progress") (sh ^. totalProgress)
    , P.progressBar (Just (show $ sh ^. pLabel)) (sh ^. pProgress)
    , C.hCenter $ str "Press Esc to quit"
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
