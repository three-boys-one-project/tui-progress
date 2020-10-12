{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Lens.Micro
import           Lens.Micro.TH

import           Brick
import qualified Brick.Forms                   as F
import qualified Brick.Focus                   as Fc
import qualified Brick.Widgets.Edit            as E
import qualified Brick.Widgets.Border          as B
import qualified Brick.Widgets.Center          as C

import qualified Graphics.Vty                  as V

import           Text.Read                      ( readMaybe )

data Name = BlockSize
          | NumberBlocks
          | RestingTime
          deriving (Eq, Ord, Show, Enum)

data UserForm = UserForm { _blockSize    :: Int
                         , _numberBlocks :: Int
                         , _restingTime  :: Int
                         }

makeLenses ''UserForm


mkForm :: UserForm -> F.Form UserForm e Name
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
  [ (E.editAttr            , V.white `on` V.black)
  , (E.editFocusedAttr     , V.black `on` V.yellow)
  , (F.invalidFormInputAttr, V.white `on` V.red)
  , (F.focusedFormInputAttr, V.black `on` V.yellow)
  ]

draw :: F.Form UserForm e Name -> [Widget Name]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
 where
  form = B.border $ padTop (Pad 1) $ hLimit 50 $ F.renderForm f
  help = padTop (Pad 1) $ B.borderWithLabel (str " Help ") body
  body =
    str
      $  "- All values are integers!\n"
      <> "- Esc quit, mouse interacts with fields"

handleEvent s (VtyEvent ev) = case ev of
  V.EvResize{}      -> continue s
  V.EvKey V.KEsc [] -> halt s
  _                 -> continue =<< F.handleFormEvent (VtyEvent ev) s
handleEvent s ev = continue =<< F.handleFormEvent ev s

app :: App (F.Form UserForm e Name) e Name
app = App { appDraw         = draw
          , appHandleEvent  = handleEvent
          , appChooseCursor = Fc.focusRingCursor F.formFocus
          , appStartEvent   = return
          , appAttrMap      = const theMap
          }

initialUserInfo :: UserForm
initialUserInfo =
  UserForm { _blockSize = 0, _numberBlocks = 0, _restingTime = 0 }


main :: IO ()
main = do
  let buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        return v
  initalVty <- buildVty
  f <- customMain initalVty buildVty Nothing app $ mkForm initialUserInfo
  return ()
