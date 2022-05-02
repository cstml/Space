module Space.Interface.REPL where

import Brick
import Brick.AttrMap qualified as A
import Brick.Focus qualified as F
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Widgets.Border qualified as B
import Brick.Widgets.Center qualified as C
import Brick.Widgets.Edit qualified as E
import Control.Lens
import Graphics.Vty qualified as V
import Space

data Input = Input
  deriving (Ord, Eq, Show)

data St = St
  { _focusRing :: F.FocusRing Input
  , _editor :: E.Editor String Input
  }

makeLenses ''St

nEd :: String -> E.Editor String Input
nEd = E.editor Input (Just 100)

ed :: E.Editor String Input
ed = nEd "Hello"

appEvent :: St -> T.BrickEvent Input e -> T.EventM Input (T.Next St)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    _ -> M.continue =<< T.handleEventLensed st editor E.handleEditorEvent ev
appEvent st _ = M.continue st

appCursor :: St -> [T.CursorLocation Input] -> Maybe (T.CursorLocation Input)
appCursor = F.focusRingCursor (^. focusRing)

drawUI :: St -> [T.Widget Input]
drawUI st = [ui]
 where
  e1 = F.withFocusRing (st ^. focusRing) (E.renderEditor (str . unlines)) (st ^. editor)

  ui =
    C.center $
      (str "Input 1 (unlimited): " <+> hLimit 30 (vLimit 5 e1))
        <=> str " "

--          (str "Input 2 (limited to 2 lines): " <+> (hLimit 30 e2)) <=>
--          str " " <=>
--          str "Press Tab to switch between editors, Esc to quit."

theMap :: A.AttrMap
theMap =
  A.attrMap
    V.defAttr
    [ (E.editAttr, V.white `on` V.blue)
    , (E.editFocusedAttr, V.black `on` V.yellow)
    ]

initialState =
  St
    (F.focusRing [Input])
    (E.editor Input (Just 800) "")

repl :: M.App St e Input
repl =
  M.App
    { M.appDraw = drawUI
    , M.appChooseCursor = appCursor
    , M.appHandleEvent = appEvent
    , M.appStartEvent = return
    , M.appAttrMap = const theMap
    }
