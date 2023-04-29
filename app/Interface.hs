{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Interface(runUI) where

import Control.Monad (void)
import qualified Data.Text as Text
import Lens.Micro ((^.))
import Lens.Micro.Mtl (use, (%=), (.=), (<~))
import Lens.Micro.TH (makeLenses)
import Simulation
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import Brick
import qualified Brick.Keybindings as K
import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Center (center)
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Table
import qualified Graphics.Vty as V
import System.Exit (exitFailure)

-- | The abstract key events for the application.
data KeyEvent
  = QuitEvent
  | RegisterEventNext
  | RegisterEventPrevious
  deriving (Ord, Eq, Show)

-- | The mapping of key events to their configuration field names.
allKeyEvents :: K.KeyEvents KeyEvent
allKeyEvents =
  K.keyEvents
    [ ("quit", QuitEvent),
      ("registern", RegisterEventNext),
      ("registerp", RegisterEventPrevious)
    ]

-- | Default key bindings for each abstract key event.
defaultBindings :: [(KeyEvent, [K.Binding])]
defaultBindings =
  [ (QuitEvent, [K.ctrl 'q', K.bind V.KEsc]),
    (RegisterEventNext, [K.bind 'j', K.bind V.KRight]),
    (RegisterEventPrevious, [K.bind 'k', K.bind V.KLeft])
  ]

data St = St
  { -- | The key config to use.
    _keyConfig :: K.KeyConfig KeyEvent,
    -- | The last key that was pressed.
    _lastKey :: Maybe (V.Key, [V.Modifier]),
    -- | Whether the last key was handled by a handler.
    _lastKeyHandled :: Bool,
    -- | Register
    _reg :: [Int],
    -- | Points at current line
    _arrows :: (Int, [String]),
    -- | Set if the application found custom keybindings in the
    -- specified file.
    _customBindingsPath :: Maybe FilePath,
    -- | The key dispatcher we'll use to dispatch input events.
    _dispatcher :: K.KeyDispatcher KeyEvent (T.EventM () St)
  }

makeLenses ''St

-- | Functions for handler. Updates register and arrows.
registerPrevious :: [State] -> [Int] -> [Int]
registerPrevious prog current
  | (current !! 0) > 0 = ((current !! 0) - 1) : (take 6 $ register (prog !! ((current !! 0) - 1)))
  | otherwise = take 7 $ current

registerNext :: [State] -> [Int] -> [Int]
registerNext prog current = ((current !! 0) + 1) : (take 6 $ register (prog !! ((current !! 0) + 1)))

arrowNext :: [State] -> (Int, [String]) -> (Int, [String])
arrowNext prog (n, _) = (n + 1, (replicate (2 * z + 1) " ") ++ ["->"] ++ (replicate (100 - 2 * z - 1) " "))
  where
    z = instructionCounter (prog !! (n + 1))

arrowPrevious :: [State] -> (Int, [String]) -> (Int, [String])
arrowPrevious prog (n, xs)
  | n == 0 = (n, xs)
  | otherwise = (n - 1, (replicate (2 * z + 1) " ") ++ ["->"] ++ (replicate (100 - 2 * z - 1) " "))
  where
    z = instructionCounter (prog !! (n - 1))

-- | Key event handlers for our application.
handlers :: [State] -> [K.KeyEventHandler KeyEvent (T.EventM () St)]
handlers steps =
  [ K.onEvent
      QuitEvent
      "Quit the program"
      M.halt,
    K.onEvent RegisterEventNext "go to next state" $ do
      reg %= registerNext steps
      arrows %= arrowNext steps,
    K.onEvent RegisterEventPrevious "go to previous state" $ do
      reg %= registerPrevious steps
      arrows %= arrowPrevious steps
  ]

appEvent :: T.BrickEvent () e -> T.EventM () St ()
appEvent (T.VtyEvent (V.EvKey k mods)) = do
  -- Dispatch the key to the event handler to which the key is mapped,
  -- if any. Also record in lastKeyHandled whether the dispatcher
  -- found a matching handler.
  d <- use dispatcher
  lastKey .= Just (k, mods)
  lastKeyHandled <~ K.handleKey d k mods
appEvent _ =
  return ()

drawUi :: Table () -> St -> [Widget ()]
drawUi leftTable st = [body]
  where
    status =
      center $
        padTop (Pad 5) (renderTable arrow)
          <+> padTop (Pad 2) (renderTable counter <=> renderTable leftTable)
          <+> padTop (Pad 2) (renderTable registers) 

    counter :: Table ()
    counter =
      setDefaultColAlignment AlignCenter $
        table
          [ [txt "Krok", str $ show ((st ^. reg) !! 0)]
          ]
    registers :: Table ()
    registers =
      setDefaultColAlignment AlignCenter $
        table
          [ [txt "Rejestr"],
            [str $ show ((st ^. reg) !! 1)],
            [str $ show ((st ^. reg) !! 2)],
            [str $ show ((st ^. reg) !! 3)],
            [str $ show ((st ^. reg) !! 4)],
            [str $ show ((st ^. reg) !! 5)],
            [str $ show ((st ^. reg) !! 6)]
          ]
    arrow :: Table ()
    arrow =
      surroundingBorder False $
        rowBorders False $
          columnBorders False $
            table [[str ((snd $ st ^. arrows) !! i)] | i <- [0 .. 29]]
    bindingHelp :: Widget ()
    bindingHelp = txt "Next: ->/j \nPrevious: <-/k \nQuit: Ctrl-q/Esc"
    body = C.center $ vBox [status, bindingHelp]

lastKeyAttr :: AttrName
lastKeyAttr = attrName "lastKey"

app :: Table () -> M.App St e ()
app t =
  M.App
    { M.appDraw = drawUi t,
      M.appStartEvent = return (),
      M.appHandleEvent = appEvent,
      M.appAttrMap =
        const $
          attrMap
            V.defAttr
            [ (K.eventNameAttr, fg V.magenta),
              (K.eventDescriptionAttr, fg V.cyan),
              (K.keybindingAttr, fg V.yellow),
              (lastKeyAttr, fg V.white `V.withStyle` V.bold)
            ],
      M.appChooseCursor = M.showFirstCursor
    }

fileToTable :: FilePath -> IO (Table ())
fileToTable filePath = do
  contents <- readFile filePath
  let linesOfc = lines contents
      rows = map (\line -> [str line]) linesOfc
  return $ table rows

runUI :: [State] -> String -> IO ()
runUI states program = do
  leftTable2 <- fileToTable program

  let kc = K.newKeyConfig allKeyEvents defaultBindings []

  d <- case K.keyDispatcher kc $ handlers states of
    Right d -> return d
    Left _ -> do
      exitFailure

  void $
    M.defaultMain (app leftTable2) $
      St
        { _keyConfig = kc,
          _lastKey = Nothing,
          _lastKeyHandled = False,
          _reg = 0 : (take 6 $ register $ head states),
          _arrows = (0, [" "] ++ ("->" : (replicate 28 " "))),
          _customBindingsPath = Nothing,
          _dispatcher = d
        }