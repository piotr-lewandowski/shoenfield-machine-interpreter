{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Interface where

import Simulation
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((<~), (.=), (%=), use)
import Control.Monad (void)
import qualified Data.Text as Text
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (center)
import qualified Graphics.Vty as V
import System.Exit (exitFailure)

import qualified Brick.Types as T
import qualified Brick.Keybindings as K
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C


-- | The abstract key events for the application.
data KeyEvent = QuitEvent
              | RegisterEventNext
              | RegisterEventPrevious
              deriving (Ord, Eq, Show)

-- | The mapping of key events to their configuration field names.
allKeyEvents :: K.KeyEvents KeyEvent
allKeyEvents =
    K.keyEvents [ ("quit",      QuitEvent)
                , ("registern", RegisterEventNext)
                , ("registerp", RegisterEventPrevious)
                ]

-- | Default key bindings for each abstract key event.
defaultBindings :: [(KeyEvent, [K.Binding])]
defaultBindings =
    [ (QuitEvent,      [K.ctrl 'q', K.bind V.KEsc])
    , (RegisterEventNext, [K.bind 'r'])
    , (RegisterEventPrevious, [K.bind 'p'])
    ]

data St =
    St { _keyConfig :: K.KeyConfig KeyEvent
       -- ^ The key config to use.
       , _lastKey :: Maybe (V.Key, [V.Modifier])
       -- ^ The last key that was pressed.
       , _lastKeyHandled :: Bool
       -- ^ Whether the last key was handled by a handler.
       ,_reg :: [Int]
       -- ^ Register
       ,_arrows :: (Int,[String])
       -- ^ Points at current line
       , _customBindingsPath :: Maybe FilePath
       -- ^ Set if the application found custom keybindings in the
       -- specified file.
       , _dispatcher :: K.KeyDispatcher KeyEvent (T.EventM () St)
       -- ^ The key dispatcher we'll use to dispatch input events.
       }

makeLenses ''St

-- | Functions for handler. Updates register and arrows.
registerPrevious :: [State] -> [Int] -> [Int]
registerPrevious prog current 
	| (current !! 0) > 0 = ((current !! 0) -1 ):(take 6 $ register (prog !! ((current !! 0)-1)))
	| otherwise = take 7 $ current
	
registerNext :: [State] -> [Int] -> [Int]
registerNext prog current = ((current !! 0) +1 ):(take 6 $ register (prog !! ((current !! 0)+1)))

arrowNext :: [State] -> (Int,[String])  -> (Int,[String])
arrowNext prog (n, xs) = (n+1, (replicate (2*z +1) " ") ++ ["->"] ++ (replicate (100 - 2*z-1) " "))
	where z = instructionCounter (prog !! (n+1))
	
arrowPrevious:: [State] -> (Int,[String])  -> (Int,[String])
arrowPrevious prog (n, xs) 
	| n==0 = (n,xs)
	| otherwise = (n-1, (replicate (2*z +1) " ") ++ ["->"] ++ (replicate (100 - 2*z-1) " "))
		where z = instructionCounter (prog !! (n-1))


-- | Key event handlers for our application.
handlers :: [State] -> [K.KeyEventHandler KeyEvent (T.EventM () St)]
handlers steps =
    [ K.onEvent QuitEvent "Quit the program"
          M.halt
    , K.onEvent RegisterEventNext "register uwu owo xd" $ do
         reg %= registerNext steps 
         arrows %= arrowNext steps
    , K.onEvent RegisterEventPrevious "register uwu owo xd" $ do
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



drawUi :: Table() -> St -> [State] -> [Widget ()]
drawUi leftTable st states = [body]
    where
        binding = uncurry K.binding <$> st^.lastKey

        -- Generate key binding help using the library so we can embed
        -- it in the UI.
        keybindingHelp = B.borderWithLabel (txt "Active Keybindings") $
                        K.keybindingHelpWidget (st^.keyConfig) $ handlers states

        lastKeyDisplay = withDefAttr lastKeyAttr $
                         txt $ maybe "(none)" K.ppBinding binding

        -- Show the status of the last pressed key, whether we handled
        -- it, and other bits of the application state.
        status = center $ padTop (Pad 5) (renderTable leftTable2) <+> padTop (Pad 2)(renderTable rejestr <=> renderTable leftTable) <+>
              padTop (Pad 2)(renderTable rightTableA )
                               

	innerTable :: Table ()
	innerTable =
    		surroundingBorder False $
    		table [ [txt "inner", txt "table"]
        		  , [txt "is",    txt "here"]
        		  ]
	arrowTable :: Table ()
	arrowTable =
    		surroundingBorder False $
    		table [ [txt "inner", txt "table"]
        		  , [txt "is",    txt "here"]
        		  ]
	rejestr :: Table ()
	rejestr =
	    setDefaultColAlignment AlignCenter $
    		table [ [txt "Krok", str $ show ((st^.reg)!!0) ]
        	  	]

	

	rightTableA :: Table ()
	rightTableA =
	    
	    setDefaultColAlignment AlignCenter $
	    table [ [txt "Rejestr"]
	          , [str $ show ((st^.reg)!!1 )]
	          , [str $ show ((st^.reg)!!2 )]
	          , [str $ show ((st^.reg)!!3 )]
	          , [str $ show ((st^.reg)!!4 )]
	          , [str $ show ((st^.reg)!!5 )]
	          , [str $ show ((st^.reg)!!6 )]
	          ]
	leftTable2 :: Table ()
	leftTable2 =
	    surroundingBorder False$
	    rowBorders False$
	    columnBorders False$
	    table [ [str ((snd $ st^.arrows) !! i)] | i <- [0..29] ]	
	body = C.center $ (status ) 
	               	


lastKeyAttr :: AttrName
lastKeyAttr = attrName "lastKey"

app :: Table() -> [State] -> M.App St e ()
app t states =
    M.App { M.appDraw = \st -> drawUi t st states
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr [ (K.eventNameAttr, fg V.magenta)
                                                     , (K.eventDescriptionAttr, fg V.cyan)
                                                     , (K.keybindingAttr, fg V.yellow)
                                                     , (lastKeyAttr, fg V.white `V.withStyle` V.bold)
                                                     ]
          , M.appChooseCursor = M.showFirstCursor
          }

sectionName :: Text.Text
sectionName = "keybindings"


fileToTable :: FilePath -> IO (Table ())
fileToTable filePath = do
    contents <- readFile filePath
    let linesOfc = lines contents
    	rows = map (\line -> [str line]) linesOfc
    return $ table rows

runUI :: [State] -> String -> IO ()
runUI states program = do
    leftTable2  <- fileToTable program
	
    let 
        kc = K.newKeyConfig allKeyEvents defaultBindings []

    d <- case K.keyDispatcher kc $ handlers states of
        Right d -> return d
        Left _ -> do
            exitFailure

    void $ M.defaultMain (app leftTable2 states) $ St { _keyConfig = kc
                                  , _lastKey = Nothing
                                  , _lastKeyHandled = False
                                  , _reg =0:(take 6 $ register $ head states)
                                  , _arrows = (0, [" "]++("->" : (replicate 28 " ")))
                                  , _customBindingsPath = Nothing
                                  , _dispatcher = d
                                  }