module Main where
import Control.Lens
import Control.Monad
import Prettyprinter (pretty)
import Space
import Space.Interface.REPL
import Space.Evaluator.Implementation.Pure
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Foldable (toList)
import Data.Colour.RGBSpace.HSL
import Data.Colour.RGBSpace
import Graphics.Gloss.Interface.IO.Simulate
import Graphics.Gloss.Data.Color
import Control.Concurrent.STM.TChan
import Control.Concurrent.Async
import Prelude hiding ( lines )
import Control.Monad (void)
import Control.Monad.STM
import System.Exit (exitSuccess)
import Data.Hashable

main :: IO ()
main = do
 ch <- newTChanIO
 putStrLn (spaceiStdConfig ^. siWelcome)
 repl <- async (dispatch ch mempty)
 viz <- async (visualizer ch) 
 _ <- wait repl
 exitSuccess


cellSize :: Float
cellSize = 64.0

visualizer :: TChan MachineMemory -> IO ()
visualizer ch = do
  let initialstate = blank
  simulateIO window background fps initialstate render update
 where
        window          = InWindow "Space" (1280, 1280) (0, 0)
        background      = white
        fps             = 12
        render xs       =  pure xs
        update _ _ p    = do
          res <- atomically $ tryReadTChan ch
          case res of
            Nothing -> pure p
            Just so -> pure $ Rotate (180.0) $ paint $ viz so

newtype Cell = Cell ((Int,Int),Picture)

paint :: Cell -> Picture
paint (Cell (_,p)) = p

instance Semigroup Cell where
  (<>) (Cell ((lw,lh),lp)) (Cell ((rw,rh),rp)) =
         Cell ((lw + rw, max lh rh)
              , Translate (cellSize*(fromIntegral (lw-rw))/2.0) 0
                  (Pictures [Translate (-cellSize*(fromIntegral lw)/2.0) 0 lp
                            ,Translate (cellSize*(fromIntegral rw)/2.0) 0 rp
                            ]
                  )
              )

instance Monoid Cell where
  mempty = Cell ((0,0),blank)

pivot :: Cell -> Cell
pivot (Cell ((w,h),p)) = Cell ((h,w),Rotate (90.0) p)


class Viz a where
  viz :: a -> Cell

instance Viz Int where
  viz i = Cell ((1,1),Pictures [cell i,Scale 0.2 0.2 $ Text (show i)])

instance Viz Char where
  viz i = Cell ((1,1),Pictures [cell i,Scale 0.2 0.2 $ Text [i]])

instance Viz Variable where
  viz (Variable s) = Cell ((1,1), Pictures [cell s, Scale 0.2 0.2 $ Text s])

instance Viz Location where
  viz DLocation = Cell ((1,1), Pictures [cell (show DLocation), Scale 0.2 0.2 $ Text "@"])
  viz (Location l) = Cell ((1,1), Pictures [cell l, Scale 0.2 0.2 $ Text l])

instance Viz Term where
  viz SEmpty = Cell ((1,1),Pictures [cell (show SEmpty),Scale 0.4 0.4 (Text "*")])
  viz (SVariable v t) = viz v <> viz t
  viz (SInteger i t) = viz i <> viz t
  viz (SChar c t) = viz c <> viz t
  viz (SPush t1 l t2) = viz t1 <> viz l <> viz t2
  viz (SPop v l t) = viz v <> viz l <> viz t
  viz (SPopT v l _s t) = viz v <> viz l <> viz t

instance (Viz l, Viz r) => Viz (l,r) where
  viz (l,r) = viz l <> viz r

instance (Viz k, Viz v) => Viz (Map k v) where
  viz m = mconcat (pivot . viz <$> Map.toList m)

instance Viz a => Viz (Stack a) where
  viz (Stack s) = mconcat (pivot . viz <$> toList s)

instance Viz MachineMemory where
  viz (Memory sp st bi) = mconcat (pivot <$> [viz sp,viz st, viz bi])

cell :: Hashable a => a -> Picture
cell a = Pictures [ (color (hashColor a) (rectangleSolid cellSize cellSize))
                  , Translate 0 (2.0 - cellSize/2.0) (color black (rectangleSolid cellSize 4.0))
                  ]

hashColor :: Hashable a => a -> Color
hashColor a =
  let ha = abs (hash a)
      v = [0.35, 0.5, 0.65]
      h = fromIntegral (ha `rem` 359)
      s = v!!((ha `div` 360) `rem` 3)
      l = v!!((ha `div` 3) `rem` 3)
      (RGB r g b) = hsl h s l
   in normalizeColor r g b 1.0

normalizeColor :: Float -> Float -> Float -> Float -> Color
normalizeColor r g b a
 = let  m               = maximum [r, g, b]
    in   makeColor (r / m) (g / m) (b / m) a

