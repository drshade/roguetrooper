-- | Procedural 8-bit pixel art: sprites authored as rows of characters (palette
-- lookup), drawn as blocks. Presentation only.
module RogueTrooper.Art
  ( Sprite (..)
  , artPx
  , drawSprite
  , renderTurret
  , renderMissile
  , paratrooperSprite
  , trooperSprite
  ) where

import           Raylib.Core.Shapes  (drawRectanglePro, drawRectangleV)
import           Raylib.Types        (Color, Vector2, pattern Rectangle, pattern Vector2)
import qualified Raylib.Util.Colors  as Colors
import           Raylib.Util.Math    ((|-|))

-- | A pixel sprite: a palette mapping chars to colours, and rows of chars
-- (' ' = transparent).
data Sprite = Sprite
  { spritePalette :: [(Char, Color)]
  , spriteRows    :: [String]
  }

-- | Screen size of one art pixel.
artPx :: Float
artPx = 4

-- | Draw a sprite centred on a point, each non-space char drawn as one art pixel.
drawSprite :: Vector2 -> Sprite -> IO ()
drawSprite (Vector2 cx cy) (Sprite palette rows) =
  sequence_
    [ drawRectangleV (Vector2 (x0 + fromIntegral col * artPx) (y0 + fromIntegral row * artPx))
                     (Vector2 artPx artPx) c
    | (row, line) <- zip [0 :: Int ..] rows
    , (col, ch)   <- zip [0 :: Int ..] line
    , c           <- maybe [] pure (lookup ch palette)
    ]
  where
    h  = length rows
    w  = maximum (map length rows)
    x0 = cx - fromIntegral w * artPx / 2
    y0 = cy - fromIntegral h * artPx / 2

-- Turret ---------------------------------------------------------------------

turretSprite :: Sprite
turretSprite = Sprite
  [('d', Colors.darkGray), ('G', Colors.gray), ('L', Colors.lightGray)]
  [ "  dGGd  "
  , " dGLLGd "
  , "dGLLLLGd"
  , "dGGGGGGd"
  , "dGGGGGGd"
  , "ddGGGGdd"
  ]

-- | The turret: base/dome sprite at the tower with a barrel that swivels to
-- point at the scanbox.
renderTurret :: Vector2 -> Vector2 -> IO ()
renderTurret tower scan = do
  let Vector2 dx dy = scan |-| tower
      angle = atan2 dy dx * 180 / pi
      Vector2 ptx pty = tower
      bar len th col = drawRectanglePro (Rectangle ptx pty len th) (Vector2 0 (th / 2)) angle col
  bar 34 12 Colors.darkGray    -- barrel body (dark end reads as a muzzle)
  bar 28 5  Colors.lightGray   -- highlight stripe, shorter so a muzzle shows
  drawSprite tower turretSprite

-- Missile --------------------------------------------------------------------

-- | A missile, drawn as a small dart rotated to face its velocity.
renderMissile :: Vector2 -> Vector2 -> IO ()
renderMissile (Vector2 cx cy) (Vector2 vx vy) = do
  let angle = if vx == 0 && vy == 0 then -90 else atan2 vy vx * 180 / pi
      -- a rect of size w×h with its local point (ox, h/2) placed at (cx,cy)
      seg w h ox col = drawRectanglePro (Rectangle cx cy w h) (Vector2 ox (h / 2)) angle col
  seg 4 4 13   Colors.gold     -- exhaust flame at the tail
  seg 16 6 8   Colors.gray     -- body
  seg 6 6 (-2) Colors.orange   -- nose, forward

-- Paratroopers ---------------------------------------------------------------

-- | A descending paratrooper: a big canopy + strings + trooper.
paratrooperSprite :: Sprite
paratrooperSprite = Sprite
  [('r', Colors.maroon), ('s', Colors.lightGray), ('h', Colors.beige), ('b', Colors.green)]
  [ "    rrrrrrr    "
  , "  rrrrrrrrrrr  "
  , " rrrrrrrrrrrrr "
  , "rrrrrrrrrrrrrrr"
  , " rrrrrrrrrrrrr "
  , "  s s s s s s  "
  , "    s  s  s    "
  , "      hhh      "
  , "     bbbbb     "
  , "     bbbbb     "
  , "     b   b     "
  ]

-- | A grounded trooper (no canopy).
trooperSprite :: Sprite
trooperSprite = Sprite
  [('h', Colors.beige), ('b', Colors.green)]
  [ "  hhh  "
  , " bbbbb "
  , " bbbbb "
  , " b   b "
  ]
