module Game (playGame) where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Control.Concurrent
import Data.IORef
import Data.StateVar
import Graphics.UI.GLFW
import Graphics.DrawingCombinators ((%%))
import qualified Graphics.DrawingCombinators as D
import qualified Graphics.Rendering.OpenGL.GL.Framebuffer as F
import qualified Data.Colour.SRGB as C
import qualified Data.Colour as C
import Data.Colour.Names
import System.Random
import Debug.Trace

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type V2 = D.R2

data World = World { asteroids :: [Asteroid]
                   , playerPos :: V2
                   , playerRot :: D.R
                   , playerAcc :: V2
                   , playerVel :: V2
                   , text :: String 
                   , font :: D.Font 
                   , projectiles :: [Projectile] 
                   , leftButtonPressed :: Bool }

data Asteroid = Asteroid { asterPos :: V2
                         , asterRad :: D.R
                         , asterRot :: D.R
                         , asterVel :: V2
                         , asterRotVel :: D.R
                         , asterImage :: D.Image D.Any}

makeAsteroid pos rad = do
  (x, y) <- case pos of
    Nothing -> do
      x' <- rand (-1, 1)
      y' <- rand (-1, 1)
      return (x', y')
    Just p -> return p
  r <- case rad of
    Nothing -> rand (0.5, 2)
    Just r -> return r
  vx <- rand (-0.5, 0.5)
  vy <- rand (-0.5, 0.5)
  rotv <- rand (-1, 1)
  nverts <- randi (8, 14)
  drs <- replicateM nverts (rand (- r / 10, r / 10))
  let by = 2 * pi / fI nverts
      image = D.convexPoly [ ((1 + dr) * cos (fI n * by), (1 + dr) * sin (fI n * by))
                           | (n, dr) <- zip [0 .. nverts - 1] drs ]
  return $ Asteroid { asterPos = (x, y)
                    , asterRad = r
                    , asterRot = 0
                    , asterVel = (vx, vy)
                    , asterRotVel = rotv
                    , asterImage = image }

data Projectile = Projectile { projPos :: V2
                             , projVel :: V2
                             , projBorn :: D.R }
                  
makeProjectile p rot t = Projectile p (1 * cos rot, 1 * sin rot) t

rand :: (D.R, D.R) -> IO D.R
rand (a, b) = realToFrac <$> randomRIO ((realToFrac a, realToFrac b) :: (Double, Double))

randi :: (Int, Int) -> IO Int
randi = randomRIO

tintA a c = let C.RGB r g b = C.toSRGB c in D.tint (D.Color r g b a)
tint c = tintA 1 c

norm (x, y) = sqrt (x ** 2.0 + y ** 2.0)

fI = fromIntegral

whenRef r a = do
  b <- readIORef r
  when b a
  
wrap x
  | x < -1 = wrap (x + 2)
  | x > 1 = wrap (x - 2)
  | otherwise = x

screenToScene (w, h) (x, y) = ((x - mid_w) / mid_w, (- y + mid_h) / mid_h)
  where mid_w = w / 2
        mid_h = h / 2

accel wh xy
  | n < 0.3 = (0, 0)
  | n > 1 = (relx / n, rely / n)
  | otherwise =(relx, rely)
  where rel@(relx, rely) = screenToScene wh xy
        n = norm rel
        
getRTime = realToFrac <$> getTime

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
        
windowWidth = 700
windowHeight = 700

initWorld font_path = do
  font' <- D.openFont font_path
  newIORef $ World { playerPos = (0, 0)
                   , playerRot = 0
                   , playerAcc = (0, 0)
                   , playerVel = (0, 0)
                   , asteroids = []
                   , text = ""
                   , font = font' 
                   , projectiles = [] 
                   , leftButtonPressed = False }

playGame = do
  True <- initialize
  True <- openWindow defaultDisplayOptions { displayOptions_width = windowWidth
                                           , displayOptions_height = windowHeight
                                           , displayOptions_windowIsResizable = False}
  disableMouseCursor
  setMousePosition (windowWidth `div` 2) (windowHeight `div` 2)
  loop <- newIORef True  
  worldRef <- initWorld "/usr/share/fonts/TTF/comic.ttf"
  setWindowCloseCallback (modifyIORef loop not >> return True)
  setWindowTitle "Yet Another Asteroids Clone"
  F.clear [F.ColorBuffer, F.AccumBuffer, F.StencilBuffer, F.DepthBuffer]
  t <- getRTime
  forkIO (renderLoop worldRef loop t)
  gameLoop worldRef loop t
  terminate
  where 

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
        
gameLoop worldRef loop old_t = do
  whenRef loop $ do
    world <- readIORef worldRef
    t <- getRTime
    leftPressed <- mouseButtonIsPressed MouseButton0
    let projectiles' = 
          if leftPressed && not (leftButtonPressed world) && length (projectiles world) < 3
          then makeProjectile (playerPos world) (playerRot world) t : (projectiles world)
          else projectiles world
    (mx, my) <- getMousePosition
    world' <- advanceScene (fI mx) (fI my) t (t - old_t) (world { projectiles = projectiles' 
                                                                , leftButtonPressed = leftPressed })
    writeIORef worldRef world'
    pollEvents
    sleep 0.001
    gameLoop worldRef loop t


renderLoop worldRef loop old_t = do
  t <- getRTime
  let draw = (t - old_t) > 0.01876
  when draw $ do
    world <- readIORef worldRef
    (mx, my) <- getMousePosition
    F.accum F.Load 1.0
    F.clear [F.ColorBuffer]
    F.accum F.Mult 0.7
    F.accum F.Return 1.0
    D.render (assembleScene world) --{ text = text' }))
    swapBuffers
  sleep 0.001
  whenRef loop $
    renderLoop worldRef loop (if draw then t else old_t)

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

advanceScene mx my t dt world = do
  world' <- if null (asteroids world)
            then do
              a <- makeAsteroid Nothing Nothing
              return $ world { asteroids = [a] }
            else do
              let (fs, ps, as) = resolveCollisions [] [] [] (projectiles world, asteroids world)
              new_asteroids <- foldM (\l a -> (l++) <$> fragmentAsteroid a) [] fs
              return $ world { projectiles = ps, asteroids = as ++ new_asteroids }
  return world' { playerAcc = (ax, ay)
                , playerVel =
                     let vx' = vx + dt * ax
                         vy' = vy + dt * ay
                         norm = sqrt $ vx' * vx' + vy' * vy'
                     in if norm > 1
                        then (vx' / norm, vy' / norm)
                        else (vx', vy')
                , playerPos = (wrap $ x + dt * vx, wrap $ y + dt * vy)
                , playerRot = uncurry (flip atan2) (screenToScene (w, h) (mx, my))
                , asteroids = map advanceAsteroid (asteroids world')
                , projectiles = map advanceProjectile $ filter ((<2) . (t-) . projBorn) (projectiles world') }
  where w = fI windowWidth
        h = fI windowHeight
        (x, y) = playerPos world
        (vx, vy) = playerVel world
        (ax, ay) = accel (w, h) (mx, my)
        advanceAsteroid a = a { asterPos = (wrap $ x + dt * vx, wrap $ y + dt * vy)
                              , asterRot = asterRot a + asterRotVel a * dt }
          where (x, y) = asterPos a
                (vx, vy) = asterVel a
        advanceProjectile p = p { projPos = (wrap $ x + dt * vx, wrap $ y + dt * vy) }
          where (x, y) = projPos p
                (vx, vy) = projVel p
        fragmentAsteroid a = replicateM n (makeAsteroid (Just $ asterPos a) (Just $ asterRad a / 2))
          where n = if (asterRad a) < 1 then 0 else 3 
        collidesWith p a = distance <= 0
          where (px, py) = projPos p
                (ax, ay) = asterPos a
                distance = norm (px - ax, py - ay) - asterRad a / 10
        resolveCollisions fragment remps remas ([], as) = (fragment, remps, remas ++ as)
        resolveCollisions fragment remps remas (p : ps, []) = resolveCollisions fragment (p : remps) [] (ps, remas)
        resolveCollisions fragment remps remas (pss@(p : ps), a : as)
          | collidesWith p a = resolveCollisions (a : fragment) remps remas (ps, as)
          | otherwise = resolveCollisions fragment remps (a : remas) (pss, as)
  

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

assembleScene world =
  -- direction indicator
  (D.rotate (playerRot world) 
   %% tintA 0.3 red (D.line (0.95, 0) (1.5, 0))) <>
  -- diagnostic text
  (D.translate (-1, 0.9) 
   %% D.scale 0.04 0.04 
   %% tint red (D.text (font world) (text world))) <>
  -- ship
  (D.translate (playerPos world) 
   %% D.scale 0.04 0.04 
   %% D.rotate (playerRot world) 
   %% ((tint yellow (D.convexPoly [(-0.5, 0.3), (-0.5, -0.3), (-0.5 - 2 * norm (playerAcc world), 0)])) <> 
       (D.translate (0.5, 0) 
        %% D.scale 0.5 0.5 
        %% tint blue (D.regularPoly 3)) <> 
       tint green (D.regularPoly 3))) <>
  -- projectiles
  foldl (\im p -> 
          im <> 
          (D.translate (projPos p) 
           %% D.scale 0.008 0.008
           %% tint red D.circle)) 
  mempty (projectiles world) <>
  -- asteroids
  foldl (\im a ->
          im <> 
          (D.translate (asterPos a) 
           %% D.scale (asterRad a / 10) (asterRad a / 10) 
           %% D.rotate (asterRot a) 
           %% tint lightslategrey (asterImage a))) 
  mempty (asteroids world)
