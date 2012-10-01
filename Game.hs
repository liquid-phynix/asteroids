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
import Control.Conditional hiding (when)

import Debug.Trace

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

type V2 = D.R2

data Screen = WelcomeScreen | GameScreen

data World = World { asteroids :: [Asteroid]
                   , playerPos :: V2
                   , playerRot :: D.R
                   , playerAcc :: D.R
                   , playerVel :: V2
                   , text :: String 
                   , font :: D.Font 
                   , projectiles :: [Projectile] 
                   , spaceWasPressed :: Bool
                   , escWasPressed :: Bool 
                   , asteroidsLot :: [Asteroid] 
                   , background :: [V2] 
                   , backdrop :: D.Sprite
                   , explosion :: Maybe Explosion
                   , explosionSprite :: D.Sprite 
                   , shipSprite :: D.Sprite
                   , shipThrustSprite :: D.Sprite 
                   , projectileSprite :: D.Sprite
                   , asterKindBig :: AsteroidKind
                   , asterKindMid :: AsteroidKind
                   , asterKindSml :: AsteroidKind 
                   , screen :: Screen 
                   , highestScore :: Maybe Int 
                   , score :: Int 
                   , loop :: Bool }

data AsteroidKind = Big { asterSprite :: D.Sprite, asterRad :: D.R }
                  | Mid { asterSprite :: D.Sprite, asterRad :: D.R }
                  | Sml { asterSprite :: D.Sprite, asterRad :: D.R }
                  | Null
data Asteroid = Asteroid { asterPos :: V2
                         , asterRot :: D.R
                         , asterVel :: V2
                         , asterRotVel :: D.R
                         , asterKind :: AsteroidKind }
                
data Explosion = Explosion { exploSt :: D.R
                           , exploDt :: D.R }

makeAsteroids (a : b : c : d : e : f : g : rlist) =
  let x = range (-1, 1) a
      y = range (-1, 1) b
      vx = range (-0.5, 0.5) d
      vy = range (-0.5, 0.5) e
      rotv = range (-1, 1) f
      in Asteroid { asterPos = (x, y)
                  , asterRot = 0
                  , asterVel = (vx, vy)
                  , asterRotVel = rotv 
                  , asterKind = Null } : makeAsteroids rlist
    where range (a, b) r = (b - a) * r + a

data Projectile = Projectile { projPos :: V2
                             , projVel :: V2
                             , projBorn :: D.R 
                             , projRot :: D.R }
                  
makeProjectile p rot t = Projectile p (cos rot, sin rot) t (rot)

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
  
whenIO c a = do
  b <- c
  when b a  
  
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

wrap x
  | x < -1 = wrap (x + 2)
  | x > 1 = wrap (x - 2)
  | otherwise = x

wrapRad phi
  | phi < 0 = phi + pi2
  | phi > pi2 = phi - pi2
  | otherwise = phi
    where pi2 = 2 * pi
                
wrapV (x, y) = (wrap x, wrap y)

-- rotV phi (x, y) = (cp * x + sp * y, - sp * x + cp * y)
--   where cp = cos phi
--         sp = sin phi
        
-- addV (x, y) (x', y') = (x + x', y + y')
-- minV (x, y) (x', y') = (x - x', y - y')
-- negateV (x, y) = (-x, -y)

mconcatmap lst f = mconcat (map f lst)


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
        
windowWidth = 700
windowHeight = 700

initGame font_path = do
  font' <- D.openFont font_path
  stars <- replicateM 100 $ do
    x <- rand (-1, 1)
    y <- rand (-1, 1)
    return (x, y)
  g <- getStdGen
  [bck, shp, shpThrust, proj, a_big, a_mid, a_sml, expl] <- mapM D.openSprite [ "backdrop.png", "ship.png", "ship_thrust.png", "projectile.png", "asteroid_big.png", "asteroid_mid.png", "asteroid_sml.png", "explosion.png" ]
  return $ initWorld $ World { font = font' 
                             , asteroidsLot = makeAsteroids (map realToFrac (randoms g :: [Double])) 
                             , background = stars 
                             , backdrop = bck 
                             , shipSprite = shp
                             , shipThrustSprite = shpThrust 
                             , projectileSprite = proj 
                             , asterKindBig = Big a_big 2
                             , asterKindMid = Mid a_mid 1
                             , asterKindSml = Sml a_sml 0.4
                             , highestScore = Nothing 
                             , explosionSprite = expl
                             , playerPos = (0, 0)
                             , playerRot = pi / 2
                             , playerAcc = 0
                             , playerVel = (0, 0)
                             , asteroids = []
                             , text = ""
                             , projectiles = [] 
                             , spaceWasPressed = False                 
                             , escWasPressed = False
                             , screen = WelcomeScreen 
                             , score = 0 
                             , loop = True 
                             , explosion = Nothing }

initWorld world =  
  world { playerPos = (0, 0)
        , playerRot = pi / 2
        , playerAcc = 0
        , playerVel = (0, 0)
        , asteroids = []
        , text = ""
        , projectiles = [] 
        , spaceWasPressed = False                 
        , escWasPressed = False
        , screen = WelcomeScreen 
        , score = 0 
        , loop = True 
        , explosion = Nothing }

  
playGame = do
  True <- initialize
  True <- openWindow defaultDisplayOptions { displayOptions_width = windowWidth
                                           , displayOptions_height = windowHeight
                                           , displayOptions_windowIsResizable = False}
  world <- initGame "comic.ttf"
  loopRef <- newIORef True
  setWindowCloseCallback (modifyIORef loopRef not >> return True)
  setWindowTitle "Yet Another Asteroids Clone"
  F.clear [F.ColorBuffer, F.AccumBuffer, F.StencilBuffer, F.DepthBuffer]
  t <- getRTime
  gameLoop loopRef world t t
  terminate

gameLoop loopRef world old_gt old_rt = do
  whenRef loopRef $
    when (loop world) $ do
      t <- getRTime
      
      let world' = let non_coll = filter (not . collidesWithPlayer (playerPos world)) (asteroids world)
                   in if length non_coll /= length (asteroids world)
                      then world { explosion = Just (Explosion t 0) 
                                 , asteroids = non_coll }
                      else world
      
      let world'' = case explosion world' of
            Nothing -> world'
            Just (Explosion st dt) -> if st == dt then initWorld world' else world'
      
      [space, esc, left, right, up] <- mapM keyIsPressed [ KeySpace, KeyEsc, KeyLeft, KeyRight, KeyUp ]
      let world''' = gameStateLogic world'' t (t - old_gt) (screen world) (spaceWasPressed world) space (escWasPressed world) esc left right up
      pollEvents
      sleep 0.001
      renderLoop loopRef (advanceScene t (t - old_gt) (world''' { spaceWasPressed = space, escWasPressed = esc })) t old_rt
  where 
        collidesWithPlayer (px, py) a = distance <= 0
          where (ax, ay) = asterPos a
                distance = norm (px - ax, py - ay) - asterRad (asterKind a) / 10 - 0.04
--      gameStateLogic w t dt screen        spacewas space escwas esc  left  right up
        gameStateLogic w t dt WelcomeScreen _        _     False  True _     _     _     = w { loop = False }
        gameStateLogic w t dt WelcomeScreen _        _     True   True _     _     _     = w { escWasPressed = True }
        
        gameStateLogic w t dt WelcomeScreen False    True  _      e    _     _     _     = w { screen = GameScreen
                                                                                             , spaceWasPressed = True 
                                                                                             , escWasPressed = e }
        gameStateLogic w t dt GameScreen    _        _     _      True _     _     _     = w { screen = WelcomeScreen }
        gameStateLogic w t dt GameScreen    False    True  _      _    _     _     _
          | length (projectiles w) < 5 = w { projectiles = makeProjectile (playerPos w) (playerRot w) t : (projectiles w) }
          | otherwise = w
        gameStateLogic w t dt GameScreen    _        _     _      _    l     r     up    =
          let w' = if up 
                   then if playerAcc w >= 1.0 then w { playerAcc = 1.0 }
                        else w { playerAcc = playerAcc w + 10 * dt }
                   else if playerAcc w <= 0.0 then w { playerAcc = 0.0 }
                        else w { playerAcc = playerAcc w - 10 * dt }
          in if l
             then w' { playerRot = wrapRad $ playerRot w' + 4 * dt }
             else if r
                  then w' { playerRot = wrapRad $ playerRot w' - 4 * dt }
                  else w'          
        gameStateLogic w _ _  _             _        _     _      _    _     _     _     = w
          


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

manageAsteroids world
  | null (asteroids world) =
    world { asteroids = [ (head (asteroidsLot world)) { asterKind = (asterKindBig world) }
                        , ((head . tail) (asteroidsLot world)) { asterKind = (asterKindMid world) } ]
          , asteroidsLot = tail (asteroidsLot world) }
  | otherwise =
      let (fs, ps, as) = resolveCollisions [] [] [] (projectiles world, asteroids world)
          (from_fragments', new_lot') =
            foldl (\(acc, lot) a -> 
                    let (from_fragments, new_lot) = fragmentAsteroid a lot
                    in (from_fragments ++ acc, new_lot))
            ([], asteroidsLot world) fs
      in world { projectiles = ps, asteroids = as ++ from_fragments'
               , asteroidsLot = new_lot' 
               , score = score world + length fs }
  where fragmentAsteroid a lot = (map (\b -> b { asterPos = asterPos a
                                               , asterKind = newkind }) (take n lot), drop n lot)
          where (n, newkind) = case (asterKind a) of
                  Big _ _ -> (3, asterKindMid world)
                  Mid _ _ -> (2, asterKindSml world)
                  Sml _ _ -> (0, asterKindSml world)
        collidesWith p a = distance <= 0
          where (px, py) = projPos p
                (ax, ay) = asterPos a
                distance = norm (px - ax, py - ay) - asterRad (asterKind a) / 10
        resolveCollisions fragment remps remas ([], as) = (fragment, remps, remas ++ as)
        resolveCollisions fragment remps remas (p : ps, []) = resolveCollisions fragment (p : remps) [] (ps, remas)
        resolveCollisions fragment remps remas (pss@(p : ps), a : as)
          | collidesWith p a = resolveCollisions (a : fragment) remps remas (ps, as)
          | otherwise = resolveCollisions fragment remps (a : remas) (pss, as)

advanceScene t dt world' =
  let world = let world'' = (manageAsteroids world')
                  s = score world'' 
              in if s == 0 then world''
                 else world'' { highestScore = case highestScore world'' of
                                   Nothing -> Just s
                                   Just hs -> Just (max hs s) }
  in case screen world of
    GameScreen ->
      let a = playerAcc world
          phi = playerRot world
          (x, y) = playerPos world
          (vx, vy) = playerVel world
          (ax, ay) = (a * cos phi, a * sin phi)
          vx' = vx + dt * ax
          vy' = vy + dt * ay
          norm = sqrt $ vx' * vx' + vy' * vy'
      in world { playerVel =
                    if norm > 1
                    then (vx' / norm, vy' / norm)
                    else (vx', vy')
               , playerPos = (wrap $ x + dt * vx, wrap $ y + dt * vy)
               , asteroids = map advanceAsteroid (asteroids world)
               , projectiles = map advanceProjectile $ filter ((<1.5) . (t-) . projBorn) (projectiles world)
               , explosion = advanceExplosion t (explosion world) }
    _ -> world { asteroids = map rotateAsteroid (asteroids world) 
               , explosion = advanceExplosion t (explosion world) }
      
  where advanceAsteroid a = rotateAsteroid (a { asterPos = (wrap $ x + dt * vx, wrap $ y + dt * vy) })
          where (x, y) = asterPos a
                (vx, vy) = asterVel a
        rotateAsteroid a = a { asterRot = asterRot a + asterRotVel a * dt }
        advanceProjectile p = p { projPos = (wrap $ x + dt * vx, wrap $ y + dt * vy) }
          where (x, y) = projPos p
                (vx, vy) = projVel p
        advanceExplosion _ Nothing = Nothing
        advanceExplosion t (Just (Explosion st dt))
--          | trace ("dt: " ++ show dt) False = undefined
          | dt > 0.5 = Just (Explosion 0 0)
          | otherwise = Just (Explosion st (t - st))
        
  

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

renderLoop loopRef world old_gt old_rt = do
  t <- getRTime
  let draw = (t - old_rt) > 0.01876
  when draw $ do
    F.accum F.Load 1.0
    F.clear [F.ColorBuffer]
    F.accum F.Mult 0.85
    F.accum F.Return 1.0
    D.render (assembleScene world)
    swapBuffers
  sleep 0.001
  if draw
    then gameLoop loopRef world old_gt t
    else gameLoop loopRef world old_gt old_rt

assembleScene world =
  showExplosion $
  textLayer $
  -- ship
  (D.translate ppos
   %% D.rotate phi
   %% D.scale (1.38 * 0.05) 0.05
   %% (D.sprite (shipSprite world)) <> (D.translate (0.2 - playerAcc world / 5, 0) %% D.sprite (shipThrustSprite world)))
  -- projectiles
  <> (mconcatmap (projectiles world) $ \p ->
       D.translate (projPos p)
       %% D.rotate (projRot p - pi / 2)
       %% D.scale 0.01 0.01
       %% D.sprite (projectileSprite world))
  -- asteroids
  <> (mconcatmap (asteroids world) $ \a ->
       D.translate (asterPos a)
       %% D.scale (0.1 * (asterRad (asterKind a))) (0.1 * (asterRad (asterKind a)))
       %% D.rotate (asterRot a)
       %% D.sprite (asterSprite (asterKind a)))
  -- text
  <> (mconcatmap (background world) $ \pos -> (tint white $ D.point pos))
  <> (D.sprite (backdrop world))
  where ppos = playerPos world
        phi = playerRot world
        h_score = case highestScore world of
          Just i -> "highest score: " ++ show i
          Nothing -> ""
        textLayer rest = case screen world of
          WelcomeScreen -> (tint lightgreen (centered title 0.08 0.2))
                           <> (tint green (centered "press" 0.04 (-0.2)))
                           <> (tint green (centered "space to play" 0.04 (-0.3)))
                           <> (tint green (centered "esc to pause" 0.04 (-0.4)))
                           <> (tint green (centered "esc esc to quit" 0.04 (-0.5)))
                           <> (tint red (showScore h_score 0.03 (-0.95) 0.9))
                           <> D.tint (D.Color 0.4 0.4 0.4 1) rest
          GameScreen -> (tint red (showScore ("score: " ++ show (score world)) 0.03 (-0.95) 0.9)) <> rest
        centered text scale y = D.translate (0, y)
                                %% D.scale scale scale 
                                %% D.translate (- (D.textWidth (font world) text) / 2, 0) 
                                %% (D.text (font world) text)
        showScore text scale x y = D.translate (x, y)
                                   %% D.scale scale scale 
                                   %% (D.text (font world) text)
        showExplosion rest = case explosion world of
          Just (Explosion st dt) -> (D.translate ppos
                                     %% D.scale (0.3 * (sqrt dt + 0.1)) (0.3 * (sqrt dt + 0.1))
                                     %% (D.sprite (explosionSprite world)))
--                                     %% tintA (1 - dt) white 
                                    <> rest
          Nothing -> rest
        title = "Asteroids"