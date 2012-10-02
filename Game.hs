module Game (playGame) where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Control.Concurrent
import Data.IORef
import Data.StateVar
import Graphics.UI.GLFW
import Graphics.DrawingCombinators hiding (tint)
import qualified Graphics.DrawingCombinators as Draw
import qualified Graphics.Rendering.OpenGL.GL.Framebuffer as F
import qualified Data.Colour.SRGB as C
import qualified Data.Colour as C
import Data.Colour.Names
import System.Random (getStdGen, randoms)
import Control.Conditional hiding (when, (|>))
import qualified Data.List (sortBy)
import Utils
import Data

-- data declarations

windowWidth :: Int
windowHeight :: Int
windowWidth = 700
windowHeight = 700

initGame :: IO World
initGame = do
  font' <- openFont "comic.ttf"
  stars <- replicateM 100 $ do
    x <- rand (-1, 1)
    y <- rand (-1, 1)
    return (x, y)
  g <- getStdGen
  [ bck, shp, shpThrust, proj, a_big, a_mid, a_sml, expl] <- mapM openSprite [ "backdrop.png"
                                                                             , "ship.png"
                                                                             , "ship_thrust.png"
                                                                             , "projectile.png"
                                                                             , "asteroid_big.png"
                                                                             , "asteroid_mid.png"
                                                                             , "asteroid_sml.png"
                                                                             , "explosion.png" ]
  return $ initWorld $ World { font = font'
                             , asteroidsLot = makeAsteroids 0 (map realToFrac (randoms g :: [Double]))
                             , background = stars
                             , backdrop = bck
                             , shipSprite = shp
                             , shipThrustSprite = shpThrust
                             , projectileSprite = proj
                             , asterKindBig = Big a_big 2
                             , asterKindMid = Mid a_mid 1
                             , asterKindSml = Sml a_sml 0.4
                             , highestScore = Nothing
                             , explosionSprite = expl }

-- game entry point

playGame :: IO ()
playGame = do
  True <- initialize
  True <- openWindow defaultDisplayOptions { displayOptions_width = windowWidth
                                           , displayOptions_height = windowHeight
                                           , displayOptions_windowIsResizable = False}
  world <- initGame
  loopRef <- newIORef True
  setWindowCloseCallback (modifyIORef loopRef not >> return True)
  setWindowTitle "Yet Another Asteroids Clone"
  F.clear [F.ColorBuffer, F.AccumBuffer, F.StencilBuffer, F.DepthBuffer]
  t <- getRTime
  gameLoop loopRef world t t
  terminate

-- game control logic
  
gameLoop :: IORef Bool -> World -> D -> D -> IO ()
gameLoop loopRef world old_gt old_rt = do
  loopref <- readIORef loopRef
  when (loopref && loop world) $ do
      t <- getRTime
      [space, esc, leftKey, rightKey, upKey] <- mapM keyIsPressed [ KeySpace, KeyEsc, KeyLeft, KeyRight, KeyUp ]      
      pollEvents
      sleep 0.001      
      let dt = t - old_gt
      let world' = world |> playerAsteroidCollision t |> checkExplosionEnd dt |> checkQuit esc |> welcomeToGame space |> gameToWelcome esc |> fire t space |> up dt upKey |> left dt leftKey |> right dt rightKey
      renderLoop loopRef (advanceScene t dt world') t old_rt
        -- renderLoop loopRef (advanceScene t dt (world' { spaceWasPressed = space
        --                                             , escWasPressed = esc })) t old_rt
  where 
        playerAsteroidCollision t w
          | length no_coll /= length (asteroids w) = w { explosion = Just (Explosion t 0)
                                                       , asteroids = asterSort no_coll }
          | otherwise = w
          where no_coll = filter (not . collidesWithPlayer (playerPos world)) (asteroids world)
        checkExplosionEnd dt w = case explosion w of
          Nothing -> w
          Just (Explosion st dt) -> if st == dt then initWorld w else w
        checkQuit True w = 
          case screen w of
            WelcomeScreen | not (escWasPressed w) -> w { loop = False
                                                       , escWasPressed = True }
            _ -> w
        checkQuit False w = w { escWasPressed = False }
        welcomeToGame True w =
          case screen w of
            WelcomeScreen | not (spaceWasPressed w) -> w { screen = GameScreen
                                                         , spaceWasPressed = True }
            _ -> w
        welcomeToGame False w = w { spaceWasPressed = False }
        gameToWelcome True w = 
          case screen w of
            GameScreen -> w { screen = WelcomeScreen 
                            , escWasPressed = True }
            _ -> w
        gameToWelcome False w = w { escWasPressed = False }
        fire t True w =
          case screen w of
            GameScreen | not (spaceWasPressed w) && length (projectiles w) < 5 -> 
              w { projectiles = makeProjectile (playerPos w) (playerRot w) t : (projectiles w) 
                , spaceWasPressed = True }
            _ -> w
        fire _ False w = w
        up dt True w =
          case screen w of
            GameScreen -> if playerAcc w >= 1.0 then w { playerAcc = 1.0 }
                          else w { playerAcc = playerAcc w + 10 * dt }
            _ -> w
        up dt False w = if playerAcc w <= 0.0 then w { playerAcc = 0.0 }
                        else w { playerAcc = playerAcc w - 10 * dt }
        left dt True w = w { playerRot = wrapRad (playerRot w + 4 * dt) }
        left _ False w = w
        right dt True w = w { playerRot = wrapRad (playerRot w - 4 * dt) }
        right _ False w = w
        collidesWithPlayer (px, py) a = distance <= 0
          where (ax, ay) = asterPos a
                distance = norm (px - ax, py - ay) - asterRad (asterKind a) / 10 - 0.04

-- advancing time

manageAsteroids :: World -> World
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
      in world { projectiles = ps, asteroids = asterSort (as ++ from_fragments')
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

advanceScene :: D -> D -> World -> World
advanceScene t dt world' =
  let world = world' |> manageAsteroids |> checkScore
  in case screen world of
    GameScreen ->
      let a = playerAcc world
          phi = playerRot world
          (x, y) = playerPos world
          (vx, vy) = playerVel world
          (ax, ay) = (a * cos phi, a * sin phi)
          vx' = vx + dt * ax
          vy' = vy + dt * ay
          norm' = norm (vx', vy')
      in world { playerVel = if norm' > 1
                             then (vx' / norm', vy' / norm')
                             else (vx', vy')
               , playerPos = wrapV (x + dt * vx, y + dt * vy)
               , asteroids = map advanceAsteroid (asteroids world)
               , projectiles = map advanceProjectile $ filter ((<1.5) . (t-) . projBorn) (projectiles world)
               , explosion = advanceExplosion t (explosion world) }
    _ -> world { asteroids = map rotateAsteroid (asteroids world) 
               , explosion = advanceExplosion t (explosion world) }
  where checkScore w
          | s == 0 = w
          | otherwise = w { highestScore = case highestScore w of
                               Nothing -> Just s
                               Just hs -> Just (max hs s) }
          where s = score w
        advanceAsteroid a = rotateAsteroid (a { asterPos = wrapV (x + dt * vx, y + dt * vy) })
          where (x, y) = asterPos a
                (vx, vy) = asterVel a
        rotateAsteroid a = a { asterRot = wrapRad (asterRot a + asterRotVel a * dt) }
        advanceProjectile p = p { projPos = wrapV (x + dt * vx, y + dt * vy) }
          where (x, y) = projPos p
                (vx, vy) = projVel p
        advanceExplosion _ Nothing = Nothing
        advanceExplosion t (Just (Explosion st dt))
          | dt > 0.5 = Just (Explosion 0 0)
          | otherwise = Just (Explosion st (t - st))
        
  
-- rendering

renderLoop :: IORef Bool -> World -> D -> D -> IO ()
renderLoop loopRef world old_gt old_rt = do
  t <- getRTime
  let draw = (t - old_rt) > 0.01876
  when draw $ do
    F.accum F.Load 1.0
    F.clear [F.ColorBuffer]
    F.accum F.Mult 0.85
    F.accum F.Return 1.0
    render (assembleScene world)
    swapBuffers
  sleep 0.001
  if draw
    then gameLoop loopRef world old_gt t
    else gameLoop loopRef world old_gt old_rt

assembleScene :: World -> Image Any
assembleScene world =
    showExplosion $
  textLayer $
  -- ship
  (translate ppos
   %% rotate phi
   %% scale (1.38 * 0.05) 0.05
   %% (sprite (shipSprite world)) <> (translate (0.2 - playerAcc world / 5, 0) %% sprite (shipThrustSprite world)))
  -- projectiles
  <> (mconcatmap (projectiles world) $ \p ->
       translate (projPos p)
       %% rotate (projRot p - pi / 2)
       %% scale 0.01 0.01
       %% sprite (projectileSprite world))
  -- asteroids
  <> (mconcatmap (asteroids world) $ \a ->
       translate (asterPos a)
       %% scale (0.1 * (asterRad (asterKind a))) (0.1 * (asterRad (asterKind a)))
       %% rotate (asterRot a)
       %% sprite (asterSprite (asterKind a)))
  -- text
  <> (mconcatmap (background world) $ \pos -> (tint white $ point pos))
  <> (sprite (backdrop world))
  where ppos = playerPos world
        phi = playerRot world
        h_score = case highestScore world of
          Just i -> "highest score: " ++ show i
          Nothing -> ""
        textLayer rest = case screen world of
          GameScreen -> (tint red (showScore ("score: " ++ show (score world)) 0.03 (-0.95) 0.9)) <> rest
          WelcomeScreen -> (tint lightgreen (centered title 0.08 0.2))
                           <> (tint green (centered "press" 0.04 (-0.2)))
                           <> (tint green (centered "space to play" 0.04 (-0.3)))
                           <> (tint green (centered "esc to pause" 0.04 (-0.4)))
                           <> (tint green (centered "esc esc to quit" 0.04 (-0.5)))
                           <> (tint red (showScore h_score 0.03 (-0.95) 0.9))
                           <> tint grey rest
        centered txt sc y = translate (0, y)
                            %% scale sc sc
                            %% translate (- (textWidth (font world) txt) / 2, 0) 
                            %% (text (font world) txt)
        showScore txt sc x y = translate (x, y)
                               %% scale sc sc 
                               %% (text (font world) txt)
        showExplosion rest = case explosion world of
          Nothing -> rest
          Just (Explosion st dt) -> (translate ppos
                                     %% scale (0.3 * (sqrt dt + 0.1)) (0.3 * (sqrt dt + 0.1))
                                     %% (sprite (explosionSprite world)))
                                    <> rest
        title = "Asteroids"