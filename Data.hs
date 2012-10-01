module Data where

import Utils
import Graphics.DrawingCombinators

data Screen = WelcomeScreen | GameScreen

data World = World { asteroids :: [Asteroid]
                   , playerPos :: V2
                   , playerRot :: D
                   , playerAcc :: D
                   , playerVel :: V2
                   , font :: Font 
                   , projectiles :: [Projectile] 
                   , spaceWasPressed :: Bool
                   , escWasPressed :: Bool 
                   , asteroidsLot :: [Asteroid] 
                   , background :: [V2] 
                   , backdrop :: Sprite
                   , explosion :: Maybe Explosion
                   , explosionSprite :: Sprite 
                   , shipSprite :: Sprite
                   , shipThrustSprite :: Sprite 
                   , projectileSprite :: Sprite
                   , asterKindBig :: AsteroidKind
                   , asterKindMid :: AsteroidKind
                   , asterKindSml :: AsteroidKind 
                   , screen :: Screen 
                   , highestScore :: Maybe Int 
                   , score :: Int 
                   , loop :: Bool }

data AsteroidKind = Big { asterSprite :: Sprite, asterRad :: D }
                  | Mid { asterSprite :: Sprite, asterRad :: D }
                  | Sml { asterSprite :: Sprite, asterRad :: D }
                  | Null

data Asteroid = Asteroid { asterPos :: V2
                         , asterRot :: D
                         , asterVel :: V2
                         , asterRotVel :: D
                         , asterKind :: AsteroidKind }
                
data Explosion = Explosion { exploSt :: R
                           , exploDt :: R }
                 
data Projectile = Projectile { projPos :: V2
                             , projVel :: V2
                             , projBorn :: D
                             , projRot :: D }

-- some smart constructors

makeAsteroids :: [D] -> [Asteroid]
makeAsteroids (a : b : c : d : e : rlist) =
  let x = range (-1, 1) a
      y = range (-1, 1) b
      vx = range (-0.5, 0.5) c
      vy = range (-0.5, 0.5) d
      rotv = range (-1, 1) e
  in Asteroid { asterPos = (x, y)
              , asterRot = 0
              , asterVel = (vx, vy)
              , asterRotVel = rotv 
              , asterKind = Null } : makeAsteroids rlist

makeProjectile :: V2 -> D -> D -> Projectile
makeProjectile p rot t = Projectile p (cos rot, sin rot) t (rot)

initWorld :: World -> World
initWorld world =  
  world { playerPos = (0, 0)
        , playerRot = pi / 2
        , playerAcc = 0
        , playerVel = (0, 0)
        , asteroids = []
        , projectiles = [] 
        , spaceWasPressed = False                 
        , escWasPressed = False
        , screen = WelcomeScreen 
        , score = 0 
        , loop = True 
        , explosion = Nothing }