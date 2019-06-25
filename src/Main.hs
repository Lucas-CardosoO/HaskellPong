module Main(main) where

import Graphics.Gloss.Interface.IO.Game
import System.Exit
import System.Random
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar
import qualified Data.Set as S


type Radius = Float
type Position = (Float, Float)
-- data Player1Move a = Player1Move (MVar (a, [MVar ()])) (MVar ())
-- data Player2Move a = Player2Move (MVar (a, [MVar ()])) (MVar ())
type Player1Move = MVar Float
type Player2Move = MVar Float

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: Position        -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity.
  , player1 :: Float          -- ^ Left player paddle height.
                               -- Zero is the middle of the screen.
  , player2 :: Float           -- ^ Right player paddle height.
  , keys :: S.Set Key
  } deriving Show

type State = (PongGame, Player1Move, Player2Move)

ballRadius :: Radius
ballRadius = 10

movePlayer :: (MVar Float) -> Float -> IO()
movePlayer move dist = do 
  m <- takeMVar move
  putMVar move (m + dist)
  return ()
  -- if m < 1
  --   then do 
  --     putMVar move (m - 1)
  --     movePlayer1 move
  --   else putMVar move (m - 1)
  -- game {player1 = player1 game + val}
  -- putMVar move (m - 1)
  -- movePlayer1 move game
  -- return ()

-- movePlayer2 :: (MVar Integer) -> PongGame -> IO()

valueToMove :: Integer -> Float
valueToMove x
  | x < 1 = - 10
  | x > 1 = 10
  | otherwise = 0

randomInitialState :: StdGen -> Float -> PongGame
randomInitialState gen magnitude = Game
  { ballLoc = (a, b)
  , ballVel = (c', d')
  , player1 = 0
  , player2 = 0
  , keys = S.empty
  }
  where
    a:b:c:d:_ = randomRs (-50, 50) gen
    c' = c * mag
    d' = d * mag
    mag = magnitude / sqrt (c^2 + d^2)

width, height, offset :: Num a => a
width = 1480
height = 780
offset = 100

window :: Display
window = FullScreen

background :: Color
background = black

fps :: Int
fps = 60


difficultyFunc :: Char -> Float
difficultyFunc c = 
  case c of
    'e' -> 200
    'm' -> 600
    'h' -> 1000
    _ -> 600

main :: IO ()
main = do
  gen <- getStdGen
  print "Write [e] for easy, [m] for medium or [h] for hard" 
  difficulty <- getChar
  -- Player1Move <- newMVar 0

  -- if difficulty == 'e'
  --   then let mag = 200
  --   else if difficulty == 'm'
  --     then let mag = 600
  --     else let mag = 1000
  

  let initState = randomInitialState gen (difficultyFunc difficulty)
  p1 <- newMVar 0.0
  p2 <- newMVar 0.0
  -- forkIO (movePlayer1 (p1) initState)
  playIO window background fps (initState, p1, p2) render handleKeys2 update

render :: State -> IO Picture
render (game, _, _) = return $ 
  pictures [ball, walls,
            mkPaddle white 700 $ player1 game,
            mkPaddle white (-700) $ player2 game]
  where
    --  The pong ball.
    ball = uncurry translate (ballLoc game) $ color ballColor $ circleSolid ballRadius
    ballColor = white

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 1440 10

    wallColor = white
    walls = pictures [wall 400, wall (-400)]

    --  Make a paddle of a given border and vertical offset.
    mkPaddle :: Color -> Float -> Float -> Picture 
    mkPaddle col x y = pictures
      [ translate x y $ color col $ rectangleSolid 26 86
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = white

-- | Update the game by moving the ball.
update :: Float -> State -> IO State
update seconds (game, p1, p2) = 
  if gameEnded game'
  then do
    putStrLn "Game ended!"
    exitSuccess
  else do
    m1 <- readMVar p1
    m2 <- readMVar p2
    return (game' {player1 = m1, player2 = m2}, p1, p2)

    -- if S.member (Char 'd') (keys game)
    --   then do
    --     let game' = paddleBounce . wallBounce . moveBall seconds $ game
    --     if player2 game' > -height/2 + 40
    --       then do
    --         return (game' { player2 = player2 game' - 10 }, p1, p2)
    --     else return (game', p1, p2)
    -- else if S.member (Char 'q') (keys game)
    --   then do
    --     exitSuccess
    -- else return (game', p1, p2)


    -- if S.member (SpecialKey KeyUp) (keys game)
    --   then do
    --     let game' = paddleBounce . wallBounce . moveBall seconds $ game
    --     if player1 game' < height/2 - 40
    --       then do
    --         return game' { player1 = player1 game' + 10 }
    --     else return game'
    -- else if S.member (SpecialKey KeyDown) (keys game)
    --   then do
    --     let game' = paddleBounce . wallBounce . moveBall seconds $ game
    --     if player1 game' > -height/2 + 40
    --       then do
    --         return game' { player1 = player1 game' - 10 }
    --     else return game'
    -- else if S.member (Char 'e') (keys game)
    --   then do
    --     let game' = paddleBounce . wallBounce . moveBall seconds $ game
    --     if player2 game' < height/2 - 40
    --       then do
    --         return game' { player2 = player2 game' + 10 }
    --     else return game'
    -- else if S.member (Char 'd') (keys game)
    --   then do
    --     let game' = paddleBounce . wallBounce . moveBall seconds $ game
    --     if player2 game' > -height/2 + 40
    --       then do
    --         return game' { player2 = player2 game' - 10 }
    --     else return game'
    -- else if S.member (Char 'q') (keys game)
    --   then do
    --     exitSuccess
    -- else return game'
  

  where
    game' = paddleBounce . wallBounce . moveBall seconds $ game

-- | Check if a game has ended.
gameEnded :: PongGame -> Bool
gameEnded game = farLeft || farRight
  where
    (x, _) = ballLoc game
    farLeft = x < -fromIntegral width / 2 + 2 * ballRadius
    farRight = x > fromIntegral width / 2 - 2 * ballRadius

-- | Respond to key events.
handleKeys :: Event -> State -> IO State
handleKeys (EventKey k Down _ _) (game, p1, p2) = return $
 (game { keys = S.insert k (keys game)}, p1, p2)
handleKeys (EventKey k Up _ _) (game, p1, p2) = return $
 (game { keys = S.delete k (keys game)}, p1, p2)
handleKeys _ (game, p1, p2) = return (game, p1, p2)
-- handleKeys event game = case event of
--   EventKey (Char 'q') _ _ _ -> exitSuccess
--   EventKey (Char 'e') _ _ _ -> return $
--     game { player2 = player2 game + 10 }
--   EventKey (Char 'd') _ _ _ ->  return $
--     game { player2 = player2 game - 10 }
--   EventKey (SpecialKey KeyUp) _ _ _ ->  return $
--     game { player1 = player1 game + 10 }
--   EventKey (SpecialKey KeyDown) _ _ _ ->  return $
--     game { player1 = player1 game - 10 }
--   _ -> return game

handleKeys2 :: Event -> State -> IO State
handleKeys2 (EventKey (SpecialKey KeyUp) _ _ _) (game, p1, p2) = do
  forkIO $ movePlayer p1 10
  return (game, p1, p2)
handleKeys2 (EventKey (Char 'e') _ _ _) (game, p1, p2) = do
  forkIO $ movePlayer p2 10
  return (game, p1, p2)
handleKeys2 _ state = return state

-- | Given position and radius of the ball, return whether a collision occurred.
wallCollision :: Position -> Radius -> Bool
wallCollision (_, y) radius = topCollision || bottomCollision
  where
    topCollision    = y - radius <= -fromIntegral height / 2
    bottomCollision = y + radius >=  fromIntegral height / 2

wallBounce :: PongGame -> PongGame
wallBounce game = game { ballVel = (vx, vy') }
  where
    -- The old velocities.
    (vx, vy) = ballVel game

    vy' = if wallCollision (ballLoc game) ballRadius
          then 
             -- Update the velocity.
             -vy
           else
            -- Do nothing. Return the old velocity.
            vy

paddleCollision :: Position -> PongGame -> Bool
paddleCollision (x, y) game = 
  (x + ballRadius > 700 && abs (y - player1 game) < 40) || 
  (x - ballRadius < -700 && abs (y - player2 game) < 40)

paddleBounce :: PongGame -> PongGame
paddleBounce game = game { ballVel = (vx', vy) }
  where
    -- The old velocities.
    (vx, vy) = ballVel game
    vx' = if paddleCollision (ballLoc game) game then -vx else vx

-- | Update the ball position using its current velocity.
moveBall :: Float    -- ^ The number of seconds since last update
         -> PongGame -- ^ The initial game state
         -> PongGame -- ^ A new game state with an updated ball position
moveBall seconds game = game { ballLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = ballLoc game
    (vx, vy) = ballVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y + vy * seconds

-- https://stackoverflow.com/questions/52871673/haskell-gloss-do-something-every-frame-key-is-pressed Link to make movement better