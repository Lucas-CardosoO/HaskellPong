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
type Player1Move = MVar Float
type Player2Move = MVar Float
type BotPos = MVar Float
type BallYPos = MVar Float
type ScorePlayer = MVar Int
type ScoreBot = MVar Int

-- | Data describing the state of the pong game.
data PongGame = Game
  { ballLoc :: Position        -- ^ Pong ball (x, y) location.
  , ballVel :: (Float, Float)  -- ^ Pong ball (x, y) velocity.
  , player1 :: Float          -- ^ Right player paddle height.
                               -- Zero is the middle of the screen.
  , player2 :: Float           -- ^ Left player paddle height.
  , keys :: S.Set Key         -- Set of keys that makes movement continuous
  , scorePlayer1 :: Int       -- Score for player
  , scoreBot :: Int           -- Score for bot
  , difficulty :: Char        -- Difficulty of game
  } deriving Show

type State = (PongGame, Player1Move, BotPos, BallYPos, ScorePlayer, ScoreBot)

ballRadius :: Radius
ballRadius = 10

moveBot :: BotPos -> BallYPos -> Float -> IO()
moveBot botPos ballY move = do
  bot <- readMVar botPos
  ball <- readMVar ballY
  if ball > (bot + 20)
    then do
      bot2 <- takeMVar botPos
      putMVar botPos (bot2 + move)
      threadDelay 60000--16667
      moveBot botPos ballY move
  else if ball < (bot - 20)
    then do
      bot2 <- takeMVar botPos
      putMVar botPos (bot2 - move)
      threadDelay 60000--16667
      moveBot botPos ballY move
  else do
    threadDelay 80000
    moveBot botPos ballY move

valueToMove :: Integer -> Float
valueToMove x
  | x < 1 = - 10
  | x > 1 = 10
  | otherwise = 0

-- changeGoal :: MVar Int -> IO()
-- changeGoal = do
--   if

randomInitialState :: StdGen -> Char -> PongGame
randomInitialState gen dif = Game
  { ballLoc = (a, b)
  , ballVel = (c', d')
  , player1 = 0
  , player2 = 0
  , keys = S.empty
  , scorePlayer1 = 0
  , scoreBot = 0
  , difficulty = dif
  }
  where
    magnitude = difficultyFunc dif
    a:b:c:d:_ = randomRs (-50, 50) gen
    c' = c * mag
    d' = d * mag
    mag = magnitude / sqrt (c^2 + d^2)

randomGoalState  :: StdGen -> PongGame -> PongGame
randomGoalState gen game = Game
  { ballLoc = (a, b)
  , ballVel = (c', d')
  , player1 = player1 game
  , player2 = player2 game
  , keys = keys game
  , scorePlayer1 = scorePlayer1 game
  , scoreBot = scoreBot game
  , difficulty = difficulty game
  }
  where
    magnitude = difficultyFunc $ difficulty game
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
    'm' -> 400
    'h' -> 600
    _ -> 400

difficultyFuncMovement :: Char -> Float
difficultyFuncMovement c = 
  case c of
    'e' -> 10
    'm' -> 20
    'h' -> 30
    _ -> 20

main :: IO ()
main = do
  gen <- getStdGen
  print "Write [e] for easy, [m] for medium or [h] for hard" 
  difficulty <- getChar

  let initState = randomInitialState gen difficulty
  p1 <- newMVar 0.0
  p2 <- newMVar 0.0
  ball <- newMVar 0.0
  botPoints <- newMVar 0
  playerPoints <- newMVar 0
  forkIO $ (moveBot p2 ball (difficultyFuncMovement difficulty))

  playIO window background fps (initState, p1, p2, ball, playerPoints, botPoints) render handleKeys update

render :: State -> IO Picture
render (game, _, _, _, _, _) = do
  -- p1 <- readMVar scorePlayer
  -- bot <- readMVar scoreBot
  if gameEnded game
    then do
      if scorePlayer1 game > scoreBot game
        then return $ pictures [color white $ translate (-650) (200) $ scale (0.8) (0.8) $ (Text "Congratulations! You Won!"), color white $ translate (-350) (0) $ scale (0.6) (0.6) $ (Text "Press [q] to quit")]
      else return $ pictures [color white $ translate (-400) (200) $ scale (0.8) (0.8) $ (Text "Sorry, you Lost"), color white $ translate (-350) (0) $ scale (0.6) (0.6) $ (Text "Press [q] to quit")]
  else return $ pictures [ball, walls, mkPaddle white 700 $ player1 game, mkPaddle white (-700) $ player2 game, color white $ translate (0) (240) $ scale (0.8) (0.8) $ (Text "-"), renderScore (scorePlayer1 game) 200, renderScore (scoreBot game) (-200)]
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
      [ translate x y $ color col $ rectangleSolid 20 80
      , translate x y $ color paddleColor $ rectangleSolid 20 80
      ]

    paddleColor = white

renderScore :: Int -> Float -> Picture
renderScore score pos = color white $ translate (pos) (240) $ scale (0.8) (0.8) $ (Text (show score))

-- | Update the game by moving the ball.
update :: Float -> State -> IO State
update seconds (game, p1, p2, ball, score1, scoreBot) = 
  if gameEnded game'
  then do
    if S.member (Char 'q') (keys game)
      then do
        exitSuccess
    else return (game', p1, p2, ball, score1, scoreBot)
  else if checkForGoalBool game'
    then do
      gen <- getStdGen
      return(randomGoalState gen game', p1, p2, ball, score1, scoreBot)
  else do

    m1 <- readMVar p1
    m2 <- readMVar p2
    b <- takeMVar ball
    putMVar ball (snd (ballLoc game'))

    if S.member (SpecialKey KeyUp) (keys game)
      then do
        let game' = paddleBounce . wallBounce . moveBall seconds $ game
        if player1 game' < height/2 - 40
          then do
            return (game' { player1 = player1 game' + move, player2 = m2 }, p1, p2, ball, score1, scoreBot)
        else return (game' {player2 = m2}, p1, p2, ball, score1, scoreBot)
    else if S.member (SpecialKey KeyDown) (keys game)
      then do
        let game' = paddleBounce . wallBounce . moveBall seconds $ game
        if player1 game' > -height/2 + 40
          then do
            return (game' { player1 = player1 game' - move, player2 = m2 }, p1, p2, ball, score1, scoreBot)
        else return (game' {player2 = m2}, p1, p2, ball, score1, scoreBot)
    else if S.member (Char 'q') (keys game)
      then do
        exitSuccess
    else return (game' {player2 = m2}, p1, p2, ball, score1, scoreBot)
  

  where
    game' = checkForGoal . paddleBounce . wallBounce . moveBall seconds $ game
    move = difficultyFuncMovement (difficulty game')

-- | Check if a game has ended.
gameEnded :: PongGame -> Bool
gameEnded game = scorePlayer1 game >= 3 || scoreBot game >= 3

checkForGoal :: PongGame -> PongGame
checkForGoal game
  | (fst(ballLoc game) < -fromIntegral width / 2 + 2 * ballRadius) && not (gameEnded game) = game {scorePlayer1 = scorePlayer1 game + 1}
  | (fst(ballLoc game) > fromIntegral width / 2 - 2 * ballRadius) && not (gameEnded game) = game {scoreBot = scoreBot game + 1}
  | otherwise = game

checkForGoalBool :: PongGame -> Bool
checkForGoalBool game = fst(ballLoc game) < -fromIntegral width / 2 + 2 * ballRadius || fst(ballLoc game) > fromIntegral width / 2 - 2 * ballRadius

-- | Respond to key events.
handleKeys :: Event -> State -> IO State
handleKeys (EventKey k Down _ _) (game, p1, p2, ball, score1, scoreBot) = return $
 (game { keys = S.insert k (keys game)}, p1, p2, ball, score1, scoreBot)
handleKeys (EventKey k Up _ _) (game, p1, p2, ball, score1, scoreBot) = return $
 (game { keys = S.delete k (keys game)}, p1, p2, ball, score1, scoreBot)
handleKeys _ (game, p1, p2, ball, score1, scoreBot) = return (game, p1, p2, ball, score1, scoreBot)

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
  ((x + ballRadius + 10) > 700 && abs (y - player1 game) < 40) || 
  ((x - ballRadius - 10) < -700 && abs (y - player2 game) < 40)

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
