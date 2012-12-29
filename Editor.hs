module Editor where

import UI.HSCurses.Curses
import Zipper

type Buf = Zipper Char

data Mode = Normal | Insert

render :: Buf -> String
render Empty = "~"
render (Zipper t ls rs) = (reverse ls) ++ (t:rs)

mkBuf :: String -> Buf
mkBuf = mkZipper

-- | move once
charNext, charPrev :: Buf -> Buf
charNext = right
charPrev = left

doThenRedraw :: (Buf -> Buf) -> Buf -> IO Buf
doThenRedraw f buf = do redrawAll buf'
                        return buf'
                     where buf' = f buf

insertChar :: Char -> Buf -> IO Buf
insertChar c = doThenRedraw (insert c)

deleteChar :: Buf -> IO Buf
deleteChar = doThenRedraw delete

redrawAll :: Buf -> IO ()
redrawAll buf = do wclear stdScr
                   wAddStr stdScr (render buf)
                   redrawCursor buf

redrawCursor :: Buf -> IO ()
redrawCursor buf = do move 0 (col buf)
                      refresh

-- | find character
charForward, charBackward :: Char -> Buf -> Buf
charForward c = rightUntil (== c)
charBackward c = leftUntil (== c)

startBuf :: Buf
startBuf = mkBuf "hello world"

-- UI
main :: IO ()
main = do initCurses
          echo False
          redrawAll startBuf
          loop Normal startBuf
          endWin

col :: Buf -> Int
col Empty = 0
col (Zipper _ ls _) = length ls

loop :: Mode -> Buf -> IO Buf
loop mode buf = do
  redrawCursor buf
  (KeyChar c) <- getCh
  case mode of Normal -> handleNormalMode c buf
               Insert -> handleInsertMode c buf

handleNormalMode, handleInsertMode :: Char -> Buf -> IO Buf

handleNormalMode c buf = case c of
  'h' -> loop Normal $ charPrev buf
  'l' -> loop Normal $ charNext buf
  'x' -> deleteChar buf >>= loop Normal
  'i' -> loop Insert buf
  _ -> return buf

handleInsertMode c buf = case c of
  '\ESC' -> loop Normal buf
  _ -> insertChar c buf >>= loop Insert
