module Editor where

import UI.HSCurses.Curses
import Zipper

type Buf = Zipper Char

render :: Buf -> String
render Empty = ""
render (Zipper t ls rs) = (reverse ls) ++ (t:rs)

mkBuf :: String -> Buf
mkBuf = mkZipper

-- | move once
nextChar, prevChar :: Buf -> Buf
nextChar = right
prevChar = left

insertChar :: Char -> Buf -> Buf
insertChar = insert

deleteChar :: Buf -> Buf
deleteChar = delete

-- | find character
charForward, charBackward :: Char -> Buf -> Buf
charForward c = rightUntil (== c)
charBackward c = leftUntil (== c)

-- | UI
draw :: Buf -> IO ()
draw buf = do
  wclear stdScr
  wAddStr stdScr (render buf)
  move 0 (col buf)
  refresh

main :: IO ()
main = do initCurses
          echo False
          normalLoop Empty
          endWin

col :: Buf -> Int
col Empty = 0
col (Zipper _ ls _) = length ls

normalLoop, insertLoop :: Buf -> IO Buf

normalLoop buf = do
  draw buf
  (KeyChar c) <- getCh
  runCmd c buf

-- | Command definitions
runCmd :: Char -> Buf -> IO Buf
runCmd 'h' = normalLoop . prevChar
runCmd 'l' = normalLoop . nextChar
runCmd 'x' = normalLoop . deleteChar
runCmd 'i' = insertLoop . prevChar
runCmd _ = return -- quit

insertLoop buf = do
  draw buf
  -- when buffer not empty and insert mode
  -- render cursor to right of actual focus
  case buf of Empty -> return ()
              _ -> move 0 (col buf + 1) >> refresh
  (KeyChar c) <- getCh
  case c of '\ESC' -> normalLoop buf
            _ -> insertLoop . insertChar c $ buf
