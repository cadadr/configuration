-- bright.hs --- set brightness

import Data.List (isPrefixOf)
import Data.Text (pack, strip, unpack)
import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.FilePath (FilePath, (</>))

dir = "/sys/class/backlight/"

curfil = "actual_brightness"
maxfil = "max_brightness"
setfil = "brightness"

backlights = do
  all <- listDirectory dir
  return $ filter (\ fp -> not $ isPrefixOf "." fp) all

get fil bl = do
  raw <- readFile $ (dir </> bl </> fil)
  return $ unpack $ strip $ pack raw

process [] bl = do
  cur <- get curfil bl
  max <- get maxfil bl
  putStrLn $ bl ++ ": " ++ cur ++ " (actual, max: " ++ max ++ ")"
process (lvl:_) bl = do
  if lvl' < 0 || lvl' > 1
    then error "out of range"
    else process' lvl' bl
  where
    lvl' = (read::String->Float) lvl

process' lvl bl = do
  max <- get maxfil bl
  let max' = (read::String->Float) max
  let newlvl = truncate $ max' * lvl
  writeFile (dir </> bl </> setfil) $ show newlvl

main = do
  bls <- backlights
  args <- getArgs
  mapM_ (process args) bls
