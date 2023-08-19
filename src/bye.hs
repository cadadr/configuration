-- bye.hs --- suspend/shutdown-via-dbus helper using rofi

-- This reimplements a script I first wrote in Python, for speed
-- purposes.

module Main where

import Data.List (lookup)
import System.Exit (ExitCode(..), exitSuccess, exitWith)
import System.Environment (getProgName)
import System.IO (hPutStrLn, stderr)
import System.Process (readProcessWithExitCode)
import qualified Data.Text as DT

-- From: https://stackoverflow.com/a/44605076
exitWithErrorMessage :: String -> ExitCode -> IO a
exitWithErrorMessage errStr e = do
  progName <- getProgName
  let outStr = progName ++ ": " ++ errStr
  hPutStrLn stderr outStr
  exitWith e

commands =
  [
    ("suspend",   makeDbusCmd "Suspend")
  , ("sleep",     makeDbusCmd "Suspend")
  , ("hibernate", makeDbusCmd "Hibernate")
  , ("reboot",    makeDbusCmd "Reboot")
  , ("shutdown",  makeDbusCmd "PowerOff")
  , ("power off", makeDbusCmd "PowerOff")
  ]
  where
    dbusCmdPrefix =
      [ "dbus-send", "--system", "--print-reply"
      , "--dest=org.freedesktop.login1" , "/org/freedesktop/login1"
      ]
    makeDbusCmd c = dbusCmdPrefix ++ [interfaceOf c, "boolean:true"]
    interfaceOf = ("org.freedesktop.login1.Manager." ++)

runAction action = do
  case commandFor action of
    Just (executable:args) -> do
      (exitCode, _, _) <- readProcessWithExitCode executable args ""
      return $ case exitCode of
        ExitSuccess   -> Nothing
        ExitFailure e ->
          Just $ "dbus exited with non-zero exit status: " ++ (show e)
    Nothing -> return $ Just $ "unsupported command: " ++ action
  where
    commandFor action = lookup action commands

prompt = do
  (exitCode, stdout, _) <- readProcessWithExitCode "dmenu" args input
  return $ case exitCode of
    ExitSuccess   -> Right $ strip stdout
    ExitFailure e -> Left $
      "dmenu/rofi exited with non-zero exit status: " ++ (show e)
  where
    args = ["-p", "Select how to shut down or pause computer"]
    input = unlines commandKeys
    commandKeys = map fst commands
    strip = DT.unpack . DT.strip . DT.pack

main = do
  maybeAction <- prompt
  case maybeAction of
    Right action -> do
      result <- runAction action
      case result of
        Nothing -> exitSuccess
        Just errorMsg -> exitWithErrorMessage errorMsg (ExitFailure 1)
    Left errorMsg -> exitWithErrorMessage errorMsg (ExitFailure 1)
