-- | Display images in the terminal, with support for multiple
--   protocols. Uses kitty's kitten icat when available, falling
--   back to the iTerm2 inline image protocol.
module System.IO.Term.Image (printImage) where

import MyPrelude hiding (putStrLn)
import System.Directory (findExecutable)
import System.IO.Term.Image.ITerm qualified as ITerm
import System.Process.Typed

printImage :: LByteString -> IO ()
printImage d = do
  kittenAvailable <- isJust <$> findExecutable "kitten"
  if kittenAvailable
    then printImageKitten d
    else ITerm.printImage d

printImageKitten :: LByteString -> IO ()
printImageKitten d =
  runProcess_ $
    setStdin (byteStringInput d) $
      proc "kitten" ["icat", "--stdin=yes"]
