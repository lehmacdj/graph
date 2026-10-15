-- | What a program that leads its session must do with its controlling
-- terminal before it ends, so that it can end.
--
-- When the process that leads a session ends, the system first waits for
-- the session's terminal to take all the output that is queued for it,
-- without a limit of time and in a state in which no signal ends the
-- process any more (not SIGKILL either). A terminal whose output is stopped
-- (Ctrl-S), or that nobody reads, takes none. So a session leader that has
-- written to such a terminal stays, as a process that is exiting, until the
-- terminal takes output again or is closed. (Measured on macOS with a small
-- C program that is not part of the repository: it led a session on a
-- pseudo-terminal that nobody read, wrote to it, made the calls of this
-- module or not, and ended. A program that a shell starts does not lead its
-- session and is not affected.)
--
-- Nothing tells a terminal that is a moment late from one that will never
-- take the output: the system only says how much is queued. So whoever
-- wants the terminal to show the last output must wait for it
-- ('awaitTerminalOutput'), for as long as it can afford, and must then
-- discard what is left ('discardTerminalOutput'), which makes the terminal
-- drop what is written to it from then on as well.
module System.ControllingTerminal
  ( awaitTerminalOutput,
    discardTerminalOutput,
    spec_discardOutputOf,
  )
where

import Data.Bits ((.|.))
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peekElemOff, pokeElemOff)
import MyPrelude
import System.Posix.IO
  ( FdOption (NonBlockingRead),
    OpenFileFlags (noctty, nonBlock),
    OpenMode (WriteOnly),
    closeFd,
    defaultFileFlags,
    openFd,
    setFdOption,
  )
import System.Posix.IO.ByteString (fdRead, fdWrite)
import System.Posix.Process (getProcessID)
import System.Posix.Terminal
  ( QueueSelector (OutputQueue),
    discardData,
    drainOutput,
    openPseudoTerminal,
  )
import System.Posix.Types (CPid (..), CTcflag (..), Fd (..))
import Utils.Testing.External

-- | Run something with the controlling terminal of the process, if the
-- process leads its session and has one; any failure counts as having
-- none.
withOwnTerminal :: (Fd -> IO ()) -> IO ()
withOwnTerminal act = void . tryAny $ do
  pid <- getProcessID
  session <- c_getsid 0
  when (session == pid) $
    bracket
      (openFd "/dev/tty" WriteOnly defaultFileFlags {noctty = True, nonBlock = True})
      closeFd
      act

-- | Wait until the controlling terminal has taken the output that is
-- queued for it, if the process leads its session: for good if the terminal
-- takes none, so only for whoever is ended from elsewhere if this takes
-- too long.
--
-- If the wait fails, as when a signal that the process handles ends it
-- before its time, what is queued is discarded, so that in no case this
-- returns with output queued (@ge@ then ended with output queued, and
-- stayed).
awaitTerminalOutput :: IO ()
awaitTerminalOutput = withOwnTerminal \fd ->
  drainOutput fd `catch` \(_ :: IOException) -> discardOutputOf fd

-- | Discard the output that is queued for the controlling terminal, and
-- have the terminal drop whatever is written to it from now on, if the
-- process leads its session: what a thread of the process is still writing
-- (also one that waits in the system for the terminal to take it) does not
-- fill the queue again. For a program that ends right afterwards.
discardTerminalOutput :: IO ()
discardTerminalOutput = withOwnTerminal discardOutputOf

discardOutputOf :: Fd -> IO ()
discardOutputOf fd@(Fd raw) = do
  -- struct termios begins with its four flag words, the local flags last;
  -- nothing else of it is looked at
  allocaBytes 512 \attributes -> do
    let flagWords = castPtr attributes :: Ptr CTcflag
    got <- c_tcgetattr raw attributes
    when (got == 0) do
      localFlags <- peekElemOff flagWords 3
      pokeElemOff flagWords 3 (localFlags .|. c_FLUSHO)
      -- TCSANOW
      void (c_tcsetattr raw c_TCSANOW attributes)
  discardData fd OutputQueue

spec_discardOutputOf :: Spec
spec_discardOutputOf =
  it "has a terminal drop what is queued for it and what is written to it afterwards" do
    -- a thread that is still writing when the queue is discarded fills it
    -- again otherwise, and a process that leads its session cannot end
    (reader, terminal) <- openPseudoTerminal
    flip finally (closeFd reader >> closeFd terminal) do
      _ <- fdWrite terminal "queued\n"
      discardOutputOf terminal
      _ <- fdWrite terminal "afterwards\n"
      -- nothing to read: reading fails at once instead of waiting
      setFdOption reader NonBlockingRead True
      (either (const Nothing) Just <$> tryAny (fdRead reader 100))
        `shouldReturn` Nothing

foreign import ccall unsafe "getsid" c_getsid :: CPid -> IO CPid

foreign import ccall unsafe "tcgetattr"
  c_tcgetattr :: CInt -> Ptr () -> IO CInt

foreign import ccall unsafe "tcsetattr"
  c_tcsetattr :: CInt -> CInt -> Ptr () -> IO CInt

foreign import capi unsafe "termios.h value FLUSHO" c_FLUSHO :: CTcflag

foreign import capi unsafe "termios.h value TCSANOW" c_TCSANOW :: CInt
