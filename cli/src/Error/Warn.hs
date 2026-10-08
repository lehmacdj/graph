{-# LANGUAGE TemplateHaskell #-}

module Error.Warn where

import Data.List.NonEmpty qualified as NonEmpty
import Effectful.State.Dynamic
import Error.UserError
import Models.Damage
import Models.NID
import Models.Node
import MyPrelude

data Warn e :: Effect where
  Warn :: e -> Warn e m ()

makeEffect ''Warn

convertError ::
  forall e effs. (Warn e :> effs) => Eff (Error e : effs) () -> Eff effs ()
convertError = (`handleError` warn)

warnString :: (Warn UserError :> r) => String -> Eff r ()
warnString = warn . OtherError . pack

warnText :: (Warn UserError :> r) => Text -> Eff r ()
warnText = warn . OtherError

printWarnings ::
  forall e r a.
  (IOE :> r, Show e) =>
  Eff (Warn e : r) a ->
  Eff r a
printWarnings = interpret \_ -> \case
  Warn e -> liftIO . eputStr . show $ e

ignoreWarnings :: forall e r a. Eff (Warn e : r) a -> Eff r a
ignoreWarnings = interpret \_ -> \case Warn _ -> pure ()

-- | The damage that was read past: files that contradict each other or can't
-- be used, where nothing was changed.
data DamageReadPast = DamageReadPast
  { -- | by the current command line, and not warned of yet (see
    -- 'warningOfDamageReadPast')
    unwarned :: Set Damage,
    -- | by the command that is running, if one is (see 'asOneDecision')
    inCommand :: Maybe ReadByCommand
  }
  deriving stock (Eq, Show, Generic)

-- | What the command that is running has read so far, in all of its steps
data ReadByCommand = ReadByCommand
  { -- | the damage that it read past
    damage :: Set Damage,
    -- | The node files that it read, each as the command last read or wrote
    -- it ('Nothing': there is no such file), by the node the file is named
    -- after. The steps of a command read the two files of an edge in
    -- different transactions as often as in one, so each transaction looks
    -- for damage among these and the files that it read itself.
    files :: Map NID (Maybe (Node Text ()))
  }
  deriving stock (Eq, Show, Generic)

-- | No damage was read past, and no command is running.
noDamageReadPast :: DamageReadPast
noDamageReadPast = DamageReadPast mempty Nothing

-- | Note damage that something read past.
noteDamageReadPast :: (State DamageReadPast :> r) => Set Damage -> Eff r ()
noteDamageReadPast damage =
  modify @DamageReadPast $
    (#unwarned %~ (<> damage)) . (#inCommand . _Just . #damage %~ (<> damage))

-- | Run a command as one decision: whatever it reads, in however many steps
-- (the targets of @at@, the nodes of a @%targets(…)@, the arguments of @mg@),
-- it decided what to write by. So once one of its steps has read past damage,
-- no later step of it writes ('damageReadPastByCommand'): the change would be
-- one that was chosen by what the damaged files seemed to say. And the node
-- files that its steps read are compared with each other as those of one
-- step are ('filesReadByCommand').
--
-- What runs inside of a command is part of that command's decision, so this
-- does nothing there. What runs outside of every command (opening the graph)
-- decides nothing for the commands, and nothing is kept from one command to
-- the next.
asOneDecision ::
  (Members [State DamageReadPast, IOE] r) => Eff r a -> Eff r a
asOneDecision command =
  gets @DamageReadPast (.inCommand) >>= \case
    Just _ -> command
    Nothing -> do
      modify @DamageReadPast (#inCommand ?~ ReadByCommand mempty mempty)
      command `finally` modify @DamageReadPast (#inCommand .~ Nothing)

-- | The damage that the command that is running has read past so far.
damageReadPastByCommand :: (State DamageReadPast :> r) => Eff r (Set Damage)
damageReadPastByCommand =
  gets @DamageReadPast (foldMap (.damage) . (.inCommand))

-- | The node files that the command that is running has read so far, as it
-- last read or wrote each. None outside of a command.
filesReadByCommand ::
  (State DamageReadPast :> r) => Eff r (Map NID (Maybe (Node Text ())))
filesReadByCommand = gets @DamageReadPast (foldMap (.files) . (.inCommand))

-- | Say which node files the command that is running has read so far, and
-- what it last read or wrote of each. Nothing is kept outside of a command.
setFilesReadByCommand ::
  (State DamageReadPast :> r) => Map NID (Maybe (Node Text ())) -> Eff r ()
setFilesReadByCommand files =
  modify @DamageReadPast (#inCommand . _Just . #files .~ files)

-- | Start the decision of the command that is running afresh: it forgets the
-- node files that it has read and the damage that it has read past, and its
-- later steps are judged by what they read from here on. For @fix@, which
-- writes node files outside of every transaction: neither those files nor
-- that damage are as the command read them. (What it read past is still
-- warned of when the line is done, as it is when @fix@ comes after the
-- command in a line.) Does nothing outside of a command.
startDecisionAfresh :: (State DamageReadPast :> r) => Eff r ()
startDecisionAfresh =
  modify @DamageReadPast (#inCommand . _Just .~ ReadByCommand mempty mempty)

-- | Run a command line, or what else is one thing to whoever runs @ge@, and
-- warn of the damage that it read past: once for each piece of it, however
-- often it was read, and only when the line is done, however it ends (an
-- error, any other exception such as that of an editor that failed, an
-- interrupt). If it failed because of some of that damage, the error says
-- so, and that damage is not warned of as well.
--
-- Nothing else warns of such damage, so what reads the graph outside of a
-- command line (completion, which would write into the line that the user is
-- editing) is silent about it.
warningOfDamageReadPast ::
  (Members [Warn UserError, State DamageReadPast, Error UserError, IOE] r) =>
  Eff r a ->
  Eff r a
warningOfDamageReadPast line = do
  result <- tryError @UserError line `onException` warnExcept mempty
  warnExcept (either (damageRefusedBy . snd) (const mempty) result)
  either (throwError . snd) pure result
  where
    warnExcept refused = do
      damage <- state @DamageReadPast \s -> (s.unwarned, s & #unwarned .~ mempty)
      for_ (NonEmpty.nonEmpty (toList (damage `difference` refused))) $
        warn . DamagedGraphWarning
