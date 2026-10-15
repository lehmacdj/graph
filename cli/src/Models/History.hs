module Models.History
  ( History (..),
    singletonHistory,
    pushHistory,
    backInTime,
  )
where

import Models.NID (NID)
import MyPrelude

-- History [2, 3] 0 [4, 5] represents a chronology 3, 2, 0 (present), 4, 5
-- The past is stored in inverted order because that way inserting/reverting is
-- efficient as the history grows very large. This is a representation like a
-- zipper.
data History = History
  { past :: [NID],
    now :: NID,
    future :: [NID]
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData History

singletonHistory :: NID -> History
singletonHistory node = History [] node []

pushHistory :: NID -> History -> History
pushHistory n (History past now []) = History (now : past) n []
pushHistory n (History past now (next : farFuture))
  -- preserve future when replaying it if possible
  | n == next = History (now : past) next farFuture
  | otherwise = History (now : past) n []

-- | Up to this many steps forward (towards the end of the future): each
-- step moves one node, so the work is bounded by the length of the
-- future, not by the count.
goForward :: Int -> History -> History
goForward k history@(History past now future)
  | k <= 0 = history
  | next : farFuture <- future =
      goForward (k - 1) (History (now : past) next farFuture)
  | otherwise = history

-- | Up to this many steps back (towards the start of the past), bounded
-- like 'goForward'.
goBack :: Int -> History -> History
goBack k history@(History past now future)
  | k <= 0 = history
  | prev : farPast <- past =
      goBack (k - 1) (History farPast prev (now : future))
  | otherwise = history

pickNow :: History -> (NID, History)
pickNow history = (history ^. #now, history)

-- | The history @howFar@ steps back (forward when negative), and the node
-- it is then at. A count beyond either end stops at that end; the work is
-- bounded by the history, whatever the count (also 'minBound', whose
-- negation is itself).
backInTime :: Int -> History -> (NID, History)
backInTime howFar history
  | howFar >= 0 = pickNow $ goBack howFar history
  -- one step, then the rest: negating 'minBound' would overflow
  | otherwise = pickNow . goForward (negate (howFar + 1)) $ goForward 1 history
