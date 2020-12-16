
module History (History (..), singletonHistory, addToHistory, backInTime) where

import Graph (NID)
import MyPrelude

-- History [2, 3] 0 [4, 5] represents a chronology 3, 2, 0 (present), 4, 5
-- The past is stored in inverted order because that way inserting/reverting is
-- efficient as the history grows very large. This is a representation like a
-- zipper.
data History = History
  { _past :: [NID],
    _now :: NID,
    _future :: [NID]
  }
  deriving (Show, Eq, Ord)

singletonHistory :: NID -> History
singletonHistory node = History [] node []

addToHistory :: NID -> History -> History
addToHistory n (History past now []) = History (now : past) n []
addToHistory n (History past now (next : farFuture))
  -- preserve future when replaying it if possible
  | n == next = History (now : past) next farFuture
  | otherwise = History (now : past) n []

goForward :: History -> History
goForward (History past now []) = History past now []
goForward (History past now (next : future)) = History (now : past) next future

goBack :: History -> History
goBack (History [] now future) = History [] now future
goBack (History (prev : past) now future) = History past prev (now : future)

pickNow :: History -> (NID, History)
pickNow history = (_now history, history)

backInTime :: Int -> History -> (NID, History)
backInTime howFar history
  | howFar >= 0 = pickNow $ applyN howFar goBack history
  | howFar < 0 = pickNow $ applyN (negate howFar) goForward history
  | otherwise = error "unreachable: exhaustive guards"
