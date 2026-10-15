module Models.HistorySpec where

import Models.History
import Models.NID
import Models.PathPattern.WalkSpec (withinAllocation)
import MyPrelude
import Utils.Testing

history :: [Int] -> Int -> [Int] -> History
history past now future =
  History (smallNID <$> past) (smallNID now) (smallNID <$> future)

test_pushHistory :: TestTree
test_pushHistory =
  testGroup
    "add 0 to history"
    [ ath 0 (history [1, 0] 2 []) (history [2, 1, 0] 0 []),
      ath 0 (history [1, 0] 2 [0]) (history [2, 1, 0] 0 []),
      ath 0 (history [1, 0] 2 [0, 4]) (history [2, 1, 0] 0 [4]),
      ath 0 (history [1, 0] 2 [3]) (history [2, 1, 0] 0 [])
    ]
  where
    ath n h expected =
      testCase name $ pushHistory (smallNID n) h @?= expected
      where
        name = show h

test_backInTime :: TestTree
test_backInTime =
  testGroup
    "backInTime"
    [ bt 0 (history [] 0 []) (0, history [] 0 []),
      bt 2 (history [] 0 []) (0, history [] 0 []),
      bt (-2) (history [] 0 []) (0, history [] 0 []),
      bt 2 (history [1, 0] 2 []) (0, history [] 0 [1, 2]),
      bt 2 (history [2, 1, 0] 3 []) (1, history [0] 1 [2, 3]),
      bt (-1) (history [] 0 [1, 2]) (1, history [0] 1 [2]),
      -- beyond either end: the node at that end
      bt 7 (history [2, 1, 0] 3 [4]) (0, history [] 0 [1, 2, 3, 4]),
      bt maxBound (history [2, 1, 0] 3 [4]) (0, history [] 0 [1, 2, 3, 4]),
      bt (-7) (history [1, 0] 2 [3, 4]) (4, history [3, 2, 1, 0] 4 []),
      bt minBound (history [1, 0] 2 [3, 4]) (4, history [3, 2, 1, 0] 4 []),
      bt minBound (history [1, 0] 2 []) (2, history [1, 0] 2 []),
      bt 100000000 (history [0] 1 []) (0, history [] 0 [1]),
      bt (-100000000) (history [] 0 [1]) (1, history [0] 1 [])
    ]
  where
    -- the work is bounded by the history, not by n: a count of work
    -- (allocation), so a step per count fails here instead of hanging
    bt n h expected =
      testCase name do
        _ <- evaluate (force h)
        done <-
          withinAllocation (1024 * 1024) (evaluate (force (backInTime n h)))
        assertBool "allocated more than 1 MiB" (isJust done)
        done @?= Just (first smallNID expected)
      where
        name = show n ++ ", " ++ show h
