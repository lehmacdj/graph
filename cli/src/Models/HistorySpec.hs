module Models.HistorySpec where

import Models.History
import Models.NID
import TestPrelude

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
      bt (-1) (history [] 0 [1, 2]) (1, history [0] 1 [2])
    ]
  where
    bt n h expected =
      testCase name $ backInTime n h @?= first smallNID expected
      where
        name = show n ++ ", " ++ show h
