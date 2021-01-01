module HistorySpec where

import History
import TestPrelude

test_addToHistory :: TestTree
test_addToHistory =
  testGroup
    "add 0 to history"
    [ ath 0 (History [1, 0] 2 []) (History [2, 1, 0] 0 []),
      ath 0 (History [1, 0] 2 [0]) (History [2, 1, 0] 0 []),
      ath 0 (History [1, 0] 2 [0, 4]) (History [2, 1, 0] 0 [4]),
      ath 0 (History [1, 0] 2 [3]) (History [2, 1, 0] 0 [])
    ]
  where
    ath node history expected =
      testCase name $ addToHistory node history @?= expected
      where
        name = show history

test_backInTime :: TestTree
test_backInTime =
  testGroup
    "backInTime"
    [ bt 0 (History [] 0 []) (0, History [] 0 []),
      bt 2 (History [] 0 []) (0, History [] 0 []),
      bt (-2) (History [] 0 []) (0, History [] 0 []),
      bt 2 (History [1, 0] 2 []) (0, History [] 0 [1, 2]),
      bt 2 (History [2, 1, 0] 3 []) (1, History [0] 1 [2, 3]),
      bt (-1) (History [] 0 [1, 2]) (1, History [0] 1 [2])
    ]
  where
    bt n history expected =
      testCase name $ backInTime n history @?= expected
      where
        name = show n ++ ", " ++ show history
