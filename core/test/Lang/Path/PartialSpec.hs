module Lang.Path.PartialSpec where

import Lang.Path.Partial
import TestPrelude

test_takeRelevantFromEnd :: TestTree
test_takeRelevantFromEnd =
  testGroup
    "takeRelevantFromEnd"
    [ takeRelevantFromEnd' "a/(b + c" "a/ c",
      takeRelevantFromEnd' "a/(b + c/d" "a/ c/d",
      takeRelevantFromEnd' "(a + b)/e + (c + d)" " (c + d)",
      takeRelevantFromEnd' "(a + b)/(e + (c + d)" "(a + b)/ (c + d)",
      takeRelevantFromEnd' "a & b/" " b/",
      takeRelevantFromEnd' "a & b/(a + b & c/(d + e" " b/ c/ e"
    ]
  where
    takeRelevantFromEnd' i e = testCase i $ e @=? (takeRelevantFromEnd . reverse $ i)
