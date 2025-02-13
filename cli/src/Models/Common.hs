module Models.Common where

import MyPrelude

type ValidTransition t = (Show t, Eq t, Ord t)
