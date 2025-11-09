{-# LANGUAGE OverloadedLists #-}

module Utils.Base62 (isBase62Char, base62Chars) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import MyPrelude

isBase62Char :: Char -> Bool
isBase62Char c =   isDigit c   ||   isAsciiUpper c   ||   isAsciiLower c

base62Chars :: Vector Char
base62Chars =    ['0' .. '9']    <>    ['A' .. 'Z']    <>    ['a' .. 'z']
