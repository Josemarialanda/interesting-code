{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

module OneOfN.OneOfN
  ( OneOf1 (..),
    foldOneOf1,
    OneOf2 (..),
    foldOneOf2,
    OneOf3 (..),
    foldOneOf3,
    OneOf4 (..),
    foldOneOf4,
    OneOf5 (..),
    foldOneOf5,
    OneOf6 (..),
    foldOneOf6,
    OneOf7 (..),
    foldOneOf7,
    OneOf8 (..),
    foldOneOf8,
    OneOf9 (..),
    foldOneOf9,
    OneOf10 (..),
    foldOneOf10,
    OneOf11 (..),
    foldOneOf11,
    OneOf12 (..),
    foldOneOf12,
    OneOf13 (..),
    foldOneOf13,
    OneOf14 (..),
    foldOneOf14,
    OneOf15 (..),
    foldOneOf15,
    OneOf16 (..),
    foldOneOf16,
    OneOf17 (..),
    foldOneOf17,
    OneOf18 (..),
    foldOneOf18,
    OneOf19 (..),
    foldOneOf19,
    OneOf20 (..),
    foldOneOf20,
  )
where

import OneOfN.TH (genOneOfN)

$(genOneOfN)