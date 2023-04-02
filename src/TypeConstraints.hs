{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module TypeConstraints where

import Data.Kind
  ( Constraint,
    Type,
  )
import Data.Type.Bool
  ( If,
    Not,
  )
import Data.Type.Equality (type (==))
import GHC.TypeLits
  ( ErrorMessage
      ( ShowType,
        Text,
        (:<>:)
      ),
    Symbol,
    TypeError,
  )

-- | This type family constraint that checks if a type is a member of a list of types.
type family Member (a :: k) (as :: [k]) :: Constraint where
  Member x xs = Member' x xs xs

type family Member' (a :: k) (as :: [k]) (bs :: [k]) :: Constraint where
  Member' x '[]       bs = TypeError ('Text "Type " ':<>: 'ShowType x ':<>: 'Text " is not a member of " ':<>: 'ShowType bs)
  Member' x '[y]      bs = If (x == y) (() :: Constraint) (Member' x '[] bs)
  Member' x (y ': ys) bs = If (Not (x == y)) (Member' x ys (y ': bs)) (() :: Constraint)