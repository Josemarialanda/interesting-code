{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

module VOneOfN where

-- This GADT to represent a variable list of types.
data VOneOf as where
  This :: a -> VOneOf (a : as)
  Next :: VOneOf as -> VOneOf (a : as)

-- This GADT to represent a variable list of types applied to some structure f.
data VOneOfF f as where
  ThisF :: f a -> VOneOfF f (a : as)
  NextF :: VOneOfF f as -> VOneOfF f (a : as)