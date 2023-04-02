{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

module OneOfNGADTS where

import Utils        (Every)
import Data.Kind    (Constraint, Type)
import GHC.TypeLits (ErrorMessage(..), TypeError)

-- | The 'OneOf' type is effectively a generalised @Either@, in the sense that
-- @Either a b@ is isomorphic to @OneOf '[a, b]@. 'OneOf', however, will hold
-- one of any number of greater-than-zero possible types.
data OneOf (xs :: [Type]) where
  Here  :: x -> OneOf (x ': xs)
  There :: OneOf xs -> OneOf (y ': xs)

instance Every xs Show => Show (OneOf xs) where

  -- | We can @Show@ a @OneOf xs@ as long as @Every@ member of @xs@ can @Show@.
  -- What @Every@ does is bring evidence for this into scope for all the types.

  show (Here  x) = "Here "   <> show x
  show (There x) = "There (" <> show x <> ")"

instance Every xs Eq => Eq (OneOf xs) where

  -- | We can @Eq@ a @OneOf xs@ in the same way as we can @Show@: if all things
  -- are @Eq@, then the 'OneOf' is @Eq@.

  Here  x == Here  y = x == y
  There x == There y = x == y
  _       == _       = False

instance (Every xs Eq, Every xs Ord) => Ord (OneOf xs) where

  -- | We can have @Ord@ if all of our 'OneOf' members are @Ord@. Interesting
  -- quirk of GHC here: we can't magically get @Every xs Eq@ given @Every xs
  -- Ord@ here (maybe this is that entailment thing Kmett talks about?)
  -- because, while @Ord@ might imply @Eq@, @Every xs Ord@ is a very different
  -- constraint.

  Here  _ <= There _ = True
  There _ <= Here  _ = False

  Here  x <= Here  y = x <= y
  There x <= There y = x <= y

instance Every xs Semigroup => Semigroup (OneOf xs) where

  -- | The @Semigroup@ instance is slightly different to that of @Either@: if
  -- we were using @OneOf '[a, b]@ as an analogue to @Either a b@, the
  -- difference in behaviour is that we're requiring @Semigroup@ of the @a@,
  -- and will @mappend@ two @a@ values if we're given them. In the case of
  -- differing values, we'll prefer the one latest in the list.

  Here  x <> Here  y = Here  (x <> y)
  There x <> Here  _ = There  x
  Here  _ <> There y = There       y
  There x <> There y = There (x <> y)

-- | Really underwhelming class: assuming that @x@ is an element of @xs@, we
-- can decorate @x@ in constructors to build a @OneOf xs@ mechanically.
class Inject (x :: Type) (xs :: [Type]) where

  -- Lift a type into a 'OneOf'.
  inject :: x -> OneOf xs


-- | This class is identical to @Inject@, except for the @initial@ parameter,
-- which is a copy of whatever the original list was. Keeping this around means
-- that we can produce some much nicer type errors.
class InjectLoop x xs initial where

  inject' :: x -> OneOf xs


-- | This is our @go@-function-style instance. Any time someone calls @inject@
-- from the @Inject@ typeclass, we'll just call @inject'@ and hold a copy of
-- the list at this point for errors.
instance InjectLoop x xs xs => Inject x xs where

  inject = inject' @_ @_ @xs


-- | If the head of our 'OneOf' list is the same type as the value we're
-- looking to inject, we just wrap our value in a @Here@ and we're done.
instance InjectLoop x (x ': xs) initial where

  inject' = Here


-- | If we're not so lucky, and the heads don't match, we'll @inject@ the value
-- into a @OneOf tail@, and then wrap that in a @There@.
instance {-# OVERLAPPABLE #-} InjectLoop x xs initial
    => InjectLoop x (y ': xs) initial where
  inject' = There . inject' @_ @_ @initial


-- | Conveniently, we know that anyone who manages to match this instance
-- /must/ have gone wrong. We can't construct a 'OneOf' with an empty list, so
-- the only reason we could ever match this instance is if we've failed to find
-- the type we're injecting inside the 'OneOf''s type list. Now that we're
-- carrying @initial@, we can show a pretty decent type error!
instance TypeError
      ( 'Text "You can't lift "  ':<>: 'ShowType x
  ':<>: 'Text " into "           ':<>: 'ShowType (OneOf initial)

  ':$$: 'Text "This is because " ':<>: 'ShowType x
  ':<>: 'Text " is not one of these types!"
      )
    =>  InjectLoop x '[] initial where
  inject' = undefined

-- | Interpret a @OneOf xs@ value, using knowledge that every member of @xs@
-- satisfies the given constraint, @c@.
class Interpret (c :: Type -> Constraint) (xs :: [Type]) where

  -- | The usual rank-2 trick: "give me a function that relies on @c@'s
  -- interface to produce some value, and I'll produce that value".
  interpret :: (forall x. c x => x -> r) -> OneOf xs -> r


instance c x => Interpret c '[x] where

  -- | Interpret a singleton list. Nothing especially clever: we know by
  -- construction that we must be looking at the type that is actually inside
  -- the 'OneOf', so we can just call the function on it.
  interpret f (Here  x) = f x
  interpret _ (There _) = error "Impossible"


instance (tail ~ (x' ': xs), Interpret c tail, c x)
    => Interpret c (x ': x' ': xs) where

  -- | For a non-singleton list (empty lists are impossible within 'OneOf'), we
  -- pattern match on whether we've found it or not. If we have, we do as we
  -- did above with the singleton. If we don't, we recurse deeper into the
  -- 'OneOf'.
  --
  -- Note that we can avoid an overlapping instance by pattern-matching @tail@
  -- a @Cons@ deeper.
  interpret f (Here x ) = f x
  interpret f (There x) = interpret @c f x

-- | A @OneOf '[x, y]@ can be folded with @(x -> r) -> (y -> r) -> r@ – we just
-- take a function for each possible value, and then apply the one that is
-- relevant. This type family produces the signature, given the list.
type family FoldSignature (xs :: [Type]) r where

  -- More recursion: take the head, add its fold, recurse.
  FoldSignature (x ': xs) r = (x -> r) -> FoldSignature xs r

  -- If we have no inputs, here's our output!
  FoldSignature '[] r = r


-- | This type class builds the fold for a given 'OneOf' value. Once that has
-- happened, usage is straightforward:
--
-- >>> import OneOf
-- >>> :set -XLambdaCase -XDataKinds
-- >>> :{
-- fold (inject True :: OneOf '[Int, String, Bool])
--   (\_ -> "Int!")
--   (\case
--       "hello" -> "String!"
--       _       -> "Rude string :(")
--   (\x -> if x then "YAY" else "BOO")
-- :}
-- "YAY"
--
-- We can now fold out of our datatype just as we would with @either@.
class BuildFold xs result where

  -- | Fold a 'OneOf' value by providing a function for each possible type.
  --
  -- >>> :set -XDataKinds
  -- >>> :t fold (undefined :: OneOf '[a, b])
  -- fold (undefined :: OneOf '[a, b])
  --   :: (a -> result) -> (b -> result) -> result
  fold :: OneOf xs -> FoldSignature xs result


instance BuildFold '[x] result where

  -- | For a singleton list (again, empties are impossible), we needn't do
  -- anything too clever: we just apply the function to the head.
  fold (Here  x) f = f x
  fold (There _) _ = error "Impossible"


instance (tail ~ (x' ': xs), BuildFold tail result, Ignore tail result)
    => BuildFold (x ': x' ': xs) result where

  -- | Things get more tricky for a non-singleton list if you find a match; how
  -- do you pass it over all the arguments in your fold?
  --
  -- Again, we avoid an overlapping instance by matching @tail@ as a @Cons@.
  fold (There x) _ = fold @_ @result x
  fold (Here  x) f = ignore @tail (f x)

---

class Ignore (args :: [Type]) result where

  -- | @Ignore@ is a class whose only purpose is to generate n-ary @const@
  -- functions. Give it a result, and it'll figure out the type of a function
  -- that ignores all its arguments.
  ignore :: result -> FoldSignature args result


instance Ignore '[] result where

  -- | An empty list of arguments means we're ready to return the result!
  ignore result = result


instance Ignore xs result => Ignore (x ': xs) result where

  -- | Provided I can ignore the tail, I can remove the whole thing. How? I
  -- just insert another argument and ignore that as well!
  ignore result _ = ignore @xs result