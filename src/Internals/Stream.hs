-- Of all the modules included with this assignment
-- this is the one you need to care about the least.
-- Streams are simply lists which are always infinite
-- If you're not feeling adventurous stop here; none
-- of this is directly assessable
-- Robert 'Probie' Offner
-- Feb 2016

module Internals.Stream where

-- I intentionally don't derive Eq, Show or Ord
-- since they might diverge
infixr 5 :<
data Stream a = a :< Stream a

-- Turn a list (finite or infinite) by repeating it
-- (i.e repeat_list [1,2] = 1 :< 2 :< 1 :< 2 :< 1 :< ...)
repeatList :: [a] -> Stream a
repeatList list = result
  where result = foldr (:<) result list

streamRepeat :: a -> Stream a
streamRepeat x = result
  where result = x :< result

nats :: Stream Integer
nats = nats' 0
  where nats' n = n :< nats' (n+1)

positives :: Stream Integer
positives = streamTail nats

negatives :: Stream Integer
negatives = fmap negate positives

streamTail :: Stream a -> Stream a
streamTail (_:<xs) = xs

streamZip :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZip f (x:<xs) (y:<ys) = f x y :< streamZip f xs ys

takeWhileS :: (a -> Bool) -> Stream a -> [a]
takeWhileS p (x :< xs) | p x = x : takeWhileS p xs
                       | otherwise = []

instance Functor Stream where
  fmap f (x :< xs) = f x :< fmap f xs
