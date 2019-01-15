module Chapter4 where

safeRoot :: Double -> Maybe Double
safeRoot x =
  if x >= 0
    then Just $ sqrt x
    else Nothing

safeReciprocal :: Double -> Maybe Double
safeReciprocal x =
  if x /= 0
    then Just $ 1 / x
    else Nothing

compose :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
compose f g =
  \x ->
    case f x of
      Just y -> g y
      Nothing -> Nothing

safeRootReciprocal :: Double -> Maybe Double
safeRootReciprocal = compose safeRoot safeReciprocal

compose2 :: (a -> Maybe a) -> (a -> Maybe a) -> (a -> Maybe a)
compose2 f g =
  \x -> do
    y <- f x
    r <- g y
    return r

safeRootReciprocal2 :: Double -> Maybe Double
safeRootReciprocal2 = compose2 safeRoot safeReciprocal

identity :: a -> Maybe a
identity = Just

idTest :: Double -> Bool
idTest x =
  let id1 = compose safeRoot identity
      id2 = compose identity safeRoot
   in id1 x == id2 x && id1 x == safeRoot x
