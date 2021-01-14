module IO where

type IO a = RealWorld -> (ReadWorld, a)

return :: a -> RealWorld -> (ReadWorld, a)
(>>=) :: (RealWorld -> (ReadWorld, a))
  -> (a -> RealWorld -> (ReadWorld, b))
  -> RealWorld -> (ReadWorld, b)

instance Monad IO where
  return a = \w -> (w,a)
  (>>=) m k = \w -> case m w of (w', a) -> k a w'