module Example.Fibonacci where

import Clash.Prelude

fib :: Int -> Int
fib n = fib (n - 1) + fib (n - 2)

fibAcc :: Int -> Int
fibAcc = fst . go (0, 1)
  where
    go (a1, a2) 0 = (a1, a2)
    go (a1, a2) n = go (a2, a1 + a2) (n - 1)

recursionFib
  :: HiddenClockResetEnable dom
  => Signal dom (Unsigned 32, Unsigned 32)
recursionFib = bundle (a1, a2)
  where
    a1 = register 0 a2
    a2 = register 1 (liftA2 (+) a1 a2)

recurrenceFib
  :: HiddenClockResetEnable dom
  => Signal dom (Unsigned 32, Unsigned 32)
recurrenceFib = seqLogic
  where
    fibStep (n, m) = (m, n + m)
    seqLogic = register (0, 1) (fibStep <$> seqLogic)

mealyFib :: HiddenClockResetEnable dom => Signal dom (Unsigned 32, Unsigned 32)
mealyFib = mealy step (0, 1) (pure ())
  where
    step (n, m) () = let p = (m, n + m) in (p, p)




mac :: Num a => a -> a -> a -> a
mac acc x y = acc + x * y

macSeq :: (HiddenClockResetEnable dom, Num a, NFDataX a)
       => Signal dom a -> Signal dom a -> Signal dom a
macSeq x y = acc
  where
    acc = register 0 (mac <$> acc <*> x <*> y)
