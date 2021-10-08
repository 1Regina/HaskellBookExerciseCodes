--Needed help --Qm MonoidSection last qn 8
module MonoidMem where

import Data.Monoid 
import Test.QuickCheck (quickCheck, oneof, Arbitrary(arbitrary), Gen, sample, CoArbitrary)

newtype Mem s a = Mem { runMem :: s -> (a, s) }

-- See Q9 Combine a b & sumidiot
-- instance Semigroup a => Semigroup (Mem s a) where
--     (<>) (Mem f) (Mem g) = Mem (f <> g)  

instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g =
    Mem (\s -> let (a, s') = f s
                   (a', s'') = g s'
               in (a <> a', s''))
 -- Note that fs (a, s') for g s' to become s''. There is a feed loop a


instance Monoid a => Monoid (Mem s a) where
  -- no change scenario
--   mempty = Mem { runMem = \x -> (mempty, x)}
  mempty = Mem ((,) mempty)  -- got to be an kosong (,) by comparing with f'
  

-- s needs to become an integer where maths oeprations can happen so CoArbitrary
instance (Arbitrary a, Arbitrary s, CoArbitrary s) => Arbitrary (Mem s a) where
  arbitrary =  fmap Mem arbitrary -- or Mem `fmap` arbitrary

f' = Mem $ \s -> ("hi", s + 1)

-- To cater for f' functioning
instance Show (Mem s a) where
  show (Mem _) = "Mem"

checkMem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print ( rmleft == runMem f'  0)
  print ( rmright == runMem f'  0)

-- ("hi",1)
-- ("hi",1)
-- ("",0)
-- True
-- True


