module Optional where
import Data.Monoid()
-- refer pg 899
data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

-- must have semigroup instance to do monoid instance otherwise cannot load - p899
-- bcos orphan Monoid instances otherwise without Semigroup
instance Semigroup (Optional a) where
    (<>) _ Nada = Nada
    (<>) Nada _ = Nada
    (<>) (Only a) _ = Only a
    -- (<>) _ (Only a) = Only a
    -- (<>) (Only a) (Only a') = Only $ mappend a a'

instance Monoid a => Monoid (Optional a) where
  mempty = Nada

  mappend (Only x) (Only y) = Only $ mappend x y
  mappend (Only x) _        = Only x
  mappend _        (Only y) = Only y
  mappend _        _        = Nada

  -- tests
-- import Data.Monoid
-- Only (Sum 1) `mappend` Only (Sum 1)          produces Only (Sum {getSum = 2})
-- Only (Product 4) `mappend` Only (Product 2)  produces Only (Product {getProduct = 8})
-- Only (Sum 1) `mappend` Nada                  produces Only (Sum {getSum = 1})
-- Only [1] `mappend` Nada                      produces Only [1]
-- Nada `mappend` Only (Sum 1)                  produces Only (Sum {getSum = 1})
