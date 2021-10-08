import Data.Monoid ( Sum, Product )
ys :: [Sum Int]
ys = [2, 4, 6] :: [Sum Int]
-- >>> foldr mappend mempty ys
-- Sum {getSum = 12}
--

xs :: [Product Int]
xs = [2, 4, 6] :: [Product Int]
-- >>> foldr mappend mempty xs
-- Product {getProduct = 48}
--

strList = ["blah", "woot"]
-- >>> foldr mappend mempty strList
-- "blahwoot"
--

-- >>> 2 + 3
-- 5
--
