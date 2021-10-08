data FlowerType             = Gardenia
                            | Daisy
                            | Rose
                            | Lilac
                            deriving Show

type Gardener               = String
data Garden                 = Garden Gardener FlowerType deriving Show

-- data GardenN                = Gardener Gardenia
--                             | Gardener Daisy
--                             | Gardener Rose
--                             | Gardener Lilac

-- Note that Garden is the product Gardener * FlowerType, or
-- Gardener * (Gardenia | Daisy | Rose | Lilac), which distributes as
-- (Gardener * Gardenia) | (Gardener * Daisy) | (Gardener * Rose) | (Gardener * Lilac)

-- using the same re-write as they did for Author0, AuthorName and BookType I think the answer could be
data Garden'                = Gardenia' Gardener 
                            | Daisy' Gardener 
                            | Rose' Gardener 
                            | Lilac' Gardener