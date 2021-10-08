module TraverseLaws where

-- 1. Naturality
-- This law tells us that function composition behaves in unsurprising ways with respect to a traversed function.Since a traversed function f generates the structure that appears on the “outside” of the traverse operation, there’s no reason we can’t float a function over the structure into the traversal itself.
t .  traverse f = traverse (t . f)


-- 2. Identity
-- This law states that traversing the data constructor of the Identity type over a value will produce the same result as just putting the value in Identity. This tells us that Identity represents a structural identity for traversing data. Another way of saying that a Traversable instance cannot add or inject any structure or effects.
traverse Identity = Identity

-- 3. Composition
traverse (Compose . fmap g . f) = Compose . fmap (traverse g) . traverse f
-- This law demonstrates how we can collapse sequential traversals into a single traversal, by taking advantage of the Compose datatype, which combines structure.

-- The sequenceA function must satisfy the following laws:

-- 1. Naturality
t. sequenceA = sequenceA . fmap t 

-- 2. Identity
sequenceA . fmap Identity = Identity

-- 3. Composition
sequenceA . fmap Compose = Compose . fmap sequenceA . sequenceA
