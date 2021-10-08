module HttpStuff where
-- make calls to a handy-dandywebsite for testing HTTP clients athttp://httpbin.org/.
import Data.ByteString.Lazy hiding (map)
import Data.Functor.Identity 
import Data.Monoid 
import Network.Wreq
import Data.Functor.Constant 

-- replaced with other websites if desired or needed

urls :: [String]
urls = [ "http://httpbin.org/ip"
       , "http://httpbin.org/bytes/5"
       ]
       
-- a list of IO actions we can performto get a respons
mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

-- rather, to have one big IO action that produces a list of responses
traversedUrls:: IO [Response ByteString]
traversedUrls = traverse get urls

-- *TediousCode Data.Functor.Identity>  import Data.Functor.Identity
-- *TediousCode Data.Functor.Identity>  traverse (Identity . (+1)) [1, 2]
-- Identity [2,3]
-- *TediousCode Data.Functor.Identity> runIdentity $ traverse (Identity . (+1)) [1, 2]
-- [2,3]
-- *TediousCode Data.Functor.Identity> edgeMap f t = runIdentity $ traverse (Identity . f) t
-- *TediousCode Data.Functor.Identity> :t edgeMap
-- edgeMap :: Traversable t => (a -> b) -> t a -> t b -- almost like :t fmap
-- *TediousCode Data.Functor.Identity> edgeMap (+1) [1..5]
-- [2,3,4,5,6]
-- *TediousCode Data.Functor.Identity Data.Monoid Data.Functor.Constant>  xs' = [1, 2, 3, 4, 5]
-- *TediousCode Data.Functor.Identity Data.Monoid Data.Functor.Constant> xs = xs' :: [Sum Integer]
-- *TediousCode Data.Functor.Identity Data.Monoid Data.Functor.Constant>  traverse (Constant . (+1)) xs
-- Constant (Sum {getSum = 20})

-- Prelude> import Data.Functor.Identity as FI
-- Prelude FI> import Data.Monoid as Mon
-- Prelude FI Mon> import Data.Functor.Constant as FuncConst
-- Prelude FI Mon FuncConst> foldMap' f t = getConstant $ traverse (Constant . f) t 
-- Prelude FI Mon FuncConst> :t foldMap'
-- foldMap' :: (Traversable t, Monoid a1) => (a2 -> a1) -> t a2 -> a1
-- Prelude FI Mon FuncConst> :t foldMap
-- foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
-- Prelude FI Mon FuncConst> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
