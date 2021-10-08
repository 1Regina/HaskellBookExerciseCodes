-- 8.3
f :: Bool  -> Int
f True = error "blah"
f False = 0

-- >>> f False
-- 0
--
-- >>> f True  - Exception will show
-- *** Exception: blah
-- CallStack (from HasCallStack):
--   error, called at c:\Users\regina\Desktop\SMU\HaskellBook\Ch08.3Bottom.hs:3:10 in main:Main
--

