module TediousCode 
        ( decodeFn
        , fetchFn
        , makeIoOnlyObj
        , pipelineFn
        , pipelineFnPF
        , pipelineFnPFTraverse 
        )where

data Query          = Query
data SomeObj        = SomeObj
data IoOnlyObj      = IoOnlyObj
data Err            = Err

-- decoder function to make string to object. Haskell is ok to work only with type
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

--  a query that runs against a database and returnsan array of strings
fetchFn   :: Query  -> IO [String]
fetchFn  = undefined

-- additional “context initializer” that per-formsIO
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj,IoOnlyObj)]
makeIoOnlyObj = undefined


-- Consolidate into...
pipelineFn :: Query -> IO (Either Err [(SomeObj , IoOnlyObj )] )
pipelineFn query = do
    a <- fetchFn query

        -- original before clean up
    -- case sequence (map decodeFn a) of
    --     (Left err) -> return $ Left err
    --     (Right res) -> do 
    --         a <- makeIoOnlyObj res 
    --         return $ Right a

        -- after clean up
    traverse makeIoOnlyObj (mapM decodeFn a)

        -- or point free 
pipelineFnPF :: Query 
            -> IO (Either Err [(SomeObj , IoOnlyObj )] )
pipelineFnPF = ((traverse makeIoOnlyObj . mapM decodeFn) =<<) .fetchFn
        
        -- since mapM is just traverse with a slightly different type
pipelineFnPFTraverse :: Query 
            -> IO (Either Err [(SomeObj , IoOnlyObj )] )
pipelineFnPFTraverse = ((traverse makeIoOnlyObj 
                        . traverse decodeFn) =<<) .fetchFn