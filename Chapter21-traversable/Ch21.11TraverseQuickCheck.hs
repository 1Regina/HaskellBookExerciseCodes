-- add the following to a module and change the type alias to determince which instances to test


type TI = []

main = do
    let trigger :: TI (Int, Int, [Int])
        trigger = undefined
    quickBatch (traversable trigger)

