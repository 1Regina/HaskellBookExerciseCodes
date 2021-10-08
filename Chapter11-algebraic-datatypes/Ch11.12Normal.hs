data Fiction            = Fiction deriving Show
data Nonfiction         = Nonfiction deriving Show
data BookType           = FictionBook Fiction 
                        | NonfictionBook Nonfiction 
                        deriving Show


-- rewrting to distinguish Type vs Data name
data FictionT           = FictionD deriving Show
data NonfictionT        = NonfictionD deriving Show
data BookType1          = FictionBook1 FictionT 
                        | NonfictionBook1 NonfictionT 
                        deriving Show


type AuthorName         = String
data Author0            = Author (AuthorName, BookType)

data Author             = FictionA AuthorName
                        | NonfictionA AuthorName 
                        deriving(Eq,Show)


data Expr               = Number Int
                        | Add Expr Expr
                        | Minus Expr
                        | Mult Expr Expr
                        | Divide Expr Expr


type Number             = Int 
type Add                = (Expr, Expr)
type Minus              = Expr 
type Mult               = (Expr, Expr)
type Divide             = (Expr, Expr)

type Expr1               = Either Number
                            (Either Add
                                (Either Minus
                                    (Either Mult Divide)))