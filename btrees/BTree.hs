module BTree where
import Control.Applicative


data BinTree a = Empty | Node a (BinTree a) (BinTree a) deriving(Show, Eq)

instance Functor BinTree
    where
        fmap _ Empty = Empty
        fmap f (Node a l r) = Node (f a) (fmap f l) (fmap f r)

instance Applicative BinTree where
    -- pure :: a -> BTree a
    pure a = Node a Empty Empty
    -- <*> :: BinTree (a -> b) BinTree a -> BinTree b
    (<*>) :: BinTree (a -> b) -> BinTree a -> BinTree b
    Empty <*> _ = Empty
    _ <*> Empty = Empty
    -- Node f ((l f) (r f)) <*> Node x (L x) (R x) = Node (f x) ((l f) <*> (L x)) ((r f) <*> (R x))
    Node f l r <*> Node x left right = Node (f x) (l <*> left) (r <*> right)

instance Alternative BinTree where
    empty = Empty
    Empty <|> ys = ys
    xs <|> Empty = xs
    (Node x lx rx) <|> (Node y ly ry) = Node x (lx <|> ly) (rx <|> ry)


insert :: Ord a => a -> BinTree a -> BinTree a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
    | x < y = Node y (insert x left) right
    | otherwise = Node y left (insert x right)


increment :: BinTree Int -> BinTree Int
increment = fmap (+ 1)

convertToString :: BinTree String -> BinTree Int
convertToString = fmap length


addMaybes :: Maybe Int -> Maybe Int -> Maybe Int
addMaybes mx my = (+) <$> mx <*> my


-- Función para mostrar un árbol binario
displayTree :: Show a => BinTree a -> String
displayTree Empty = "Empty"
displayTree (Node a left right) = "(" ++ show a ++ " " ++ displayTree left ++ " " ++ displayTree right ++ ")"


mergeTrees :: Ord a => BinTree a -> BinTree a -> BinTree a
mergeTrees tree1 Empty = tree1
mergeTrees Empty tree2 = tree2
mergeTrees tree1 (Node x left right) = mergeTrees (insert x tree1) left `mergeTrees` right


main :: IO ()
main = do
    let tree1 = Node 5 (Node 3 Empty Empty) (Node 8 Empty Empty)
        tree2 = Node 6 (Node 2 Empty Empty) (Node 7 Empty Empty)
        mergedTree = mergeTrees tree1 tree2
    putStrLn "Árbol original 1:"
    putStrLn $ displayTree tree1
    putStrLn "\nÁrbol original 2:"
    putStrLn $ displayTree tree2
    putStrLn "\nÁrbol fusionado:"
    putStrLn $ displayTree mergedTree
