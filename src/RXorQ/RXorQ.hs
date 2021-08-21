-- ABC185F Range Xor Query https://atcoder.jp/contests/abc185/tasks/abc185_f
-- Time 2838 2768 2805

import Data.Maybe
import Data.Bits
import qualified Data.ByteString.Char8 as B

parseInt :: B.ByteString -> Int
parseInt = fst . fromJust . B.readInt
readIntList :: IO [Int]
readIntList = map parseInt . B.words <$> B.getLine

main :: IO ()
main = do
    _ <- readIntList
    l <- readIntList
    qs <- map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents
    let bst = fromList (map Xor l) :: Tree Xor
    loop bst qs
    return ()

    where
        loop _ [] = return ()
        loop tree ([t,x,y]:qs) = do
            if t == 1 then do
                let tree' = update tree (x - 1) (get tree (x - 1) `mappend` (Xor y))
                loop tree' qs
            else do
                print $ fromXor $ query tree (x - 1,y)
                loop tree qs

-- segment tree
data Tree v = Null | Node (Int,Int) v (Tree v) (Tree v)
    deriving (Show)

val :: Monoid v => Tree v -> v
val (Node (_,_) v _ _) = v
val Null           = mempty

rl,rr :: Tree v -> Int
rl (Node (a,_) _ _ _) = a
rr (Node (_,b) _ _ _) = b

fromList :: Monoid v => [v] -> Tree v
fromList xs = makeTree 0 ((length xs) - 1) xs

makeTree :: Monoid v => Int -> Int -> [v] -> Tree v
makeTree l r es = loop $ map (uncurry f) (zip [l..r] es)
    where
        loop [x] = x
        loop xs = loop $ buildTree xs
        f :: Int -> v -> Tree v
        f idx v = Node (idx,idx + 1) v Null Null
        buildTree (a:b:ys) =
            let v = val a `mappend` val b
            in Node (rl a,rr b) v a b : buildTree ys
        buildTree x = x

query :: Monoid v => Tree v -> (Int,Int) -> v
query (Node (l,r) v left right) (a,b)
    | b <= l || r <= a = mempty
    | a <= l && r <= b = v
    | otherwise        = (query left (a,b)) `mappend` (query right (a,b))

update :: Monoid v => Tree v -> Int -> v -> Tree v
update (Node (l,r) _ left right) idx x
    | l + 1 == r = Node (l,r) x left right
    | idx < m =
        let newleft = update left idx x
        in Node (l,r) ((val newleft) `mappend` (val right)) newleft right
    | otherwise =
        let newright = update right idx x
        in Node (l,r) ((val left) `mappend` (val newright)) left newright
    where m = rr left

get :: Monoid v => Tree v -> Int -> v
get (Node (l,r) v left right) idx
    | l + 1 == r = v
    | idx < m    = get left idx
    | otherwise  = get right idx
    where m = rr left
-- end segment tree

-- start Xor
newtype Xor = Xor { fromXor :: Int } deriving (Ord,Show,Eq)
instance Monoid Xor where
    mempty  = Xor 0
    Xor a `mappend` Xor b = Xor (a `xor` b)
instance Semigroup Xor where
    Xor a <> Xor b = Xor (a `xor` b)
-- end Xor
