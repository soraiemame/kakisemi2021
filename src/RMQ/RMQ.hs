-- Range Minimum Query https://onlinejudge.u-aizu.ac.jp/courses/library/3/DSL/2/DSL_2_A
-- Time 00.64 00.65 00.65

import Data.Maybe
import Data.Monoid
import Control.Applicative
import qualified Data.ByteString.Char8 as B

parseInt :: B.ByteString -> Int
parseInt = fst . fromJust . B.readInt
readIntList :: IO [Int]
readIntList = map parseInt . B.words <$> B.getLine

newtype Min = Min { fromMin :: Int } deriving (Ord,Show,Eq)
instance Monoid Min where
    mempty  = Min (2 ^ 31 - 1)
    mappend = min

main :: IO ()
main = do
    [n,q] <- readIntList
    qs <- map (map (fst . fromJust . B.readInt) . B.words) . B.lines <$> B.getContents
    let st = fromList $ replicate n mempty :: Tree (Min)
    loop st qs
    return ()
    where
        loop _ [] = return ()
        loop tree ([q,x,y]:qs) = do
            if q == 0 then do
                let tree' = update tree x (Min y)
                loop tree' qs
            else do
                print . fromMin $ query tree (x,y + 1)
                loop tree qs

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
