module Editor.Block where

data Block = Block {bStart :: Int
                   ,bEnd :: Int
                   ,bContent :: String
                   } deriving Show

whiteString :: String -> Bool
whiteString "" = True
whiteString (x:xs) = if elem x " \t\n" then whiteString xs else False

linesNum :: String -> [(Int,String)]
linesNum s = zip [0..] (addNewLine . lines $ s)

blocks' :: [(Int,String)] -> [[(Int,String)]]
blocks' ss = case break (whiteString . snd) ss of
            ([], (_:ys)) -> blocks' ys
            (xs, (_:ys)) -> xs:(blocks' ys)
            (xs, []) -> [xs]

blocks :: [[(Int,String)]] -> [Block]
blocks [] = []
blocks ([]:_) = []
blocks (b:bs) = (Block {bStart = (fst . head) b , bEnd = (fst . last) b, bContent = concatMap snd b}):(blocks bs)

getBlock :: Int -> [Block] -> Maybe Block
getBlock _ [] = Nothing
getBlock num ( block@(Block n1 n2 _):bs) = if n1 <= num && num <= n2 then Just block else getBlock num bs

getBlocks :: String -> [Block]
getBlocks = blocks . blocks' . linesNum

addNewLine :: [String] -> [String]
addNewLine [] = []
addNewLine [x] = [x]
addNewLine (x:xs) = (x ++ "\n") : (addNewLine xs)

getLineContent :: Int -> [(Int,String)] -> Maybe Block
getLineContent _ [] = Nothing
getLineContent num ((n,s):ls) | n == num = Just $ Block n n s
                            | otherwise = getLineContent num ls
