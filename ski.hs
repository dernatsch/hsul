import System.Environment

-- s a b c = (a c) $ (b c)
-- 
-- k :: a -> b -> a
-- k a b = a
-- 
-- i :: a -> a
-- i = (s k k)

data SKIProg = S | S' SKIProg | S'' SKIProg SKIProg | K | K' SKIProg | I | P Char | R | Ap SKIProg SKIProg | EmptyProg deriving (Show, Read, Eq)

parse :: String -> Maybe SKIProg
parse s = do
    (p, _) <- parse' s
    return p

parse' :: String -> Maybe (SKIProg, String)
parse' "" = Nothing

parse' ('`':xs) = do
    (lb, rp) <- parse' xs
    (rb, re) <- parse' rp
    return (Ap lb rb, re)

parse' ('s':xs) = Just (S, xs)
parse' ('k':xs) = Just (K, xs)
parse' ('i':xs) = Just (I, xs)
parse' ('.':c:xs) = Just (P c, xs)
parse' ('r':xs) = Just (R, xs)
parse' (' ':xs) = parse' xs
parse' ('\n':xs) = parse' xs
parse' _ = Nothing

eval :: SKIProg -> IO SKIProg
eval (Ap f g) = do
    f' <- eval f
    g' <- eval g
    apply f' g'
eval c = return c

apply :: SKIProg -> SKIProg -> IO SKIProg
apply I x = do
    x' <- eval x
    return x'

apply (P c) x = do
    putChar c
    x' <- eval x
    return x'

apply R x = do
    putStrLn ""
    x' <- eval x
    return x'

apply K x = do
    x' <- eval x
    return (K' x)

apply (K' x) y = return x

apply S x = do
    x' <- eval x
    return (S' x')

apply (S' x) y = do
    y' <- eval y
    return (S'' x y)

apply (S'' x y) z = do
    z' <- eval z
    eval (Ap (Ap x z) (Ap y z))

run' :: Maybe SKIProg -> IO ()
run' Nothing = return ()
run' (Just p) = do
    _ <- eval p
    return ()

run :: String -> IO()
run = run' . parse

main = do
    [f] <- getArgs
    p <- readFile f
    run p
