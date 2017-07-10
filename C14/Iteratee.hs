{-# LANGUAGE DeriveFunctor #-}
import Data.Function (fix)
import Control.Monad
import qualified Control.Exception as Exc
import Control.Monad.IO.Class
import Control.Monad.Trans

data Stream a = Chunks [a] | EOF
                       deriving (Show, Eq,Functor)

instance Monoid (Stream a) where
         mempty = Chunks mempty
         mappend (Chunks xs) (Chunks ys) = Chunks (xs ++ ys)
         mappend _ _ = EOF

instance Monad Stream where
         return = Chunks . return
         Chunks xs >>= f = mconcat (fmap f xs)
         EOF >>= _ = EOF

instance Applicative Stream where
        pure = return
        (<*>) = ap

data Step a m b
     = Continue (Stream a -> Iteratee a m b)
     | Yield b (Stream a)
     | Error Exc.SomeException
       deriving Functor

newtype Iteratee a m b = Iteratee { runIteratee :: m (Step a m b)} 
                                                   deriving Functor

instance Monad m => Monad (Iteratee a m) where
         return x = yield x (Chunks [])
         m0 >>= f = ($ m0) $ fix $
               \bind m -> Iteratee $ runIteratee m >>= \r1 ->
                          case r1 of
                                Continue k -> return (Continue (bind . k))
                                Error err  -> return (Error err)
                                Yield x (Chunks []) -> runIteratee (f x)
                                Yield x extra -> runIteratee (f x) >>= \r2 ->
                                           case r2 of
                                                Continue k -> runIteratee (k extra)
                                                Error err -> return (Error err)
                                                Yield x' _ -> return (Yield x' extra)

instance Monad m => Applicative (Iteratee a m) where
       pure = return
       (<*>) = ap

instance MonadTrans (Iteratee a) where
         lift m = Iteratee (m >>= runIteratee . return)

instance MonadIO m => MonadIO (Iteratee a m) where
         liftIO = lift . liftIO

returnI :: Monad m => Step a m b -> Iteratee a m b
returnI step = Iteratee (return step)

yield :: Monad m => b -> Stream a -> Iteratee a m b
yield x extra = returnI (Yield x extra)

continue :: Monad m => (Stream a -> Iteratee a m b) -> Iteratee a m b
continue k = returnI (Continue k)

enumEOF :: Monad m => Enumerator a m b
enumEOF (Yield x _) = yield x EOF
enumEOF (Error err) = returnI (Error err)
enumEOF (Continue k) = k EOF >>== check where
        check (Continue _) = error "mEOF: divergent iteratee"
        check s = enumEOF s

run :: Monad m => Iteratee a m b
    -> m (Either Exc.SomeException b)
run i = do
        mStep <- runIteratee $ enumEOF ==<< i
        case mStep of
            Error err -> return $ Left err
            Yield x _ -> return $ Right x
            Continue _ -> error "run: divergent iteratee"

run_ :: Monad m => Iteratee a m b -> m b
run_ i = run i >>= either Exc.throw return

type Enumerator a m b = Step a m b -> Iteratee a m b

enumList :: Monad m => Int -> [a] -> Enumerator a m b
enumList n = loop where
        loop xs (Continue k) | not (null xs) = let
                (s1, s2) = splitAt n xs
                in k (Chunks s1) >>== loop s2
        loop _ step = returnI step

type Enumeratee ao ai m b = Step ai m b -> Iteratee ao m (Step ai m b)

infixl 1 >>==
infixr 1 ==<<
infixr 0 $$
infixr 1 >==>
infixr 1 <==<

(>>==) :: Monad m
       => Iteratee a m b
       -> (Step a m b -> Iteratee a' m b')
       -> Iteratee a' m b'
i >>== f = Iteratee (runIteratee i >>= runIteratee . f)

(==<<) :: Monad m
       => (Step a m b -> Iteratee a' m b')
       -> Iteratee a m b
       -> Iteratee a' m b'
(==<<) = flip (>>==)

($$) :: Monad m
     => (Step a m b -> Iteratee a' m b')
     -> Iteratee a m b
     -> Iteratee a' m b'
($$) = (==<<)

(>==>) :: Monad m
       => Enumerator a m b
       -> (Step a m b -> Iteratee a' m b')
       -> Step a m b
       -> Iteratee a' m b'
(>==>) e1 e2 s = e1 s >>== e2

(<==<) :: Monad m
       => (Step a m b -> Iteratee a' m b')
       -> Enumerator a m b
       -> Step a m b
       -> Iteratee a' m b'
(<==<) = flip (>==>)

throwError :: (Monad m, Exc.Exception e) => e -> Iteratee a m b
throwError exc = returnI (Error (Exc.toException exc))

joinI :: Monad m => Iteratee a m (Step a' m b)-> Iteratee a m b
joinI outer = outer >>= check where
	check (Continue k) = k EOF >>== \s -> case s of
		Continue _ -> error "joinI: divergent iteratee"
		_ -> check s
	check (Yield x _) = return x
	check (Error e) = throwError e

iterateeHead :: Monad m => Iteratee a m (Maybe a)
iterateeHead = continue loop 
             where
                loop (Chunks []) = iterateeHead
                loop (Chunks (x:xs)) = yield (Just x) (Chunks xs)
                loop EOF = yield Nothing EOF

iterateeLength :: Monad m => Iteratee stream m Int
iterateeLength = continue (loop 0)
             where
                loop n (Chunks []) = iterateeLength
                loop n (Chunks xs) = continue (loop (n + length xs))
                loop n EOF = yield n EOF

iterateeSum :: Monad m => Iteratee Int m Int
iterateeSum = continue (step 0)
             where 
                step n (Chunks []) = iterateeSum
                step n (Chunks xs) = continue (step (n + sum xs))
                step n EOF = yield n EOF

iterateeDrop :: Monad m => Int -> Iteratee a m ()
iterateeDrop n | n <= 0 = return ()
iterateeDrop n = continue (loop n) where
        loop n' (Chunks xs) = iter where
                len = length xs
                iter = if len < n'
                         then iterateeDrop (n' - len)
	                 else yield () (Chunks (drop n' xs))
        loop _ EOF = yield () EOF

drop1keep1 :: Monad m => Iteratee s m (Maybe s)
drop1keep1 = iterateeDrop 1 >> iterateeHead

alternates :: Monad m => Iteratee s m [Maybe s]
alternates = replicateM 5 drop1keep1

checkYield :: Monad m =>
        ((Stream i -> Iteratee i m a) -> Iteratee o m (Step i m a)) ->
        Enumeratee o i m a
checkYield _ y@(Yield x chunk) = return y
checkYield f (Continue k) = f k

iterateeMap :: Monad m => (o -> i) -> Enumeratee o i m a
iterateeMap f = checkYield $ continue . step where
        step k EOF = yield (Continue k) EOF
        step k (Chunks []) = continue $ step k
        step k chunk = k (fmap f chunk) >>== iterateeMap f

(=$) :: Monad m => Enumeratee ao ai m b -> Iteratee ai m b -> Iteratee ao m b
enum =$ iter = joinI (enum ==<< iter)
