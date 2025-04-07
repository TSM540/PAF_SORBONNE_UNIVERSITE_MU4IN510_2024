
import System.IO
import Control.Monad.RWS.Class (MonadState(put))
newtype Id a= Id {runId::a} deriving (Show, Eq)

instance Functor Id where
  fmap f (Id a)= Id (f a)

instance Applicative Id where
    pure= Id
    f <*> x= Id $ runId f (runId x)

instance Monad Id where
    x >>= f= f (runId x)

-- >>> (+1) <$> (Id 1)
-- Id {runId = 2}

-- >>> (+) <$> (Id 1) <*> (Id 2)
-- Id {runId = 3}

-- >>> Id 3 >>= (\x -> Id (3+x) >>= (\y -> Id (x+y)))
-- Id {runId = 9}


newtype IdT m a = IdT {runIdT::m (Id a)} 

instance (Show (m a), Functor m) =>Show (IdT m a) where 
    show = show . fmap runId . runIdT 
--   string m a    m a          m (Id a) 

instance (Eq (m a ),Functor m)=> Eq (IdT m a) where
    (==)  x y =
            fmap runId (runIdT x) == fmap runId (runIdT y)

-- x :: IdT m a
-- runIdT x :: m (Id a)
-- fmap runId (runIdT x) :: m a
-- y :: IdT m a
-- runIdT y :: m (Id a)

fmaptIdT :: (Functor m)=>(a -> b) -> IdT m a -> IdT m b
-- fmaptIdT f x = IdT (fmap (fmap f) (runIdT x))
fmaptIdT f = IdT . fmap (fmap f) . runIdT
-- f :: a -> b
-- x :: IdT m a
-- runIdT x :: m (Id a)
-- fmap/m :: (c -> d) -> m c -> m d
-- fmap/Id :: (e -> f) -> Id e -> Id f
-- fmap/Id f ::  Id a -> Id b
-- c = Id a, d = Id b
-- fmap/m (fmap/Id f) :: m (Id a) -> m (Id b)
-- IdT(fmap/m (fmap/Id f) (runIdT x)) :: IdT m b



instance (Functor m)=>Functor (IdT m) where
    -- fmap :: (a -> b) -> IdT m a -> IdT m b
    fmap = fmaptIdT

pureIdT :: (Applicative m) => a -> IdT m a
pureIdT = IdT . pure . pure
-- x :: a
-- pure/Id x :: Id a 
-- pure/m (pure/Id x) :: m (Id a)
applyIdT ::(Applicative m)=>IdT m (a -> b) -> IdT m a -> IdT m b
applyIdT f x= IdT $ (<*>) ((<$>)(<*>)  (runIdT f)) (runIdT x)

-- f :: IdT m (a -> b)
-- x :: IdT m a
-- runIdT f :: m Id (a -> b)
-- runIdT x :: m (Id a)
-- <*>/m :: m (c -> d) -> m c -> m d
-- <*>/Id :: Id (e -> f) -> Id e -> Id f
-- <$>/m :: (g -> h) -> m g -> m h
-- g = Id (a -> b)
-- (Id (a -> b )->h) = Id (e -> f) ->Id e -> Id f
-- <$>/m <*> Id (runIdT f) :: m (Id a -> Id b)
-- <*>/m  (<$>/m <*> Id (runIdT f) (runIdT f)) :: m (Id a) -> m (Id b) 
-- (<*>/m  (<$>/m <*> Id (runIdT f) (runIdT f))) (runIdT x) :: m (Id b)
-- ((<*>/m  (<$>/m <*> Id (runIdT f) (runIdT f))) (runIdT x)) (runIdT x) :: IdT m b
instance (Applicative m)=>Applicative (IdT m) where
   pure = pureIdT
   (<*>) = applyIdT

bindIdT :: (Monad m)=>IdT m a -> (a -> IdT m b) -> IdT m b
bindIdT x f = IdT $ runIdT x >>= runIdT . f . runId 
-- bindIdT x f = IdT $ runIdT x >>= (\x -> runIdT (f (runId x)))


instance (Monad m)=>Monad (IdT m) where
    (>>=) = bindIdT

type Id2 a = IdT Id a

id2_vers_Id :: Id2 a -> Id a
id2_vers_Id = runId . runIdT

id_vers_Id2 :: Id a -> Id2 a
id_vers_Id2 = IdT . pure

prop_iso_id :: (Eq a) => Id2 a -> Bool
prop_iso_id x = id_vers_Id2 (id2_vers_Id x) == x

type Deep a = IdT (IdT IdT (IO)) a 

progIO :: IO Integer
progIO = do
    putStr "Entrez un entier : "
    str <- getLine
    return (read str)

hello :: Deep Integer
hello = (IdT . IdT .IdT) (
    do
        putStrLn "Hello"
        -- (return . return . return .putStrLn ) "Hello"
        -- (return . return . return .return ) 42
        fmap Id (fmap Id (fmap Id (putStrLn "hello")) )
    )

quaranteDeux :: Deep Integer
quaranteDeux = (IdT . IdT .IdT) (
    do
       return (return (return (return 42)))
    )
runDeep :: Deep a -> IO (Id (Id (Id a)))
runDeep =   runIdT . runIdT . runIdT

deep :: IO a -> Deep a
deep = IdT . IdT .IdT .fmap (Id . Id . Id) 

proIODeep :: Deep Integer
proIODeep = 
    do 
        deep (putStrLn "hello")
        str <- deep getLine 
        return (read str)

class MonadTrans t where 
    lift :: (Monad m)=>m a -> t m a 

instance MonadTrans IdT where
    lift = liftIdt

liftIdt :: (Monad m)=>m a -> IdT m a
liftIdt = IdT . fmap Id

progIOLift :: Deep Integer
progIOLift = 
    do
        lift $ lift $ lift $ putStrLn "hello"
        str <- lift $ lift $ lift getLine
        return (read str)

class Monad m => MonadIO m where
    liftIO :: IO a -> m a

instance MonadIO IO where
    liftIO = id

instance (MonadIO m)=>MonadIO (IdT m) where
    liftIO x = liftIdT $ liftIO x

progIOLiftIO:: Deep Integer 
progIOLiftIO = 
    do
        liftIO $ putStrLn "hello"
        str <- liftIO getLine
        return (read str)