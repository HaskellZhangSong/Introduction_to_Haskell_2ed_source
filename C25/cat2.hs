{-# LANGUAGE CPP, TypeFamilies, MultiParamTypeClasses, PolyKinds, TypeOperators,FunctionalDependencies, EmptyDataDecls,TypeSynonymInstances, RankNTypes,FlexibleContexts, UndecidableInstances, AllowAmbiguousTypes,ExistentialQuantification, FlexibleInstances, GADTs, ConstraintKinds,StandaloneDeriving, InstanceSigs #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

import Prelude hiding (Functor,fmap, Applicative, pure, (<*>), (**), (.), id, Monad, return, (>>=))
#if __GLASGOW_HASKELL__ >= 800
import GHC.Types (Constraint)
import GHC.Exts (Any)
#else
import GHC.Prim (Constraint)
import GHC.Prim (Any)
#endif
import qualified Data.Type.Equality as E
import Data.Type.Equality ((:~:))
import Data.Coerce

type family (~>) :: k -> k1 -> *
type instance (~>) = (->)

class Category cat where
    id :: cat a a
    (.) :: cat b c -> cat a b -> cat a c

type Hask = (->)
instance Category Hask where
    id x = x
    (.) g f x = g (f x)

class Functor f where
    fmap :: (a ~> b) -> (f a ~> f b)

class (Category r, Category t) => 
        CategoricalFunctor f (r :: i -> i -> *) (t :: j -> j -> *) 
                             | f r -> t, f t -> r where
    cfmap :: r a b -> t (f a) (f b)

instance CategoricalFunctor Maybe (->) (->) where
    cfmap f Nothing  = Nothing
    cfmap f (Just a) = Just (f a)

class (Category r, Category t) => CategoricalContravariant f r t | f r -> t, f t -> r where
    ccontramap :: r a b -> t (f b) (f a)

class Contravariant f where
    contramap  :: (b ~> a) -> (f a ~> f b)

instance Functor ((->) a) where
    fmap f g = (f . g)

data Op b a = Op (a -> b)

instance Contravariant (Op a) where
    contramap h (Op g) = Op (g.h)

newtype Compose f g a = Compose { getCompose :: f (g a) }
newtype (:.:) f g p = Comp1 { unComp1 :: f (g p) } 
                      deriving Show

instance (Functor f, Functor g) => Functor (g :.: f) where
     fmap f (Comp1 a) = Comp1 (fmap (fmap f) a)

newtype Identity a = Identity {runIdentity :: a}
                     deriving Show

instance Functor Identity where
     fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
     pure a = Identity a
     (<*>) (Identity f) (Identity a) = (Identity (f a))

instance Monad Identity where
     return = pure
     (>>=) (Identity a) f = f a

instance Functor [] where
     fmap f [] = []
     fmap f (x:xs) = f x : fmap f xs

idphi :: Functor f => (f :.: Identity) a -> f a
idphi (Comp1 a) = fmap runIdentity a

idpsy :: Functor f => f a -> (f :.: Identity) a
idpsy xs = Comp1 (fmap Identity xs)

newtype Nat f g = Nat {runNat :: forall a. f a ~> g a}

type instance (~>) = Nat

fphi :: (Functor f, Functor g, Functor h) => 
                             ((f :.: g) :.: h) a -> (f :.: (g :.: h)) a
--  (:.:) (f :.: g) h a = (Comp1 (Comp1 (f (g (h a)))))
--  (:.:) f (g :.: h) a = Comp1 (f ((g :.: h) a)) = Comp (f (Comp1 (g (h a))))
--   fg_h :: Comp1 ((f :.: g) (h a)) = Comp1 (Comp1 (f (g (h a)
fphi (Comp1 (Comp1 fgh)) = Comp1 (fmap Comp1 fgh)

fpsy :: (Functor f, Functor g, Functor h) => (f :.: (g :.: h)) a -> ((f :.: g) :.: h) a
fpsy (Comp1 f_gh) = Comp1 (Comp1 (fmap unComp1 f_gh))

instance Category ((~>) :: i -> i -> *) => 
                  Category (Nat :: (k -> i) -> (k -> i) -> *) where
    id = Nat id
    Nat f . Nat g =  Nat (f.g)

instance CategoricalFunctor Either (->) Nat where
        cfmap f = Nat $ \g -> case g of
                                Left a  -> Left (f a)
                                Right a -> Right a

instance Functor ((,) a) where
    fmap f (a,b) = (a , f b)

instance Functor (Either a) where
    fmap f (Left a) = Left a
    fmap f (Right b) = Right (f b)

instance Functor Either where
    fmap f = Nat $ \g -> case g of
                            Left  a -> Left (f a)
                            Right b -> Right b

instance Functor (,) where
    fmap f = Nat $ \g -> case g of
                            (a,b) -> (f a, b)

instance Contravariant (->) where
    contramap f = Nat $ \g -> g . f

nat2 :: (forall a b . f a b ~> g a b) -> f ~> g
nat2 f = Nat $ Nat f

runNat2 :: Nat f g -> f a b ~> g a b
runNat2 = runNat . runNat 

nat3 :: (forall a b c. f a b c ~> g a b c) -> f ~> g
nat3 f = Nat $ Nat $ Nat f

runNat3 :: Nat f g -> f a b c ~> g a b c
runNat3 = runNat . runNat . runNat

data Or a b c = A a | B b | C c deriving (Eq, Show)

instance Functor Or where
    fmap f = Nat $ Nat $ \g -> case g of
                                   A x -> A (f x)
                                   B x -> B x
                                   C x -> C x
instance Functor (Or a) where
    fmap f = Nat $ \g -> case g of
                             A x -> A x
                             B x -> B (f x)
                             C x -> C x

instance Functor (Or a b) where
    fmap f = \g -> case g of
                       A x -> A x
                       B x -> B x
                       C x -> C (f x)

n1,n2,n3 :: Nat ((->) Bool) Maybe
n1 = Nat $ \f -> Nothing
n2 = Nat $ \f -> Just (f False)
n3 = Nat $ \f -> Just (f True)

verticalComp :: (Functor (f :: k -> *) , Functor (g :: k -> *), Functor (h :: k -> *)) => 
                          Nat f g -> Nat g h -> Nat f h
verticalComp u@(Nat fg) t@(Nat gh) = Nat (gh.fg)

horizontalComp :: (Functor f, Functor f', Functor g, Functor g') => 
                         Nat f' g' -> Nat f g -> Nat (f':.:f) (g' :.: g)
                  -- f'g' :: f' a -> g' a
                  -- fg   :: f  a -> g  a
                  -- x    :: f' (f a)
                  -- f'g' x :: g' (f a)
                  -- fmap fg :: g' (f a) -> g' (g a)
horizontalComp (Nat f'g') (Nat fg) = Nat $ \(Comp1 x) -> Comp1 (fmap fg (f'g' x))

preFComp :: (Functor f, Functor g, Functor h)
         => Nat g h -> g (f a) -> h (f a)
preFComp (Nat gh) fa = gh fa

postFComp :: (Functor f, Functor g, Functor h)
         => Nat g h -> f (g a) -> f (h a)
postFComp (Nat gh) fa = fmap gh fa

instance Functor f => Functor ((:.:) f) where
   fmap (Nat u) = Nat $ \(Comp1 fah) -> Comp1 $ fmap u fah

newtype (:-.:) (f :: k1 -> k) (g :: k -> *) (p :: k1) 
                       = FlipComp1 { unFlipComp1 :: (g (f p))}

instance Functor f => Functor ((:-.:) f) where
   fmap (Nat u) = Nat $ \(FlipComp1 fa) -> FlipComp1 (u fa)

-------------------------------------------
-- Class Dict
-------------------------------------------

data Dict (p :: Constraint) where
     Dict :: p => Dict p

instance Show (Dict p) where
     show Dict = "Dict"

-- Sub :: (p => Dict q) -> p :- q
newtype p :- q = Sub (p => Dict q)

instance Show (p :- q) where
     showsPrec d _ = showParen (d > 10) $ showString "Sub Dict"

(\\) :: p => ((q => r) -> (p :- q) -> r)
r \\ (Sub Dict) = r

trans :: (q :- r) -> (p :- q) -> (p :- r)
trans f g = Sub $ (Dict \\ f) \\ g

refl :: a :- a
refl = Sub Dict

instance Category (:-) where
    id = refl
    (.) = trans

type instance (~>) = (:-)

instance Functor Dict where
    fmap :: (a :- b) -> Dict a ~> Dict b
    fmap p Dict = case p of Sub q -> q
 
unfmap :: (Dict a -> Dict b) -> a :- b
unfmap f = Sub (f Dict)

data Foo = Foo Int

instance () => Eq Foo where
    (==) = undefined

class Any => Bottom where
  no :: Dict a

bottom :: Bottom :- a
bottom = Sub no


-------------------------------------------
-- Equality Category
-------------------------------------------

instance Category (:~:) where
   id = E.Refl
   (.) = flip $ E.trans

-------------------------------------------
-- Yoneda
-------------------------------------------

data Yoneda f a = Yoneda { runYoneda :: (forall b. (a -> b) -> f b) }

instance Functor (Yoneda f) where
    -- (a -> b) -> Yoneda f a -> Yoneda f b
    fmap f y = Yoneda (\k -> runYoneda y (k . f))

{-
instance Functor f => Functor (Yoneda f) where
    fmap f y = yphi $ fmap f (ypsi y)
-}

instance Applicative f => Applicative (Yoneda f) where
    pure a = Yoneda $ \f -> pure (f a)
    -- y2 :: forall b2. (a -> b2) -> f b2
    -- y1 :: forall b1. ((a -> b) -> b1) -> f b1
    -- f  :: b -> b1
    -- (<*>) :: Yoneda f (a -> b) -> Yoneda f a -> Yoneda f b
    Yoneda y1 <*> Yoneda y2 = Yoneda (\f -> y1 (\g -> f . g) <*> y2 id)

{-
instance Applicative f => Applicative (Yoneda f) where
    pure a = yphi $ pure a
    y1 <*> y2 = let f = ypsi y1
                    x = ypsi y2
                in yphi (f <*> x)
-}
instance Monad m => Monad (Yoneda m) where
    return a = Yoneda (\f -> return (f a))
    Yoneda m >>= k = Yoneda (\f -> m id >>= \a -> runYoneda (k a) f)

{-
instance Monad m => Monad (Yoneda m) where
    return a = yphi $ return a
    ym >>= ayn = let m = ypsi ym
                     n = \x -> ypsi (ayn x)
                 in yphi (m >>= n)
-}

instance MonadTrans Yoneda where
    lift a = Yoneda (\f -> fmap f a)


mb1,mb2,mb3 :: Maybe Bool
mb1 = Nothing
mb2 = Just False
mb3 = Just True

ymb1,ymb2,ymb3 :: Yoneda Maybe Bool
ymb1 = Yoneda (\g -> Nothing)
ymb2 = Yoneda (\g -> Just $ g False)
ymb3 = Yoneda (\g -> Just $ g True)

instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just a) = Just (f a)

yphi :: Functor f => f a -> Yoneda f a
yphi x = Yoneda (\f -> fmap f x)

ypsi :: Yoneda f a -> f a
ypsi (Yoneda f) = f id

data Coyoneda f a = forall b. Coyoneda (b -> a) (f b)

instance Functor (Coyoneda f) where
    fmap f (Coyoneda g v) = Coyoneda (f . g) v

liftCoyoneda :: f a -> Coyoneda f a
liftCoyoneda f = Coyoneda id f

lowerCoyoneda :: Functor f => Coyoneda f a -> f a
lowerCoyoneda (Coyoneda g v) = fmap g v

class CategoricalFunctor f a a => CategoricalEndofunctor f a
instance CategoricalFunctor f a a => CategoricalEndofunctor f a

instance CategoricalFunctor Identity (->) (->) where
    cfmap f (Identity a) = Identity (f a)

instance CategoricalEndofunctor Identity (->)

-- class Monad m where
--     return     :: a -> m a
--     join       :: forall a. m (m a) -> m a

etaT :: Monad m => (Identity :.: m) a -> m (m a)
etaT (Comp1 (Identity ma)) = return ma

tEta :: (Functor m, Monad m) => (m :.: Identity) a -> m (m a)
tEta (Comp1 mi) = fmap (return . runIdentity) mi

eq1 :: Monad m => (Identity :.: m) a -> m a
eq1 (Comp1 (Identity ma)) = ma

eq2  :: (Functor m, Monad m) => (m :.: Identity) a -> m a
eq2 (Comp1 mi) = fmap runIdentity mi

newtype Kleisli m a b = K (a -> m b)
type (a :~> b) m = Kleisli m a b

instance Monad m => Category (Kleisli m) where
    id            = K return
    (K f) . (K g) = K (f <=< g)

instance Monad m => Functor (Kleisli m a) where
    fmap f (K amb) = K $ \a -> amb a >>= \t -> return (f t)

class Functor f => Applicative f where
    pure :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

class Applicative m => Monad m where
    return :: a -> m a
    return = pure
    
    join  :: m (m a) -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    (<=<) :: (b -> m c) -> (a -> m b) -> a -> m c
    
    join m = (m >>= id)
    join m = (id <=< id) m
    
    m >>= f = join (fmap f m)
    m >>= f = flip (<=< id) m f
    
    g <=< f = \x -> f x >>= g
    g <=< f = join . fmap g . f
    {-# MINIMAL return, (>>=) | return, (<=<) | return, join #-}

-------------------------------------
-- Monad homomorphism
-------------------------------------
-- newtype m ~~> n = MonadMorph { runMonadMorph :: forall a . m a -> n a }

-- instance Category (~~>) where
--   id = MonadMorph $ \x -> x
--   (.) (MonadMorph f) (MonadMorph g) = MonadMorph (f.g)

type m ~~> n = forall a. (Monad m, Monad n) => m a -> n a

data Proxy a = Proxy

instance Functor Proxy where
    fmap _ _ = Proxy

instance Applicative Proxy where
    pure _ = Proxy
    (<*>) _ _ = Proxy

instance Monad Proxy where
    return = pure
    (>>=) _ _ = Proxy
-- collapse 
terminal_monad :: Monad m => m ~~> Proxy
terminal_monad _ = Proxy

-- generalize
initial_monad :: Monad m => Identity ~~> m
initial_monad (Identity a) = return a

class Lifting p t where
  lifting :: p a :- p (t a)

class MonadTrans (t :: (* -> *) -> * -> *) where
  lift :: Monad m => m ~~> t m -- return
  transform :: Monad m :- Monad (t m)

class (Lifting Monad t, MonadTrans t) => MPointed t
instance (Lifting Monad t, MonadTrans t) => MPointed t

class (Lifting Monad t) => MFunctor t where
  hoist :: (Monad m, Monad n) => (m ~~> n) -> (t m ~~> t n)

newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }
type State s a = StateT s Identity a

instance Functor (StateT s m) where
instance Applicative (StateT s m) where
instance Monad (StateT s m) where

instance MonadTrans (StateT s) where
  lift m = StateT $ \s -> m >>= \a -> return (a,s)

instance Lifting Monad (StateT s) where
  lifting = Sub Dict

instance MFunctor (StateT s) where
  hoist nat m = StateT (\s -> nat (runStateT m s))

state :: (s -> (a,s)) -> State s a
state f = StateT $ \s -> return $ f s 

push :: a -> State [a] ()
push a = state $ \xs -> ((), a:xs)

pop :: State [a] a
pop = state $ \(x:xs) -> (x,xs)

pushT :: Monad n => a -> StateT [a] n ()
pushT a = hoist initial_monad (push a)

popT :: Monad n => StateT [a] n a
popT = hoist initial_monad pop

class MFunctor t => MMonad t where
  embed :: (Monad m, Monad n) => (m ~~> t n) -> t m ~~> t n
--  embed :: Monad m => (m a -> t n a) -> t m a -> t n a 
--  embed f m = squash (hoist f m)
  squash :: Monad m => t (t m) ~~> t m
  squash = embed id

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
instance Functor m => Functor (MaybeT m) where
instance Monad m => Applicative (MaybeT m) where
instance Monad m => Monad (MaybeT m) where
instance Monad m => MonadTrans MaybeT where
instance Lifting Monad MaybeT where lifting = Sub Dict

instance MFunctor MaybeT where
    hoist nat m = MaybeT (nat (runMaybeT m))

instance MMonad MaybeT where
    embed f m = MaybeT ((runMaybeT (f (runMaybeT m))) >>= 
                                       \x -> return (case x of
                                                 Nothing -> Nothing
                                                 Just Nothing -> Nothing
                                                 Just (Just a) -> Just a))

foo :: Identity a -> MaybeT Proxy a
foo (Identity a) = return a


--------------------------------------------------
class (Functor f, Functor g) => Adjoint f g where
    unit :: a -> (g (f a))
    counit :: (f (g a)) -> a

    phi :: Adjoint f g => (f a -> b) -> a -> g b
    phi f = fmap f . unit

    psi :: Adjoint f g => (a -> g b) -> f a -> b
    psi g = counit . fmap g


instance Adjoint ((,) a) ((->) a) where
  unit :: b -> (a -> (a, b))
  unit x = \y -> (y, x)

  counit :: (a, a -> b) -> b
  counit (y, f) = f y

newtype Prod a b = Prod (b, a)
instance Functor (Prod a) where
    fmap f (Prod (x, a)) = Prod ((f x), a)

instance Adjoint (Prod a) ((->) a) where
    -- unit :: b -> (a -> (b, a))
    unit :: b -> (a -> Prod a b)
    unit b = \a -> Prod (b, a)

    -- counit :: (a -> b, a) -> b
    counit :: Prod a (a -> b) -> b
    counit (Prod (f, a)) = f a

-- use (, a) replace Prod a
-- curry: Hom((a, b), c) = Hom(a, c^b) :uncurry
-- function phi
curry :: ((a, b) -> c) -> a -> b -> c
curry f x y = f (x, y)
-- function psi
uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f (x, y) = f x y

-- use (, a) replace Prod a
-- unit   =     (,)     = curry   id = coeval :: a -> b -> (a, b)
-- counit = uncurry ($) = uncurry id = eval   :: (b -> a, b) -> a

instance (Functor f , Functor g) => Applicative (f :.: g) where

uncomp :: (Functor f,Functor g, Functor h,Functor i) 
                   => (f :.: g) ((h :.: i) a) -> (f (g (h (i a))))
uncomp x = unComp1 (fmap unComp1 x)

instance (Adjoint f g) => Monad (g :.: f) where
    return a = Comp1 (unit a)
    join a = Comp1 (fmap counit (uncomp a))

instance Monad (((->) s) :.: (Prod s)) where
    return :: a -> (((->) s) :.: (Prod s)) a
 -- return :: a -> (s -> (a, s))
 -- return :: a -> (s -> Prod s a)
 -- return x = Comp1 (unit x)
    return x = Comp1 (\s -> Prod (x,s))
    join x = Comp1 (fmap counit (uncomp x))
    a >>= f = Comp1 (\s -> let Prod (r,s1) = (unComp1 a) s 
                           in unComp1 (f r) s1)
