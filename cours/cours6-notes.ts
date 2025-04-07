/* 

!   Foncteur : une fonction de type en type 


?   la classe Fonctor 
?   class Functor f where 
?       fmap    : (a->b ) -> ( f a -> f b )
?           ou  : (a -> b) -> f a -> f b 
?       g = (a->b)
?       ou f est un foncteur 
?       fmap g = ( f a -> f b)
?   
e.g : 
    kind a = *
    kind b = *
    kind  f a = *
    => kind f = * -> *  
?   y'a deux type de contextes fonctoriels : 
?   -   contexte  structurels : structure de données ou conteneurs
?   -   contexte comportementaux : contexte tout court  

e.g ; 
    Maybe :: * -> * => contexte structurel 
    [] :: * -> * => contexte comportemental 
    ?   data Base a = Base a | Base :: * -> *
!       Étape 1 : Instanciation
?           fmapBase :: (a->b)-> Base a -> Base b 
?           fmapBase  g (Base v)= Base (g v)
?               -- v est de type b 
?               -- (g v)

?           instance Functor Base where 
?               fmap  = fmapBase
!       Étape 2 : vérification des lois 
?           1-  loi d'identité : fmap id = id
*                   law_Functor_id :: (Functor f, Eq (f a)) => f a -> Bool
*                   law_Functor_id x = (fmap id x) == x
?           2-  loi de composition : fmap (f . g) = fmap f . fmap g
*                   law_Functor_comp :: (Functor f, Eq (f c)) => (b -> c) -> (a -> b) -> f a -> Bool
*                   law_Functor_comp h g x = fmap (h . g) x == (fmap h . fmap g) x
?   fonction de composition est défini par : (.) f g x = f (g x)

=> contexte avec plusieurs arguments, le dernier arguments est le plus important 
e.g :   data LPair a b = LPair a b
        data RPair b a = RPair b a
=> si on prend a comme étant un foncteur => faut rendre RPair comme étant un foncteur (plus que c'est le dernier )


!   Monoid : 

?   définiton algébrique d'un semi groupe 
!       Semi groupe : 
*       une fonction (<<loi>>) de composition interne (<>) sur les les éléments a 
?           (<>):: a-> a-> a
*       une loi (propriété) d'associativité : 
?       law_SemiGroup_assoc x y z = x <> (y<>z) == (x<>y) <> z

!       monoide :  
*       -   Semi groupe  :
*               -    (<>) : loi de compoosition interne 
*               -    a<>(b<>c) == (a<>b)<>c : loi d'associativité  
*       -   mempty  :   élément neutre (e.g Nil pour les listes)
*       -   <>      :   concanténation  

*   on rajoute aussi la loi de empty 

?   instanciation 
?       data List = 
?           Cons a List 
?           | Nil 
?       instance Semigroup (List a) where 
?           (<>) = concatList <=== concaténation des liste
?       instance Monoid (List a )
?           mempty =  Nil


!   Foldable 
?   

*/