data Tr a = Lf a | Br (Tr a) (Tr a)
    deriving Show

type Lt a = Tr (S, a)
type S = Int

label :: Tr a -> Lt a
label tr = snd (lab tr 0)
    where
        lab :: Tr a -> S -> (S, Lt a)
        lab (Lf contents) n = (n+1, Lf (n, contents))
        lab (Br l r) n0 = let (n1, l') = lab l n0 
                              (n2, r') = lab r n1
                           in (n2, Br l' r')

tr1 = Br (Lf 'a') (Br (Br (Lf 'b') (Lf 'a')) (Lf 'c'))