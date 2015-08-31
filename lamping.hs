module Lamping where
import LCParser
import qualified Data.Map as Map
import Data.Map ((!))
import qualified Data.Set as Set
import qualified Data.List as List
import Diagrams.Prelude hiding (N, LG)
import Diagrams.Backend.SVG.CmdLine



  -- Lamping graph node type
data N
  = LGA Int Int Int
  | LGL Int Int Int
  | LGV Int Int
  | LGF Int Int Int Int -- point, star, zero, level
  | LGB Int Int
  | LGRB Int Int Int
  | LGCB Int Int Int
  | LGR Int
  | LGN Int
  deriving (Eq, Ord, Show)

instance IsName N

isLambda :: N -> Bool
isLambda (LGL _ _ _) = True
isLambda _ = False

-- Lamping graph type
data LG
  = LG { heapSize :: Int,
         nodes :: (Map.Map Int N),
         edges :: (Map.Map Int Int),
         frees :: [Int] }

getNode :: LG -> Int -> N
getNode (LG _ n _ _) i = n ! i

foldN :: (Int -> a -> a) -> N -> a -> a
foldN f (LGA e1 e2 e3) a = f e3 (f e2 (f e1 a))
foldN f (LGL e1 e2 e3) a = f e3 (f e2 (f e1 a))
foldN f (LGF e1 e2 e3 _) a = f e3 (f e2 (f e1 a))
foldN f (LGV e1 e2) a = f e2 (f e1 a)
foldN f (LGB e1 e2) a = f e2 (f e1 a)
foldN f (LGRB e1 e2 _) a = f e2 (f e1 a)
foldN f (LGCB e1 e2 _) a = f e2 (f e1 a)
foldN f (LGR e1) a = f e1 a
foldN f (LGN e1) a = f e1 a

emptyLG :: LG
emptyLG = LG 0 Map.empty Map.empty []

nextVertex :: LG -> (Int, LG)
nextVertex (LG sz n e (h:t)) = (h, LG sz n e t)
nextVertex (LG sz n e []) = (sz, LG (sz + 1) n e [])

nextVertices :: LG -> Int -> ([Int], LG)
nextVertices l 0 = ([],l)
nextVertices l n = (nv:rest,l'')
  where (rest,l'') = nextVertices l' (n - 1)
        (nv, l') = nextVertex l

deleteVertex :: LG -> Int -> LG
deleteVertex (LG sz n e f) i = LG sz (Map.delete i n) e (i:f)

addEdge :: LG -> Int -> Int -> LG
addEdge (LG sz n e f) v1 v2 = LG sz n ne f
  where e' = Map.insert v1 v2 e
        ne = Map.insert v2 v1 e'

deleteEdge :: LG -> Int -> Int -> LG
deleteEdge (LG sz n e f) v1 v2 = LG sz n e'' f
  where e' = Map.delete v1 e
        e'' = Map.delete v2 e'

addNode :: LG -> N -> LG
addNode (LG sz n e f) nn = (LG sz n' e f)
  where n' = foldN (\i m -> Map.insert i nn m) nn n 

deleteNode :: LG -> N -> LG
deleteNode lg nd = (LG (heapSize lg') (nodes lg') e' (frees lg'))
  where lg' = foldN (\i lgi -> deleteVertex lgi i) nd lg
        e' = foldN (\i ed -> if Map.member i ed then
                               Map.delete i (Map.delete (ed ! i) ed)
                             else ed) nd e
        e = edges lg'

makeGraph' :: Exp -> LG -> (Map.Map Int Int) -> Int -> (Map.Map Int Int, LG)
makeGraph' (A e1 e2) l m p = (m3, l6)
  where ([p', f, a], l2) = nextVertices l 3
        l3 = addEdge l2 p p'
        n = LGA p' f a
        l4 = addNode l3 n
        (m2, l5) = makeGraph' e1 l4 m f
        (m3, l6) = makeGraph' e2 l5 m2 a
makeGraph' (L _ bdy) l m p = (Map.mapKeys (\a -> a - 1) (Map.delete 0 m''), l5)
  where ([p', f, v], l2) = nextVertices l 3
        l3 = addEdge l2 p p'
        n = LGL p' v f
        l4 = addNode l3 n
        m' = Map.insert 0 v (Map.mapKeys (\a -> a + 1) m)
        (m'',l5) = makeGraph' bdy l4 m' f
makeGraph' (F i) l m p = (m', l')
  where var = m ! i
        (m', l') = if isLambda $ getNode l var then (m3, l5) else (m, l11)
        ([p', v], l2) = nextVertices l 2
        n = LGV p' v
        l3 = addNode l2 n
        l4 = addEdge l3 p p'
        l5 = addEdge l4 v var
        m2 = Map.delete i m
        m3 = Map.insert i p' m2
        ([f, st, z], l6) = nextVertices l 3
        n2 = LGF f st z 0
        l7 = addNode l6 n2
        os = (edges l) ! var
        l8 = deleteEdge l7 os var
        l9 = addEdge l8 os st
        l10 = addEdge l9 p z
        l11 = addEdge l10 var f

addVoids :: LG -> LG
addVoids l@(LG _ n e _) = Prelude.foldl addVoid l
                          (List.nub $ Prelude.filter isVoid (Map.elems n)) 
  where isVoid (LGL _ v _) = case Map.lookup v e of
          Nothing -> True
          Just _ -> False
        isVoid _ = False
        addVoid lg (LGL _ lv _) = lg6
          where ([nv, vv, vpv],lg2) = nextVertices lg 3
                lg3 = addNode lg2 (LGN nv)
                lg4 = addNode lg3 (LGV vpv vv)
                lg5 = addEdge lg4 lv vv
                lg6 = addEdge lg5 vpv nv
        addVoid lg _ = lg

bracketFans' :: LG -> Int -> LG
bracketFans' lg@(LG _ n e _) i = case (n ! i) of
  (LGF out str zro _) -> if i == out then lg14 else lg
    where ([rbsi, rbso, rbzi, rbzo, ubi, ubo], lg2) = nextVertices lg 6
          lg3 = addNode lg2 (LGRB rbsi rbso 0) 
          lg4 = addNode lg3 (LGRB rbzi rbzo 0)
          lg5 = addNode lg4 (LGB ubi ubo)
          lg6 = deleteEdge lg5 str (e ! str)
          lg7 = addEdge lg6 str rbsi
          lg8 = addEdge lg7 rbso (e ! str)
          lg9 = deleteEdge lg8 zro (e ! zro)
          lg10 = addEdge lg9 zro rbzi
          lg11 = addEdge lg10 rbzo (e ! zro)
          lg12 = deleteEdge lg11 out (e ! out)
          lg13 = addEdge lg12 out ubi
          lg14 = addEdge lg13 ubo (e ! out)
  _ -> lg

bracketFans :: LG -> LG
bracketFans l@(LG _ n _ _) = foldl bracketFans' l (map fst $ Map.toList n)
  
makeGraph :: Program -> String -> LG
makeGraph p s = bracketFans $ addVoids $ snd $ makeGraph' e l' Map.empty i
  where (i, l) = nextVertex emptyLG
        n = LGR i
        l' = addNode l n
        e' = Prelude.lookup s (getEnv p)
        e = case e' of
          Just v -> v
          Nothing -> error "!"
          
nameLetters :: [Char]
nameLetters = ['x' .. 'z'] ++ ['w'] ++ ['a' .. 'v']

getName :: Int -> String
getName 0 = ""
getName n = nameLetters !! ((n - 1) `mod` len) : getName ((n - 1) `div` len)
  where len = length nameLetters

text' :: String -> Diagram B
text' st = bbox (fromIntegral $ length st) <> text st # font "sans-serif" 
  where bbox nd = phantom $ (square 0.8 # scaleX (0.57 * nd) :: Diagram B)

diagramGraph' :: LG -> Int -> Map.Map Int String -> Set.Set N ->
                 (Map.Map Int String, Set.Set N, Diagram B)
diagramGraph' lg@(LG _ n e _) v m s = if Set.member (n ! v) s then (m, s, mempty)
                                      else (m', s'', d)
  where s' = Set.insert (n ! v) s
        nsep = 0.8
        lambdad = (fromVertices [p2 (-0.2, 0.33),p2 (0.25, -0.3)] <> 
                  fromVertices [p2 (0,  0.05),p2 (-0.25, -0.3)] <> 
                  arc (direction $ r2 (0.7, 0.5)) (0.35 @@ turn) # scale 0.1
                  # translate (r2 (-0.28, 0.27))) # lwL (2/30)
        textl st = (lambdad ||| text' st) # centerXY
        rootd = ((circle 0.35 :: Diagram B) <>
                 fromVertices [p2 (0, 0.25),p2 (0, -0.35)] <> 
                 fromVertices [p2 (-0.244, 0.25),p2 (0.244, 0.25)]) # lwL (1.5/30)
        voidd = ((circle 0.35 :: Diagram B) <>
                 fromVertices [p2 (0.2, 0.2),p2 (-0.2, -0.2)] <> 
                 fromVertices [p2 (-0.2, 0.2),p2 (0.2, -0.2)]) # lwL (1.5/30)
        cbd = ((fromVertices [p2 (0.2, -0.3), p2 (-0.2, -0.3)] <>
               fromVertices [p2 (0.2, -0.3),p2 (0.2, 0.35)] <> 
               fromVertices [p2 (-0.2, -0.3),p2 (-0.2, 0.35)]) :: Diagram B)
              # lwL (2/30)
        bd = ((arc (direction $ r2 (-0.5, 0.0)) (0.5 @@ turn) # scale 0.2
                  # translate (r2 (0.0, -0.1))) # lwL (2/30) <>
                 (fromVertices [p2 (0.2, -0.1),p2 (0.2, 0.35)] <> 
                 fromVertices [p2 (-0.2, -0.1),p2 (-0.2, 0.35)]) :: Diagram B)
             # lwL (2/30)
        rbd = bd <> (fromVertices [p2 (-0.2, 0.1),p2 (0.2, 0.1)] # lwL (2/30))
              # lineCap LineCapButt
        (m', s'', d) = case n ! v of
          nd@(LGA p f a) -> (m3, s4,
                             ((text "" # named p #
                              translate (rotateBy 0 (r2 (0, 0.5)))) <>
                             (text "" # named f #
                              translate (rotateBy 0.3333333 (r2 (0, 0.5)))) <>
                             (text "" # named a #
                              translate (rotateBy 0.6666667 (r2 (0, 0.5)))) <>
                             (text' "@" # named nd === strutY nsep ===
                                 (fd ||| strutX nsep ||| ad) # centerXY))
                             # fc (sRGB 1.0 0.75 0.75))
            where (m2, s3, fd) = diagramGraph' lg (e ! f) m s'
                  (m3, s4, ad) = diagramGraph' lg (e ! a) m2 s3
          nd@(LGL p var bdy) -> (m3, s5,
                                 ((text "" # named p #
                                  translate (rotateBy 0 (r2 (0, 0.5)))) <>
                                 (text "" # named bdy #
                                  translate (rotateBy 0.5 (r2 (0, 0.5)))) <>
                                 (textl nm # named nd === strutY nsep === bdyd))
                                 # fc (sRGB 0.75 1.0 0.75) # lc (sRGB 0.75 1.0 0.75))
            where (m3, s5, bdyd) = diagramGraph' lg (e ! bdy) m2 s'
                  m2 = Map.insert (e ! var) nm m
                  nm = getName $ length m2
          nd@(LGV p lbd) -> (m, s', ((text "" # named p #
                                     translate (rotateBy 0 (r2 (0, 0.5)))) <>
                                    (text' (m ! lbd) # named nd))
                                    # fc (sRGB 0.75 0.75 1.0))
          nd@(LGF out st zro lvl) -> if out == v then
                                (m3, s7, ((text "" # named out #
                                          translate (rotateBy 0 (r2 (0, 0.6)))) <>
                                         (text "" # named st #
                                          translate (rotateBy (1/3)(r2 (0, 0.6))))<>
                                         (text "" # named zro #
                                          translate (rotateBy (2/3)(r2 (0, 0.6)))) <>
                                         (text (show lvl) # scale 0.6
                                         # translate (r2 (0.49, 0.15)) <>
                                         text "*" # scale 0.5
                                         # translate (r2 (-0.46, -0.35)) <>
                                         text "0" # scale 0.5
                                         # translate (r2 (0.46, -0.35)) <>
                                         (rtri # named nd # fcA transparent ===
                                          strutY nsep ===
                                         (std ||| strutX nsep ||| zrod) # centerXY)))
                                         # fc (sRGB 1.0 0.5 1.0) #
                                         lc (sRGB 1.0 0.5 1.0))
                         else (m4, s8, ((text "" # named out #
                                        translate (rotateBy 0 (r2 (0, -0.6)))) <>
                                       (text "" # named st #
                                        translate (rotateBy (2/3)(r2 (0, -0.6))))<>
                                       (text "" # named zro #
                                        translate (rotateBy (1/3)(r2 (0, -0.6)))) <>
                                       (text (show lvl) # scale 0.6
                                        # translate (r2 (0.49, -0.35)) <>
                                        text "*" # scale 0.5
                                        # translate (r2 (-0.46, 0.15)) <>
                                        text "0" # scale 0.5
                                        # translate (r2 (0.46, 0.15)) <>
                                        (utri # named nd # fcA transparent ===
                                         strutY nsep === outd)))
                                       # fc (sRGB 1.0 0.5 1.0)
                                       # lc (sRGB 1.0 0.5 1.0))
            where (m2, s6, std) = diagramGraph' lg (e ! st) m s'
                  (m3, s7, zrod) = diagramGraph' lg (e ! zro) m2 s6
                  rtri = regPoly 3 0.8 # lwL (1/30)
                  (m4, s8, outd) = diagramGraph' lg (e ! out) m s'
                  utri = rotateBy 0.5 rtri
          nd@(LGR g) -> (m2, s9, ((text "" # named g #
                                  translate (rotateBy 0.5 (r2 (0, 0.5)))) <>
                                 (rootd # named nd === strutY nsep === gd))
                                 # lc (sRGB 0.75 0.75 0.75))
            where (m2, s9, gd) = diagramGraph' lg (e ! g) m s'
          nd@(LGN g) -> (m2, s9, ((text "" # named g #
                                  translate (rotateBy (-0.25) (r2 (0, 0.5)))) <>
                                 (voidd # named nd === strutY nsep === gd))
                                 # lc (sRGB 1.0 1.0 0.5))
            where (m2, s9, gd) = diagramGraph' lg (e ! g) m s'
          nd@(LGB p b) -> (m2, s9, ((text "0" # scale 0.6
                                        # translate (r2 (0.49, -0.35))) <>
                                       (text "" # named inp #
                                        translate (rotateBy 0 (r2 (0, 0.5)))) <>
                                       (text "" # named outp #
                                        translate (rotateBy 0.5 (r2 (0, 0.5)))) <>
                                       (bdf # named nd === strutY nsep === gd))
                                       # fc (sRGB 0.5 1 1) # lc (sRGB 0.5 1 1))
            where (m2, s9, gd) = diagramGraph' lg (e ! outp) m s'
                  (inp, outp) = if v == p then (p, b) else (b, p)
                  bdf = if v == p then bd else rotateBy 0.5 bd
          nd@(LGCB p b lvl) -> (m2, s9, ((text (show lvl) # scale 0.6
                                         # translate (r2 (0.49, -0.35))) <>
                                         (text "" # named inp #
                                          translate (rotateBy 0 (r2 (0, 0.5)))) <>
                                         (text "" # named outp #
                                          translate (rotateBy 0.5 (r2 (0, 0.5)))) <>
                                         (cbdf # named nd === strutY nsep === gd))
                                        # fc (sRGB 0.5 1 1) # lc (sRGB 0.5 1 1))
            where (m2, s9, gd) = diagramGraph' lg (e ! outp) m s'
                  (inp, outp) = if v == p then (p, b) else (b, p)
                  cbdf = if v == p then cbd else rotateBy 0.5 cbd
          nd@(LGRB p b lvl) -> (m2, s9, ((text (show lvl) # scale 0.6
                                         # translate (r2 (0.49, -0.35))) <>
                                         (text "" # named inp #
                                          translate (rotateBy 0 (r2 (0, 0.5)))) <>
                                         (text "" # named outp #
                                          translate (rotateBy 0.5 (r2 (0, 0.5)))) <>
                                         (rbdf # named nd === strutY nsep === gd))
                                        # fc (sRGB 0.5 1 1) # lc (sRGB 0.5 1 1))
            where (m2, s9, gd) = diagramGraph' lg (e ! outp) m s'
                  (inp, outp) = if v == p then (p, b) else (b, p)
                  rbdf = if v == p then rbd else rotateBy 0.5 rbd
      

          
thd :: (a, b, c) -> c
thd (_, _, v) = v

withoutConnectedVoids :: LG -> Int -> Set.Set Int -> Set.Set Int ->
                         (Set.Set Int, Set.Set Int)
withoutConnectedVoids lg@(LG _ n e _) i s pst = if (Set.member i pst) then (s, pst)
                                                else case (n ! i) of
  (LGN bdy) -> withoutConnectedVoids lg (e ! bdy) (Set.delete i s) (Set.insert i pst) 
  (LGA prnt func arg) -> withoutConnectedVoids lg (e ! ot2) s' pst'
    where (s', pst') = withoutConnectedVoids lg (e ! ot1) s
                       (foldl (\ps int -> Set.insert int ps) pst [prnt, func, arg])
          (ot1, ot2) = if ( i == prnt ) then (func, arg) else
                         if ( i == func ) then (prnt, arg) else (prnt, func)
  (LGF out str zro _) -> withoutConnectedVoids lg (e ! ot2) s' pst'
    where (s', pst') = withoutConnectedVoids lg (e ! ot1) s
                       (foldl (\ps int -> Set.insert int ps) pst [out, str, zro])
          (ot1, ot2) = if ( i == out ) then (str, zro) else
                         if ( i == str ) then (out, zro) else (out, str)
  (LGL prnt _ bdy) -> withoutConnectedVoids lg (e ! ot) s
                      (foldl (\ps int -> Set.insert int ps) pst [prnt, bdy])
    where ot = if ( i == prnt ) then bdy else prnt
  (LGB prnt bdy) -> withoutConnectedVoids lg (e ! ot) s
                      (foldl (\ps int -> Set.insert int ps) pst [prnt, bdy])
    where ot = if ( i == prnt ) then bdy else prnt
  (LGRB prnt bdy _) -> withoutConnectedVoids lg (e ! ot) s
                       (foldl (\ps int -> Set.insert int ps) pst [prnt, bdy])
    where ot = if ( i == prnt ) then bdy else prnt
  (LGCB prnt bdy _) -> withoutConnectedVoids lg (e ! ot) s
                       (foldl (\ps int -> Set.insert int ps) pst [prnt, bdy])
    where ot = if ( i == prnt ) then bdy else prnt
  _ -> (s, Set.insert i pst)

removeDuplicateVoids :: LG -> Set.Set Int -> [Int]
removeDuplicateVoids lg@(LG _ n _ _) s = if (Set.null s) then [] else
                                           case (n ! (Set.elemAt 0 s)) of
  (LGN bdy) -> bdy:(removeDuplicateVoids lg s')
    where (s', _) = withoutConnectedVoids lg bdy s Set.empty
  (LGR bdy) -> bdy:(removeDuplicateVoids lg (Set.delete (Set.elemAt 0 s) s))
  _ -> removeDuplicateVoids lg (Set.delete (Set.elemAt 0 s) s)

isParent' :: LG -> Int -> Set.Set Int -> (Maybe Bool, Set.Set Int)
isParent' lg@(LG _ n e _) i s = if (Set.member i s) then (Nothing, s) else
                                  case (n ! i) of
  (LGA prnt _ _) -> if prnt == i then (Just True, s) else (Just False, s)
  (LGF out str zro _) -> if out /= i then
                           isParent' lg (e ! out) (Set.insert i s) else case mb of
    Just b -> (Just b, s'')
    Nothing -> isParent' lg (e ! ot2) s''
    where (mb, s'') = isParent' lg (e ! ot1) s'
          s' = Set.insert i s
          (ot1, ot2) = (str, zro)
  (LGB prnt bdy) -> isParent' lg (e ! ot) (Set.insert i s)
    where ot = if i == prnt then bdy else prnt
  (LGRB prnt bdy _) -> isParent' lg (e ! ot) (Set.insert i s)
    where ot = if i == prnt then bdy else prnt
  (LGCB prnt bdy _) -> isParent' lg (e ! ot) (Set.insert i s)
    where ot = if i == prnt then bdy else prnt
  (LGL prnt _ _) -> if prnt == i then (Just True, s) else (Just False, s)
  (LGN bdy) -> isParent' lg (e ! bdy) (Set.insert i s)
  (LGV _ _) -> (Just True, s)             
  (LGR _) -> (Just True, s)             

isParent :: LG -> Int -> Bool
isParent lg i = case fst $ isParent' lg i Set.empty of
  Nothing -> True
  Just b -> b
             
graphList :: LG -> [Int]
graphList lg@(LG _ n _ _) = (removeDuplicateVoids lg
                             (Set.fromList $ filter (isParent lg) $
                              map fst $ Map.toList n))

diagramGraph'' :: LG -> Diagram B
diagramGraph'' l = ans
  where (_, _, ans) = foldl folddg (ms, ss, ds) (tail $ graphList l)
        (ms, ss, ds) = diagramGraph' l (head $ graphList l) Map.empty Set.empty
        folddg (m, s, dia) ind = (m', s', dia ||| strutX 1 ||| lgd)
          where (m', s', lgd) = diagramGraph' l ind m s
        


getOneSide :: Map.Map Int Int -> [Int]
getOneSide m = if (Map.null m) then [] else fe:(getOneSide nm)
  where (fe, fer) = Map.elemAt 0 m
        nm = Map.delete fe (Map.delete fer m)

delambda :: LG -> [Int] -> [Int]
delambda _ [] = []
delambda lg@(LG _ n _ _) (i:t) = ans
  where t' = delambda lg t
        ans = case (n ! i) of
          (LGL _ v _) -> if i == v then t' else (i:t')
          (LGV _ l) -> if i == l then t' else (i:t')
          _ -> i:t'

lookupOE :: Diagram B -> Int -> Subdiagram B V2 Double Any
lookupOE dg i = case lookupName i dg of
  Nothing -> error ("No vertex " ++ show i)
  Just sdg -> sdg

lookupOEN :: Diagram B -> N -> Subdiagram B V2 Double Any
lookupOEN dg n = case lookupName n dg of
  Nothing -> error ("No vertex node " ++ show n)
  Just sdg -> sdg


diagramGraph :: LG -> Set.Set Int -> Double -> Diagram B
diagramGraph lg@(LG _ n e _) es scl =  foldl foldlg dgne (delambda lg $ getOneSide e)
                                   # lineCap LineCapRound # scale scl # frame scl
                                   # bg black
  where dgne = diagramGraph'' lg
        foldlg :: Diagram B -> Int -> Diagram B
        foldlg dia nm = place (bzc dia nm #
                               opacity (if Set.member nm es then 1.0 else 0.7) #
                               lwL (if Set.member nm es then (5/30) else (2/30))# 
                               lc (if Set.member nm es then (sRGB 0.2 1.0 0.2)
                                   else (sRGB 1.0 1.0 1.0)) #
                               (withEnvelope $ (text "" :: Diagram B)))
                        (location (lookupOE dia nm) ) <> dia 
        bzc :: Diagram B -> Int -> Diagram B
        bzc dia nm = fromSegments [bezier3
                                   ((location (lookupOE dia ((nm))))
                                    .-. (location (lookupOEN dia (n ! (nm)))))
                                   ((endp dia nm ) ^+^
                                    ((location (lookupOE dia ((e ! nm))))
                                    .-. (location (lookupOEN dia (n ! (e ! nm))))))
                                   (endp dia nm)]
        endp dia nm = ((location (lookupOE dia (e ! nm)))
                       .-. (location (lookupOE dia nm)))



graphString' :: LG -> Int -> Set.Set N -> String
graphString' lg@(LG sz n _ _) v s =
  if v >= sz then "" else if (not $ Map.member v n) || Set.member (n ! v) s then
    graphString' lg (v + 1) s
  else
    case (n ! v) of
      (LGA e1 e2 e3) -> "Application " ++ show e1
                        ++ ", " ++ show e2 
                        ++ ", " ++ show e3 
                        ++ "\n" ++ graphString' lg (v + 1) (Set.insert (n ! v) s)
      (LGL e1 e2 e3) -> "Lambda " ++ show e1  
                        ++ ", " ++ show e2
                        ++ ", " ++ show e3
                        ++ "\n" ++ graphString' lg (v + 1) (Set.insert (n ! v) s)
      (LGF e1 e2 e3 l) -> "Fan " ++ show e1 
                        ++ ", " ++ show e2
                        ++ ", " ++ show e3
                        ++ ", level " ++ show l
                        ++ "\n" ++ graphString' lg (v + 1) (Set.insert (n ! v) s)
      (LGV e1 e2) -> "Variable " ++ show e1 
                        ++ ", " ++ show e2
                        ++ "\n" ++ graphString' lg (v + 1) (Set.insert (n ! v) s)
      (LGB e1 e2) -> "Bracket " ++ show e1 
                        ++ ", " ++ show e2
                        ++ ", level 0"
                        ++ "\n" ++ graphString' lg (v + 1) (Set.insert (n ! v) s)
      (LGRB e1 e2 l) -> "R.Bracket " ++ show e1  
                        ++ ", " ++ show e2 
                        ++ ", level " ++ show l
                        ++ "\n" ++ graphString' lg (v + 1) (Set.insert (n ! v) s)
      (LGCB e1 e2 l) -> "C.Bracket " ++ show e1 
                        ++ ", " ++ show e2
                        ++ ", level " ++ show l
                        ++ "\n" ++ graphString' lg (v + 1) (Set.insert (n ! v) s)
      (LGR e1) -> "Root " ++ show e1  
                  ++ "\n" ++ graphString' lg (v + 1) (Set.insert (n ! v) s)
      (LGN e1) -> "Void " ++ show e1 
                  ++ "\n" ++ graphString' lg (v + 1) (Set.insert (n ! v) s)

edgeString :: LG -> String
edgeString (LG _ _ e _) = Map.foldlWithKey (\s et eo -> show et ++ " -> " ++ show eo ++ "\n" ++ s) "" e 
      
graphString :: LG -> String
graphString l = graphString' l 0 Set.empty ++ "\nEdges:\n" ++ edgeString l

data PC = PCEmpty
        | PC [Bool] PC PC

emptyPC :: PC
emptyPC = PC [] PCEmpty PCEmpty

bracket :: PC -> Int -> PC
bracket pc 0 = PC [] PCEmpty pc
bracket (PC d c pc) n = PC d c (bracket pc (n - 1))
bracket PCEmpty _ = error "Level too high in a bracket!"

unbracket :: PC -> Int -> PC
unbracket (PC _ _ pc) 0 = pc
unbracket (PC d c pc) n = PC d c (unbracket pc (n - 1))
unbracket PCEmpty _ = error "Level too high in a unbracket!"

cbracket :: PC -> Int -> PC
cbracket (PC d (PC cd cc cpc) pc) 0 = PC cd cpc (PC d cc pc)
cbracket (PC _ PCEmpty _) _ = error "No closure!"
cbracket (PC d c pc) n = PC d c (cbracket pc (n - 1))
cbracket PCEmpty _ = error "Level to high in a cbracket!"

cunbracket :: PC -> Int -> PC
cunbracket (PC d c (PC nd nc npc)) 0 = PC nd (PC d nc c) npc 
cunbracket (PC _ _ PCEmpty) _ = error "No next closure!"
cunbracket (PC d c pc) n = PC d c (cunbracket pc (n - 1))
cunbracket PCEmpty _ = error "Level to high in a cunbracket!"

fanin :: PC -> Int -> Bool -> PC
fanin (PC d c pc) 0 b = PC (b:d) c pc
fanin (PC d c pc) n b = PC d c (fanin pc (n - 1) b) 
fanin PCEmpty _ _ = error "Level too high in a fanin!"

fanout :: PC -> Int -> (Bool, PC)
fanout (PC (h:t) c pc) 0 = (h, PC t c pc)
fanout (PC [] _ _) _ = error "Out of directions in a fanout!"
fanout (PC d c pc) n = (a, PC d c cont)
  where (a, cont) = fanout pc (n - 1)
fanout PCEmpty _ = error "Level too high in a fanout!"

traverseGraph :: LG -> PC -> Int -> (PC -> Int -> a -> (Bool, a)) -> a -> (Bool, a)
traverseGraph lg@(LG _ n e _) pc i f acc = case (n ! i) of
  (LGA _ func arg) -> if ans then
                        (if ansf then (ansa, a2) else (ansf, a1))
                      else (ans, a)
    where (ans, a) = f pc i acc
          (ansf, a1) = traverseGraph lg pc (e ! func) f a
          (ansa, a2) = traverseGraph lg pc (e ! arg) f a1
  (LGL _ _ bdy) -> if ans then traverseGraph lg pc (e ! bdy) f a else (ans, a)
    where (ans, a) = f pc i acc
  (LGV _ _) -> f pc i acc
  (LGF out str zro lvl) -> if ans then
                             (if i == out then traverseGraph lg pc1 (e ! ot) f a
                              else traverseGraph lg pc2 (e ! out) f a)
                           else (ans, a)
    where (ans, a) = f pc i acc
          (isstr, pc1) = fanout pc lvl
          ot = if isstr then str else zro
          pc2 = fanin pc lvl (i == str)
  (LGR bdy) -> if ans then traverseGraph lg pc (e ! bdy) f a else (ans, a)
    where (ans, a) = f pc i acc
  (LGB cin cout) -> if ans then
                      (if i == cin then traverseGraph lg pc1 (e ! cout) f a
                       else traverseGraph lg pc2 (e ! cin) f a)
                    else (ans, a)
    where (ans, a) = f pc i acc
          pc1 = unbracket pc 0
          pc2 = bracket pc 0
  (LGRB cin cout lvl) -> if ans then
                           (if i == cin then traverseGraph lg pc1 (e ! cout) f a
                            else traverseGraph lg pc2 (e ! cin) f a)
                         else (ans, a)
    where (ans, a) = f pc i acc
          pc1 = unbracket pc lvl
          pc2 = bracket pc lvl
  (LGCB cin cout lvl) -> if ans then
                           (if i == cin then traverseGraph lg pc1 (e ! cout) f a
                            else traverseGraph lg pc2 (e ! cin) f a)
                         else (ans, a)
    where (ans, a) = f pc i acc
          pc1 = cunbracket pc lvl
          pc2 = cbracket pc lvl
  (LGN _) -> error "Null in traversal!"
          
data BP = TwoArgs (Map.Map Int Int) (Exp -> Exp -> Exp)
        | OneArg (Map.Map Int Int) (Exp -> Exp)
        | NoArgs Exp


getBPmap :: [BP] -> (Map.Map Int Int)
getBPmap ((TwoArgs m _):_) = m
getBPmap ((OneArg m _):_) = m
getBPmap ((NoArgs _):_) = error "getBPmap on NoArgs!"
getBPmap [] = Map.empty

applify :: [BP] -> [BP]
applify ((NoArgs e):(OneArg _ f):t) = applify ((NoArgs (f e)):t)
applify ((NoArgs e):(TwoArgs m f):t) = ((OneArg m (f e)):t)
applify l = l

buildProgram' :: LG -> PC -> Int -> [BP] -> (Bool, [BP])
buildProgram' (LG _ nds e _) _ i l = case (nds ! i) of
  (LGA _ _ _) -> (True, (TwoArgs (getBPmap l) (\f a -> A f a)):l) 
  (LGV _ lbd) -> (True, applify ((NoArgs fv):l))
    where fv = F ((getBPmap l) ! lbd)
  (LGL _ var _) -> case l of
    ((TwoArgs _ _):_) -> (True, (OneArg m' (\lbd -> L (getName (length m')) lbd)):l)
    ((OneArg _ f):t) -> (True, (OneArg m' (\lbd -> f (L (getName (length m')) lbd)):t))
    ((NoArgs _):_) -> error "snp"
    [] -> (True, [OneArg m' (\lbd -> L (getName 1) lbd)])
    where m' = Map.insert (e ! var) 0 (Map.map (\n -> (n + 1)) (getBPmap l))
  _ -> (True, l)        

buildProgram :: LG -> Exp
buildProgram lg = case snd $ traverseGraph lg emptyPC 0 (buildProgram' lg) [] of
  [NoArgs e] -> e
  _ -> error "Malformed buildProgram!"

data LRule = LR {
  lrname :: String,
  lrfunc :: LG -> N -> ([Int], LG)
}

applyRule :: LG -> LRule -> Int -> ([Int], LG)
applyRule lg r n = ans
  where ans = case traverseGraph lg emptyPC 0 (aprule lg) (n, [], lg) of
          (_, (_, l, lg')) -> (l, lg')
        aprule :: LG -> PC -> Int -> (Int, [Int], LG) -> (Bool, (Int, [Int], LG))
        aprule (LG _ nds _ _) _ i (n', _, _) = case (lrfunc r) lg (nds ! i) of
          ([], _) -> (True, (n', [], lg))
          (l, lg') -> if n' == 0 then (False, (0, l, lg')) else
                        (True, (n' - 1, [], lg))

diagramGraphWithHeading :: LG -> Set.Set Int -> Double -> String -> Diagram B
diagramGraphWithHeading lg s scl h =
  ((text' h # scale (scl / 30) # fc white # bg black <>
   square scl # scaleY (2 / 30) # fc black # lw 0)
   # translateY (scl * 15.5 / 30) <>
  (diagramGraph lg s 1 # sizedAs (square scl :: Diagram B) # centerXY <>
  square scl # fc black # lw 0))
  # frame (scl/60) # bg white

  

  
