module Rules where
import Lamping
import Data.Map ((!))
import qualified Data.Set as Set
import Diagrams.Prelude hiding (N, LG)
import Diagrams.Backend.SVG.CmdLine


rule1a' :: LG -> N -> ([Int], LG)
rule1a' lg@(LG _ n e _) nd@(LGA va lbd vd) = case n ! (e ! lbd) of
  l@(LGL p var vb) -> ([va, (e ! va), vd, (e ! vd), lbd, p, vb, (e ! vb), vc,
                        (e ! vc)], if vc == (e ! vb) then lg' else lg6)
    where (vc, avar) = case n ! (e ! var) of
            lgv@(LGV varc _) -> (varc, lgv)
            _ -> error "Nonvariable var link!"
          lg2 = deleteNode lg nd
          lg3 = deleteNode lg2 l
          lg4 = deleteNode lg3 avar
          lg5 = addEdge lg4 (e ! vc) (e ! vd)
          lg6 = addEdge lg5 (e ! vb) (e ! va)
          lg' = addEdge lg4 (e ! vd) (e ! va)
  _ -> ([], lg)
rule1a' lg _ = ([], lg)
rule1a :: LRule
rule1a = LR "I.a" rule1a'

rule1b' :: LG -> N -> ([Int], LG)
rule1b' lg@(LG _ n e _) nd@(LGA va rbz vd) = case n ! (e ! rbz) of
  (LGRB lbd rbzo 0) -> case n ! (e ! lbd) of
    l@(LGL p var vb) -> ([va, (e ! va), vd, (e ! vd), lbd, p, vb, (e ! vb), vc,
                          (e ! vc), rbz, rbzo], if vc == (e ! vb) then lg'3 else lg10)
      where (vc, avar) = case n ! (e ! var) of
              lgv@(LGV varc _) -> (varc, lgv)
              _ -> error "Nonvariable var link!"
            lg2 = deleteNode lg nd
            lg3 = deleteNode lg2 l
            lg4 = deleteNode lg3 avar
            ([nrbi, nrbo], lg5) = nextVertices lg4 2
            lg6 = addNode lg5 (LGRB nrbi nrbo 0) 
            lg7 = addEdge lg6 (e ! vc) nrbi
            lg8 = addEdge lg7 (e ! vd) nrbo
            lg9 = addEdge lg8 (e ! vb) lbd
            lg10 = addEdge lg9 (e ! va) rbzo
            lg' = addEdge lg6 (e ! va) rbzo
            lg'2 = addEdge lg' lbd nrbi
            lg'3 = addEdge lg'2 nrbo (e ! vd)
    _ -> ([], lg)
  _ -> ([], lg)
rule1b' lg _ = ([], lg)
rule1b :: LRule
rule1b = LR "I.b" rule1b'

rule2a' :: LG -> N -> ([Int], LG)
rule2a' lg@(LG _ n e _) (LGCB va lbd cblvl) = case n ! (e ! lbd) of
   (LGL p var vb) -> ([va, (e ! va), vb, (e ! vb), lbd, p, vc, (e ! vc)],
                      if vc == (e ! vb) then lg'4 else lg12)
    where vc = case n ! (e ! var) of
            (LGV varc _) -> varc
            _ -> error "Nonvariable var link!"
          lg2 = deleteEdge lg va (e ! va)
          lg3 = deleteEdge lg2 lbd p
          lg4 = deleteEdge lg3 vb (e ! vb)
          lg5 = deleteEdge lg4 vc (e ! vc)
          ([ncbi, ncbo], lg6) = nextVertices lg5 2
          lg7 = addNode lg6 (LGCB ncbi ncbo cblvl) 
          lg8 = addEdge lg7 (e ! vc) ncbo
          lg9 = addEdge lg8 vc ncbi
          lg10 = addEdge lg9 (e ! va) p
          lg11 = addEdge lg10 (e ! vb) lbd
          lg12 = addEdge lg11 va vb
          lg' = addEdge lg7 ncbi vc
          lg'2 = addEdge lg' ncbo lbd
          lg'3 = addEdge lg'2 va vb
          lg'4 = addEdge lg'3 p (e ! va)
   _ -> ([], lg)
rule2a' lg _ = ([], lg)
rule2a :: LRule
rule2a = LR "II.a" rule2a'

rule2b' :: LG -> N -> ([Int], LG)
rule2b' lg@(LG _ n e _) (LGRB lbd va rblvl) = case n ! (e ! lbd) of
   (LGL p var vb) -> if rblvl > 0 then
                       ([va, (e ! va), vb, (e ! vb), lbd, p, vc, (e ! vc)],
                        if vc == (e ! vb) then lg'4 else lg12)
                     else ([], lg)
     where vc = case n ! (e ! var) of
             (LGV varc _) -> varc
             _ -> error "Nonvariable var link!"
           lg2 = deleteEdge lg va (e ! va)
           lg3 = deleteEdge lg2 lbd p
           lg4 = deleteEdge lg3 vb (e ! vb)
           lg5 = deleteEdge lg4 vc (e ! vc)
           ([nrbi, nrbo], lg6) = nextVertices lg5 2
           lg7 = addNode lg6 (LGCB nrbi nrbo rblvl) 
           lg8 = addEdge lg7 (e ! vc) nrbi
           lg9 = addEdge lg8 vc nrbo
           lg10 = addEdge lg9 (e ! va) p
           lg11 = addEdge lg10 (e ! vb) lbd
           lg12 = addEdge lg11 va vb
           lg' = addEdge lg7 nrbo vc
           lg'2 = addEdge lg' nrbi lbd
           lg'3 = addEdge lg'2 va vb
           lg'4 = addEdge lg'3 p (e ! va)
   _ -> ([], lg)
rule2b' lg _ = ([], lg)
rule2b :: LRule
rule2b = LR "II.b" rule2b'

rule2c' :: LG -> N -> ([Int], LG)
rule2c' lg@(LG _ n e _) (LGF lbd va vb flvl) = case n ! (e ! lbd) of
   (LGL p var vc) -> ([va, (e ! va), vb, (e ! vb), lbd, p, vc, (e ! vc), vd, e ! vd],
                      if vd == (e ! vc) then lg' else lg21)
    where vd = case n ! (e ! var) of
            (LGV vard _) -> vard
            _ -> error "Nonvariable var link!"
          lg2 = deleteEdge lg va (e ! va)
          lg3 = deleteEdge lg2 lbd p
          lg4 = deleteEdge lg3 vb (e ! vb)
          lg5 = deleteEdge lg4 vc (e ! vc)
          lg6 = deleteEdge lg5 vd (e ! vd)
          ([nfo, nfs, nfz], lg7) = nextVertices lg6 3
          lg8 = addNode lg7 (LGF nfo nfs nfz flvl) 
          ([nvp, nvv], lg9) = nextVertices lg8 2
          lg10 = addNode lg9 (LGV nvp nvv)
          ([nlp, nlv, nlb], lg11) = nextVertices lg10 3
          lg12 = addNode lg11 (LGL nlp nlv nlb)
          lg13 = addEdge lg12 (e ! vb) nlp
          lg14 = addEdge lg13 (e ! va) p
          lg15 = addEdge lg14 nlb vb
          lg16 = addEdge lg15 va vc
          lg17 = addEdge lg16 nfs vd
          lg18 = addEdge lg17 nfz nvp
          lg19 = addEdge lg18 nvv nlv
          lg20 = addEdge lg19 lbd (e ! vc)
          lg21 = addEdge lg20 nfo (e ! vd)
          lg' = addEdge lg18 lbd nfo
   _ -> ([], lg)
rule2c' lg _ = ([], lg)
rule2c :: LRule
rule2c = LR "II.c" rule2c'

rule3a' :: LG -> N -> ([Int], LG)
rule3a' lg@(LG _ n e _) br@(LGB va lbd) = case n ! (e ! lbd) of
   (LGL p _ vb) -> ([va, (e ! va), vb, (e ! vb), lbd, p], lg6)
    where lg2 = deleteNode lg br
          ([nbo, nbi], lg3) = nextVertices lg2 2
          lg4 = addNode lg3 (LGCB nbi nbo 0)
          lg5 = addEdge lg4 (e ! va) nbi
          lg6 = addEdge lg5 p nbo
   _ -> ([], lg)
rule3a' lg _ = ([], lg)
rule3a :: LRule
rule3a = LR "III.a" rule3a'



diagramRules' :: LG -> [(LRule, Int)] -> Double -> Int -> Int -> Diagram B
diagramRules' lg [] scl cnt rws = diagramGraphWithHeading lg Set.empty scl "Done" 
  # translateX ((fromIntegral (cnt `mod` rws)) * 30.5 * scl / 30.0) 
  # translateY ((fromIntegral (cnt `div` rws)) * (-32) * scl / 30.0)
diagramRules' lg ((lr, lrl):lrs) scl cnt rws =
  (diagramGraphWithHeading lg (Set.fromList rls) scl ("Applying rule " ++ lrname lr))
  # translateX ((fromIntegral (cnt `mod` rws)) * 30.5 * scl / 30.0) 
  # translateY ((fromIntegral (cnt `div` rws)) * (-32) * scl / 30.0) <>
  (diagramRules' lg' lrs scl (cnt + 1) rws)
    where (rls, lg') = applyRule lg lr lrl

diagramRules :: LG -> [(LRule, Int)] -> Double -> Int -> Diagram B
diagramRules lg rl scl rws = diagramRules' lg rl scl 0 rws  
