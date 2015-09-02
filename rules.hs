module Rules where
import Lamping
import Data.Map ((!))
import qualified Data.Set as Set
import Diagrams.Prelude hiding (N, LG)
import Diagrams.Backend.SVG.CmdLine


rule1a' :: LG -> N -> ([Int], LG)
rule1a' lg@(LG _ n e _) nd@(LGA va lbd vd) = case n ! (e ! lbd) of
  l@(LGL p var vb) -> if p /= (e ! lbd) then ([], lg) else
                        ([va, (e ! va), vd, (e ! vd), lbd, p, vb, (e ! vb), vc,
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
    l@(LGL p var vb) -> if p /= (e ! lbd) then ([], lg) else
                          ([va, (e ! va), vd, (e ! vd), lbd, p, vb, (e ! vb), vc,
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
   (LGL p var vb) -> if p /= (e ! lbd) then ([], lg) else
                       ([va, (e ! va), vb, (e ! vb), lbd, p, vc, (e ! vc)],
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
   (LGL p var vb) -> if p /= (e ! lbd) || rblvl == 0 then ([], lg) else
                       ([va, (e ! va), vb, (e ! vb), lbd, p, vc, (e ! vc)],
                        if vc == (e ! vb) then lg'4 else lg12)
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
   (LGL p var vc) -> if p /= (e ! lbd) then ([], lg) else
                       ([va, (e ! va), vb, (e ! vb), lbd, p, vc, (e ! vc), vd, e ! vd],
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
          lg' = addEdge lg19 lbd nfo
   _ -> ([], lg)
rule2c' lg _ = ([], lg)
rule2c :: LRule
rule2c = LR "II.c" rule2c'

rule3a' :: LG -> N -> ([Int], LG)
rule3a' lg@(LG _ n e _) br@(LGB va lbd) = case n ! (e ! lbd) of
   (LGL p _ vb) -> if p /= (e ! lbd) then ([], lg) else
                     ([va, (e ! va), vb, (e ! vb), lbd, p], lg6)
    where lg2 = deleteNode lg br
          ([nbo, nbi], lg3) = nextVertices lg2 2
          lg4 = addNode lg3 (LGCB nbi nbo 0)
          lg5 = addEdge lg4 (e ! va) nbi
          lg6 = addEdge lg5 p nbo
   _ -> ([], lg)
rule3a' lg _ = ([], lg)
rule3a :: LRule
rule3a = LR "III.a" rule3a'

rule4b' :: LG -> N -> ([Int], LG)
rule4b' lg@(LG _ n e _) (LGA va cb vc) = case n ! (e ! cb) of
   (LGCB vb cbo cblvl) -> if cb /= (e ! cbo) then ([], lg) else
                            ([va, (e ! va), cb, cbo, vb, (e ! vb),
                            vc, (e ! vc)], lg12)
    where lg2 = deleteEdge lg va (e ! va)
          lg3 = deleteEdge lg2 vb (e ! vb)
          lg4 = deleteEdge lg3 vc (e ! vc)
          lg5 = deleteEdge lg4 cb cbo
          ([ncbo, ncbi], lg6) = nextVertices lg5 2
          lg7 = addNode lg6 (LGCB ncbi ncbo cblvl)
          lg8 = addEdge lg7 (e ! va) cbo
          lg9 = addEdge lg8 vb va 
          lg10 = addEdge lg9 cb (e ! vb)
          lg11 = addEdge lg10 vc ncbi
          lg12 = addEdge lg11 ncbo (e ! vc)
   _ -> ([], lg)
rule4b' lg _ = ([], lg)
rule4b :: LRule
rule4b = LR "IV.b" rule4b'

rule4d' :: LG -> N -> ([Int], LG)
rule4d' lg@(LG _ n e _) (LGCB va appl cblvl) = case n ! (e ! appl) of
   (LGA p vb vc) -> if p /= (e ! appl) then ([], lg) else
                      ([va, (e ! va), p, appl, vb, (e ! vb),
                        vc, (e ! vc)], lg12)
    where lg2 = deleteEdge lg va (e ! va)
          lg3 = deleteEdge lg2 vb (e ! vb)
          lg4 = deleteEdge lg3 vc (e ! vc)
          lg5 = deleteEdge lg4 p appl
          ([ncbo, ncbi], lg6) = nextVertices lg5 2
          lg7 = addNode lg6 (LGCB ncbi ncbo cblvl)
          lg8 = addEdge lg7 (e ! va) p
          lg9 = addEdge lg8 vb va
          lg10 = addEdge lg9 appl (e ! vb)
          lg11 = addEdge lg10 vc ncbi
          lg12 = addEdge lg11 ncbo (e ! vc)
   _ -> ([], lg)
rule4d' lg _ = ([], lg)
rule4d :: LRule
rule4d = LR "IV.d" rule4d'

rule4e' :: LG -> N -> ([Int], LG)
rule4e' lg@(LG _ n e _) (LGA va fos vd) = case n ! (e ! fos) of
   (LGF foo vb vc flvl) -> if fos /= (e ! foo) then ([], lg) else
                            ([va, (e ! va), fos, foo, vb, (e ! vb), vc, (e ! vc),
                            vd, (e ! vd)], lg18)
    where lg2 = deleteEdge lg va (e ! va)
          lg3 = deleteEdge lg2 vb (e ! vb)
          lg4 = deleteEdge lg3 vc (e ! vc)
          lg5 = deleteEdge lg4 vd (e ! vd)
          lg6 = deleteEdge lg5 foo fos
          ([nap, naf, naa], lg7) = nextVertices lg6 3
          lg8 = addNode lg7 (LGA nap naf naa)
          ([nfo, nfs, nfz], lg9) = nextVertices lg8 3
          lg10 = addNode lg9 (LGF nfo nfs nfz flvl)
          lg11 = addEdge lg10 (e ! va) foo
          lg12 = addEdge lg11 vb va
          lg13 = addEdge lg12 fos (e ! vb)
          lg14 = addEdge lg13 vc nap
          lg15 = addEdge lg14 naf (e ! vc)
          lg16 = addEdge lg15 vd nfs
          lg17 = addEdge lg16 naa nfz
          lg18 = addEdge lg17 nfo (e ! vd)
          
   _ -> ([], lg)
rule4e' lg _ = ([], lg)
rule4e :: LRule
rule4e = LR "IV.e" rule4e'

rule5a' :: LG -> N -> ([Int], LG)
rule5a' lg@(LG _ n e _) (LGF fano va vb jlvl) = case n ! (e ! fano) of
   (LGF fani vc vd ilvl) -> if fani /= (e ! fano) || ilvl == jlvl then ([], lg) else
                              ([va, (e ! va), fani, fano, vb, (e ! vb), vc, (e ! vc),
                                vd, (e ! vd)], lg17)
    where lg2 = deleteEdge lg fani fano
          lg3 = deleteEdge lg2 va (e ! va)
          lg4 = deleteEdge lg3 vb (e ! vb)
          lg5 = deleteEdge lg4 vc (e ! vc)
          lg6 = deleteEdge lg5 vd (e ! vd)
          ([nfio, nfis, nfiz, nfoo, nfos, nfoz], lg7) = nextVertices lg6 6
          lg8 = addNode lg7 (LGF nfio nfis nfiz jlvl)
          lg9 = addNode lg8 (LGF nfoo nfos nfoz ilvl)
          lg10 = addEdge lg9 (e ! va) fani
          lg11 = addEdge lg10 (e ! vb) nfoo
          lg12 = addEdge lg11 vc va
          lg13 = addEdge lg12 vd nfis
          lg14 = addEdge lg13 vb nfos
          lg15 = addEdge lg14 nfoz nfiz
          lg16 = addEdge lg15 fano (e ! vc)
          lg17 = addEdge lg16 nfio (e ! vd)
   _ -> ([], lg)
rule5a' lg _ = ([], lg)
rule5a :: LRule
rule5a = LR "V.a" rule5a'

rule5b' :: LG -> N -> ([Int], LG)
rule5b' lg@(LG _ n e _) fni@(LGF fano va vb jlvl) = case n ! (e ! fano) of
   fno@(LGF fani vc vd ilvl) -> if fani /= (e ! fano) || ilvl /= jlvl then ([], lg) else
                                  ([va, (e ! va), fani, fano, vb, (e ! vb), vc, (e ! vc),
                                    vd, (e ! vd)], lg5)
    where lg2 = deleteNode lg fni
          lg3 = deleteNode lg2 fno
          lg4 = addEdge lg3 (e ! va) (e ! vc)
          lg5 = addEdge lg4 (e ! vb) (e ! vd)
   _ -> ([], lg)
rule5b' lg _ = ([], lg)
rule5b :: LRule
rule5b = LR "V.b" rule5b'

rule6a' :: LG -> N -> ([Int], LG)
rule6a' lg@(LG _ n e _) (LGB va fan) = case n ! (e ! fan) of
   fn@(LGF p vb vc flvl) -> if p /= (e ! fan) then ([], lg) else
                           ([va, (e ! va), p, fan, vb, (e ! vb),
                             vc, (e ! vc)], lg12)
    where lg2 = deleteNode lg fn
          lg3 = deleteEdge lg2 va (e ! va)
          ([nbo, nbi], lg4) = nextVertices lg3 2
          lg5 = addNode lg4 (LGB nbi nbo)
          ([nfo, nfs, nfz], lg6) = nextVertices lg5 3
          lg7 = addNode lg6 (LGF nfo nfs nfz (flvl + 1))
          lg8 = addEdge lg7 (e ! va) nfo
          lg9 = addEdge lg8 nfs va
          lg10 = addEdge lg9 fan (e ! vb)
          lg11 = addEdge lg10 nfz nbi
          lg12 = addEdge lg11 nbo (e ! vc)
   _ -> ([], lg)
rule6a' lg _ = ([], lg)
rule6a :: LRule
rule6a = LR "VI.a" rule6a'

rule6cd' :: LG -> N -> ([Int], LG)
rule6cd' lg@(LG _ n e _) fnd@(LGF rb va vb ilvl) = case n ! (e ! rb) of
  (LGRB rbo vc jlvl) -> if rb /= (e ! rbo) || ilvl == jlvl then ([], lg) else
                          ([va, (e ! va), rb, rbo, vb, (e ! vb), vc, (e ! vc)],
                           lg12)
    where lg2 = deleteNode lg fnd
          lg3 = deleteEdge lg2 vc (e ! vc)
          ([nfo, nfs, nfz], lg4) = nextVertices lg3 3
          lg5 = addNode lg4 (LGF nfo nfs nfz
                             (if ilvl > jlvl then ilvl - 1 else ilvl))
          ([nrbi, nrbo], lg6) = nextVertices lg5 2
          lg7 = addNode lg6 (LGRB nrbi nrbo jlvl)
          lg8 = addEdge lg7 (e ! va) rbo
          lg9 = addEdge lg8 (e ! vb) nrbi
          lg10 = addEdge lg9 nfs vc
          lg11 = addEdge lg10 nfz nrbo
          lg12 = addEdge lg11 nfo (e ! vc)
  _ -> ([], lg)
rule6cd' lg _ = ([], lg)
rule6cd :: LRule
rule6cd = LR "VI.cd" rule6cd'

rule6ef' :: LG -> N -> ([Int], LG)
rule6ef' lg@(LG _ n e _) fnd@(LGF cb va vb flvl) = case n ! (e ! cb) of
   (LGCB vc cbo cblvl) -> if cb /= (e ! cbo) || cblvl == flvl then ([], lg) else
                            ([va, (e ! va), cb, cbo, vb, (e ! vb), vc, (e ! vc)],
                             lg12)
    where lg2 = deleteNode lg fnd
          lg3 = deleteEdge lg2 vc (e ! vc)
          ([nfo, nfs, nfz], lg4) = nextVertices lg3 3
          lg5 = addNode lg4 (LGF nfo nfs nfz
                             (if flvl > cblvl then flvl + 1 else flvl))
          ([ncbi, ncbo], lg6) = nextVertices lg5 2
          lg7 = addNode lg6 (LGCB ncbi ncbo cblvl)
          lg8 = addEdge lg7 (e ! va) cbo
          lg9 = addEdge lg8 (e ! vb) ncbo
          lg10 = addEdge lg9 nfs vc
          lg11 = addEdge lg10 nfz ncbi
          lg12 = addEdge lg11 nfo (e ! vc)
          
   _ -> ([], lg)
rule6ef' lg _ = ([], lg)
rule6ef :: LRule
rule6ef = LR "VI.ef" rule6ef'

rule7c' :: LG -> N -> ([Int], LG)
rule7c' lg@(LG _ n e _) ocbnd@(LGCB va cb cblvl) = case n ! (e ! cb) of
   cbnd@(LGCB vb cbo cblvl2) -> if cb /= (e ! cbo) || cblvl /= cblvl2 then ([], lg)
                                else ([va, (e ! va), cb, cbo, vb, (e ! vb)],
                                   lg4)
    where lg2 = deleteNode lg cbnd
          lg3 = deleteNode lg2 ocbnd
          lg4 = addEdge lg3 (e ! va) (e ! vb)
   _ -> ([], lg)
rule7c' lg _ = ([], lg)
rule7c :: LRule
rule7c = LR "VII.c" rule7c'

rule7f' :: LG -> N -> ([Int], LG)
rule7f' lg@(LG _ n e _) (LGB va cb) = case n ! (e ! cb) of
   cbnd@(LGCB vb cbo cblvl) -> if cb /= (e ! cbo) then ([], lg) else
                                 ([va, (e ! va), cb, cbo, vb, (e ! vb)],
                                   lg8)
    where lg2 = deleteNode lg cbnd
          lg3 = deleteEdge lg2 va (e ! va)
          ([ncbo, ncbi], lg4) = nextVertices lg3 2
          lg5 = addNode lg4 (LGCB ncbi ncbo (cblvl + 1))
          lg6 = addEdge lg5 (e ! va) ncbo
          lg7 = addEdge lg6 ncbi va
          lg8 = addEdge lg7 cb (e ! vb)
   _ -> ([], lg)
rule7f' lg _ = ([], lg)
rule7f :: LRule
rule7f = LR "VII.f" rule7f'

rule7h' :: LG -> N -> ([Int], LG)
rule7h' lg@(LG _ n e _) (LGRB va cb 0) = case n ! (e ! cb) of
   cbnd@(LGCB vb cbo cblvl) -> if cb /= (e ! cbo) then ([], lg) else
                                 ([va, (e ! va), cb, cbo, vb, (e ! vb)],
                                   lg8)
    where lg2 = deleteNode lg cbnd
          lg3 = deleteEdge lg2 va (e ! va)
          ([ncbo, ncbi], lg4) = nextVertices lg3 2
          lg5 = addNode lg4 (LGCB ncbi ncbo (cblvl + 1))
          lg6 = addEdge lg5 (e ! va) ncbo
          lg7 = addEdge lg6 ncbi va
          lg8 = addEdge lg7 cb (e ! vb)
   _ -> ([], lg)
rule7h' lg _ = ([], lg)
rule7h :: LRule
rule7h = LR "VII.h" rule7h'

rules :: [LRule]
rules = [
  rule1a,
  rule1b,
  rule2a,
  rule2b,
  rule2c,
  rule3a,
  rule4b,
  rule4d,
  rule4e,
  rule5a,
  rule5b,
  rule6a,
  rule6cd,
  rule6ef,
  rule7c,
  rule7f
  ]
  
diagramRules' :: LG -> [(LRule, Int)] -> Double -> Int -> Int -> Diagram B
diagramRules' lg [] scl cnt rws = diagramGraphWithHeading lg (Set.fromList rls) scl nm'
  # translateX ((fromIntegral (cnt `mod` rws)) * 30.5 * scl / 30.0) 
  # translateY ((fromIntegral (cnt `div` rws)) * (-32) * scl / 30.0) <>
  nxt 
  where nxt = if rls == [] then mempty else nxtp
        nxtp = diagramRules' lg' [] scl (cnt + 1) rws
        (nm, rls, lg') = applyAnyRule lg rules
        nm' = if nm == "" then "Done." else "Applying rule " ++ nm
        
diagramRules' lg ((lr, lrl):lrs) scl cnt rws = 
  (diagramGraphWithHeading lg (Set.fromList rls) scl ("Applying rule " ++ lrname lr))
  # translateX ((fromIntegral (cnt `mod` rws)) * 30.5 * scl / 30.0) 
  # translateY ((fromIntegral (cnt `div` rws)) * (-32) * scl / 30.0) <>
  (diagramRules' lg' lrs scl (cnt + 1) rws)
    where (rls, lg') = applyRule lg lr lrl

diagramRules :: LG -> [(LRule, Int)] -> Double -> Int -> Diagram B
diagramRules lg rl scl rws = diagramRules' lg rl scl 0 rws  
