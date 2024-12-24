module Day24 (
    day24_part1,
    day24_part2
    ) where

import Data.Bits ( ( .&. ), ( .|. ), xor )
--import Debug.Trace ( trace, traceShow )

compose :: [Int] -> Int
compose = foldr (\z acc -> acc * 2 + z) 0

part1_example_1_execute :: Int
part1_example_1_execute =
    let x00 = 1
        x01 = 1
        x02 = 1
        y00 = 0
        y01 = 1
        y02 = 0

        z00 = x00  .&.  y00
        z01 = x01 `xor` y01
        z02 = x02  .|.  y02
    in compose [z00, z01, z02]

part1_example_2_execute :: Int
part1_example_2_execute = 
    let x00 = 1
        x01 = 0
        x02 = 1
        x03 = 1
        x04 = 0
        y00 = 1
        y01 = 1
        y02 = 1
        y03 = 1
        y04 = 1

        mjb = ntg `xor` fgs
        tnw = y02  .|.  x01
        z05 = kwq  .|.  kpj
        fst = x00  .|.  x03
        z01 = tgd `xor` rvg
        bfw = vdt  .|.  tnw
        z10 = bfw  .&.  frj
        bqk = ffh  .|.  nrd
        djm = y00  .&.  y03
        psh = y03  .|.  y00
        z08 = bqk  .|.  frj
        frj = tnw  .|.  fst
        z11 = gnj  .&.  tgd
        z00 = bfw `xor` mjb
        vdt = x03  .|.  x00
        z02 = gnj  .&.  wpb
        kjc = x04  .&.  y00
        qhw = djm  .|.  pbm
        hwm = nrd  .&.  vdt
        rvg = kjc  .&.  fst
        fgs = y04  .|.  y02
        pbm = y01  .&.  x02
        kwq = ntg  .|.  kjc
        tgd = psh `xor` fgs
        z09 = qhw `xor` tgd
        kpj = pbm  .|.  djm
        ffh = x03 `xor` y03
        ntg = x00 `xor` y04
        z06 = bfw  .|.  bqk
        wpb = nrd `xor` fgs
        z04 = frj `xor` qhw
        z07 = bqk  .|.  frj
        nrd = y03  .|.  x01
        z03 = hwm  .&.  bqk
        z12 = tgd `xor` rvg
        gnj = tnw  .|.  pbm
    in compose [z00, z01, z02, z03, z04, z05, z06, z07, z08, z09, z10, z11, z12]

part1_execute :: Int
part1_execute =
    let x00 = 1
        x01 = 1
        x02 = 1
        x03 = 1
        x04 = 0
        x05 = 1
        x06 = 0
        x07 = 1
        x08 = 0
        x09 = 1
        x10 = 1
        x11 = 1
        x12 = 1
        x13 = 0
        x14 = 1
        x15 = 1
        x16 = 1
        x17 = 1
        x18 = 1
        x19 = 0
        x20 = 1
        x21 = 1
        x22 = 0
        x23 = 1
        x24 = 1
        x25 = 0
        x26 = 1
        x27 = 1
        x28 = 1
        x29 = 0
        x30 = 1
        x31 = 1
        x32 = 0
        x33 = 1
        x34 = 1
        x35 = 0
        x36 = 0
        x37 = 1
        x38 = 1
        x39 = 1
        x40 = 0
        x41 = 0
        x42 = 0
        x43 = 0
        x44 = 1
        y00 = 1
        y01 = 1
        y02 = 1
        y03 = 1
        y04 = 1
        y05 = 0
        y06 = 0
        y07 = 0
        y08 = 0
        y09 = 0
        y10 = 1
        y11 = 0
        y12 = 0
        y13 = 0
        y14 = 1
        y15 = 0
        y16 = 0
        y17 = 1
        y18 = 0
        y19 = 1
        y20 = 0
        y21 = 1
        y22 = 1
        y23 = 0
        y24 = 0
        y25 = 0
        y26 = 1
        y27 = 1
        y28 = 1
        y29 = 0
        y30 = 0
        y31 = 0
        y32 = 1
        y33 = 0
        y34 = 0
        y35 = 1
        y36 = 0
        y37 = 0
        y38 = 0
        y39 = 1
        y40 = 1
        y41 = 1
        y42 = 1
        y43 = 1
        y44 = 1
        cgm = ktr  .&.  qkd
        smb = y41  .&.  x41
        kmt = gst  .&.  gtv
        bfq = y29 `xor` x29
        z07 = hqs `xor` jmb
        bdr = x03  .&.  y03
        z30 = gbs `xor` rrg
        dqf = gbs  .&.  rrg
        wpd = pgs  .&.  jhf
        z27 = rbr `xor` frj
        jrp = vsr  .|.  gng
        fjs = knw  .|.  hwq
        vbf = tsp  .&.  vng
        z45 = fpg  .|.  dqg
        vjq = wpd  .|.  smb
        z26 = fcn `xor` pmf
        hvf = dqf  .|.  tmm
        ghc = pmq  .&.  ggj
        dcg = dgk  .&.  ppk
        z22 = y22  .&.  x22
        qth = hqs  .&.  jmb
        tgg = pmf  .&.  fcn
        rhg = y28  .&.  x28
        z05 = pvb `xor` jbd
        z13 = cqb `xor` pbd
        dgj = dcg  .|.  vgw
        psh = mpj  .&.  jrp
        ggj = y39 `xor` x39
        vvm = y38 `xor` x38
        dqg = rrq  .&.  gjw
        vkh = y23  .&.  x23
        pgs = x41 `xor` y41
        dwm = jsb  .|.  pqq
        qcj = x15  .&.  y15
        qrq = qnq  .&.  ftn
        rrg = x30 `xor` y30
        rmm = y12 `xor` x12
        z10 = qnq `xor` ftn
        cmn = qwd  .|.  ntb
        hct = y01  .&.  x01
        pbq = ncj  .|.  dvw
        dcf = pdq  .|.  rhg
        cmd = y13  .&.  x13
        bgs = pvc  .&.  tdm
        fcn = rws  .|.  gwq
        fpg = x44  .&.  y44
        cgv = wss  .&.  hgw
        chb = y04  .&.  x04
        z40 = qbn `xor` dhr
        z42 = cpr `xor` vjq
        tpp = rhk  .&.  vsn
        z25 = dwm `xor` cnn
        nhj = jrk  .|.  nhp
        pmq = jnb  .|.  ctv
        pqp = x42  .&.  y42
        knk = dck  .&.  qrb
        z12 = vmv `xor` rmm
        vkv = vbf  .|.  ksh
        ncj = y19  .&.  x19
        frr = qth  .|.  ggw
        z24 = qbr `xor` qsr
        dvw = grf  .&.  wfs
        qnq = csw  .|.  tbc
        qdj = x31  .&.  y31
        qgg = y20 `xor` x20
        cvf = x32  .&.  y32
        pqq = qbr  .&.  qsr
        jnb = vvm  .&.  dnk
        pdq = bst  .&.  stp
        ctv = y38  .&.  x38
        gbs = bfq `xor` dcf
        gsm = x12  .&.  y12
        qsr = y24 `xor` x24
        z08 = vqp  .&.  frr
        pbd = y13 `xor` x13
        dgk = jth  .|.  dnn
        jbd = y05 `xor` x05
        rhk = y00  .&.  x00
        z23 = fhs `xor` fjs
        z11 = gtv `xor` gst
        mfj = y37  .&.  x37
        gtc = y15 `xor` x15
        mqv = y11  .&.  x11
        qrb = bkg  .|.  ktg
        gtv = y11 `xor` x11
        csw = x09  .&.  y09
        rbt = dhr  .&.  qbn
        z38 = vvm `xor` dnk
        vng = cvf  .|.  bgq
        wfs = hpp  .|.  sgw
        z16 = djg `xor` kpw
        kpw = x16 `xor` y16
        dgr = x27  .&.  y27
        gwq = y25  .&.  x25
        sgw = dgj  .&.  stq
        z33 = tsp `xor` vng
        cqb = gsm  .|.  sft
        vmv = kmt  .|.  mqv
        z36 = qvh `xor` fpc
        fpc = ptf  .|.  psh
        gjw = wjm  .|.  cgm
        qbr = vkh  .|.  fdc
        tsp = y33 `xor` x33
        grf = x19 `xor` y19
        rrq = x44 `xor` y44
        jdj = pwg  .&.  mbr
        mjj = wrm  .|.  cgv
        z35 = jrp `xor` mpj
        z31 = hvf `xor` pfk
        cpr = x42 `xor` y42
        cdf = x22 `xor` y22
        tmm = x30  .&.  y30
        z21 = bnv `xor` nhj
        wjm = y43  .&.  x43
        z04 = hfw `xor` ggh
        z09 = dnc `xor` pjb
        z17 = ppk `xor` dgk
        stp = y28 `xor` x28
        z15 = mjj `xor` gtc
        hpp = y18  .&.  x18
        sjw = x06  .&.  y06
        dnc = mpr  .|.  thm
        z02 = mbr `xor` pwg
        frc = y26  .&.  x26
        thm = frr `xor` vqp
        pwg = y02 `xor` x02
        z41 = jhf `xor` pgs
        z00 = x00 `xor` y00
        z18 = stq `xor` dgj
        vns = cqb  .&.  pbd
        dck = x06 `xor` y06
        ggh = qwg  .|.  bdr
        tnm = x36  .&.  y36
        pjb = x09 `xor` y09
        tdm = hdw  .|.  tnm
        rwc = vjq  .&.  cpr
        bnv = y21 `xor` x21
        qkd = rwc  .|.  pqp
        nhp = qgg  .&.  pbq
        ggw = y07  .&.  x07
        z14 = hgw `xor` wss
        cnn = y25 `xor` x25
        z37 = tdm `xor` pvc
        qwg = jdt  .&.  rsm
        bkg = y05  .&.  x05
        rsm = dhh  .|.  jdj
        dnn = djg  .&.  kpw
        z20 = qgg `xor` pbq
        ntv = gtc  .&.  mjj
        frj = tgg  .|.  frc
        jth = y16  .&.  x16
        fhs = y23 `xor` x23
        jsb = x24  .&.  y24
        bst = pgq  .|.  dgr
        wss = x14  .&.  y14
        jhf = rbt  .|.  sng
        qbb = y10  .&.  x10
        z03 = rsm `xor` jdt
        pfk = y31 `xor` x31
        vsr = vkv  .&.  bbc
        vsn = y01 `xor` x01
        mbr = tpp  .|.  hct
        bgq = qvq  .&.  cdn
        cdn = y32 `xor` x32
        qwd = x21  .&.  y21
        hfw = y04 `xor` x04
        ppk = y17 `xor` x17
        rpq = bfq  .&.  dcf
        spd = ggh  .&.  hfw
        jdt = x03 `xor` y03
        ktg = jbd  .&.  pvb
        stq = x18 `xor` y18
        gng = x34  .&.  y34
        sng = y40  .&.  x40
        ksh = x33  .&.  y33
        z06 = qrb `xor` dck
        pmf = x26 `xor` y26
        z01 = vsn `xor` rhk
        pgq = frj  .&.  rbr
        mpr = x08  .&.  y08
        knw = cdf  .&.  cmn
        qvq = wjp  .|.  qdj
        fdc = fjs  .&.  fhs
        jmb = x07 `xor` y07
        hdw = qvh  .&.  fpc
        bbc = x34 `xor` y34
        dhr = ghm  .|.  ghc
        sft = rmm  .&.  vmv
        z43 = ktr `xor` qkd
        hqs = knk  .|.  sjw
        qbn = y40 `xor` x40
        dnk = bgs  .|.  mfj
        z39 = pmq `xor` ggj
        gst = qrq  .|.  qbb
        mpj = y35 `xor` x35
        z44 = rrq `xor` gjw
        z34 = vkv `xor` bbc
        ktr = y43 `xor` x43
        djg = qcj  .|.  ntv
        z19 = wfs `xor` grf
        ftn = x10 `xor` y10
        pvb = chb  .|.  spd
        rbr = x27 `xor` y27
        dhh = y02  .&.  x02
        z32 = qvq `xor` cdn
        z28 = bst `xor` stp
        wrm = x14 `xor` y14
        vgw = y17  .&.  x17
        qvh = x36 `xor` y36
        jrk = x20  .&.  y20
        ptf = x35  .&.  y35
        ntb = bnv  .&.  nhj
        rws = cnn  .&.  dwm
        hwq = cmn `xor` cdf
        tbc = pjb  .&.  dnc
        ghm = x39  .&.  y39
        vqp = x08 `xor` y08
        hgw = cmd  .|.  vns
        z29 = grd  .|.  rpq
        grd = y29  .&.  x29
        pvc = y37 `xor` x37
        wjp = hvf  .&.  pfk
    in compose [z00, z01, z02, z03, z04, z05, z06, z07, z08, z09, z10, z11, z12, z13, z14, z15, z16, z17, z18, z19, z20, z21, z22, z23, z24, z25, z26, z27, z28, z29, z30, z31, z32, z33, z34, z35, z36, z37, z38, z39, z40, z41, z42, z43, z44, z45]

-- Part 1
day24_part1 :: String -> IO Int
day24_part1 _ = do
    let result = part1_execute
    return result

-- Part 2
day24_part2 :: String -> IO Int
day24_part2 input = do
    return 0
