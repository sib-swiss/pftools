*       Program pfsearch
*----------------------------------------------------------------------*     
*       Function: Scan a protein or DNA sequence library for profile 
*                 matches 
*       Author:   Philipp Bucher
*       Syntax:   pfsearch [-abfru] profile seqfile [cut-off]
*       Version:  This file is part of pftools release 0.1 January 1995
*----------------------------------------------------------------------*     
* DATA
*----------------------------------------------------------------------*     

* array dimensions and I/O units

        Parameter        (IDMP=1023)
        Parameter        (IDMS=262144)
        Parameter        (IDMA=1024) 

        Parameter        (NOUT=   6)    

        Parameter        (NPRF=  11)    
        Parameter        (NSEQ=  12)    

* profile 

        Character*64      FPRF

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'pfdat.f'
        Include          'dfdat.f'
        Include          'pxdat.f'

        Logical           LUNI
        Logical           LNOR

* sequence

        Character*64      FSEQ

        Character*12      CSID
        Character*08      CSAC
        Character*256     CSDE

        Integer           LSEQ
        Integer*2         ISEQ(IDMS)

        Logical           LCKS(IDMS)

        Integer*2         IS

* options and command line parameters

        Logical           OPTA
        Logical           OPTB 
        Logical           OPTF 
        Logical           OPTR 
        Logical           OPTU 
        

        Integer           NCUC
        Integer           KCUC
        real              XCUC

* alignments

        Integer           NALI
        Integer           IALS(IDMA)
        Integer           IALB(IDMA)
        Integer           IAL1(IDMA)
        Integer           IAL2(IDMA)
        Integer           IALE(IDMA)

* path matrix fields

        Integer           IOPM(0:IDMP)
        Integer           IOPI(0:IDMP)
        Integer           IOPD(0:IDMP)

        Integer           IOMB(0:IDMP)
        Integer           IOM1(0:IDMP)
        Integer           IOM2(0:IDMP)

        Integer           IOIB(0:IDMP)
        Integer           IOI1(0:IDMP)
        Integer           IOI2(0:IDMP)

        Integer           IODB(0:IDMP)
        Integer           IOD1(0:IDMP)
        Integer           IOD2(0:IDMP)

        Character*132     RCEX

*----------------------------------------------------------------------*     
* INPUT SECTION 
*----------------------------------------------------------------------*     

        IRC=0
        CABC(0)='-'
 
* read command line arguments

        Call Repar(
     *     OPTA,OPTB,OPTF,OPTR,OPTU,FPRF,FSEQ,NCUC,KCUC,XCUC,IRC)
        If(IRC.NE.0) then 
           Write(0,'(
     *      ''Usage: pfsearch [-abfru] profile seqfile [cut-off]''
     *        )')
           Stop
        End if

* read profile

        Call REPRF
     *    (NPRF,FPRF,
     *     CPID,CPAC,CPDE,NABC,CABC,LPRF,LPCI,
     *     MDIS,NDIP,
     *     JNOR,MNOR,NNOR,NNPR,CNTX,RNOP, 
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT, 
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

        If(IRC.GT.0) go to 100

* cut-off value

           KCUT=0
        Do   6 I1=1,JCUT
           If(MCLE(I1).EQ.0) then
                 INOR=0
              If(JCNM(I1).NE.0) then 
                 LNOR=.TRUE.
                       J2=1
                 Do  5 I2=1,JCNM(I1)
                          J3=0
                    Do  4 I3=1,JNOR
                       If(MCUT(I2,I1).EQ.NNOR(I3)) J3=I3
    4               Continue
                 
                    If     (I2.EQ.1) then 
                       INOR=J3
                       NPRI=NNPR(J3)    
                       IFUN=MNOR(J3)
                       XCUT=RCUT(I2,I1)
                    Else if(NNPR(J3).LT.NPRI) then
                       INOR=J3
                       NPRI=NNPR(J3)    
                       IFUN=MNOR(J3)
                       KCUT=ICUT(I1)
                       XCUT=RCUT(I2,I1)
                    End if
    5            Continue
              End if 

              If(JCNM(I1).EQ.0.OR.INOR.EQ.0) then
                 KCUT=ICUT(I1)
                 LNOR=.FALSE.
              End if
           End if
    6   Continue

        If     (NCUC.EQ.1) then 
           KCUT=KCUC 
           LNOR=.FALSE.
        Else if(NCUC.EQ.2.AND.LNOR) then
           XCUT=XCUC 
        End if

        If(OPTR) LNOR=.FALSE.

* disjoint definition

        If(MDIS.EQ.1.OR.OPTU.OR.OPTA) then
           LUNI=.TRUE.
        Else
           LUNI=.FALSE.

* - initialize profile lock

           Do  8 I1=0,NDIP(1)-1 
              IIPP(E0,I1)=NLOW
              IIPP(E1,I1)=NLOW
    8      Continue

           Do  9 I1=NDIP(2),LPRF 
              IIPP(B0,I1)=NLOW
              IIPP(B1,I1)=NLOW
    9      Continue

        End if

* profile extra parameters

           MLOW=NLOW/4*3
        Do  10 I1=0,LPRF
           IIPX( XM,I1) = MAX(MLOW,IIPP( B1,I1) + IIPP( BM,I1)) 
           IIPX( XI,I1) = MAX(MLOW,IIPP( B1,I1) + IIPP( BI,I1)) 
           IIPX( XD,I1) = MAX(MLOW,IIPP( B1,I1) + IIPP( BD,I1)) 
           IIPX( YM,I1) = MAX(MLOW,IIPP( B0,I1) + IIPP( BM,I1)) 
           IIPX( YI,I1) = MAX(MLOW,IIPP( B0,I1) + IIPP( BI,I1)) 
           IIPX( YD,I1) = MAX(MLOW,IIPP( B0,I1) + IIPP( BD,I1)) 
           IIPX( MX,I1) = MAX(MLOW,IIPP( E1,I1) + IIPP( ME,I1)) 
           IIPX( IX,I1) = MAX(MLOW,IIPP( E1,I1) + IIPP( IE,I1)) 
           IIPX( DX,I1) = MAX(MLOW,IIPP( E1,I1) + IIPP( DE,I1)) 
           IIPX( MY,I1) = MAX(MLOW,IIPP( E0,I1) + IIPP( ME,I1)) 
           IIPX( IY,I1) = MAX(MLOW,IIPP( E0,I1) + IIPP( IE,I1)) 
           IIPX( DY,I1) = MAX(MLOW,IIPP( E0,I1) + IIPP( DE,I1)) 
   10   Continue

*----------------------------------------------------------------------*
* major loop over sequences
*----------------------------------------------------------------------*

* read sequence  

   20   Continue

        If(OPTF) then 
           Call RFSEQ
     *       (NSEQ,FSEQ,NABC,CABC,CSID,CSDE,LSEQ,ISEQ,IRC)
        Else 
           Call RESEQ
     *       (NSEQ,FSEQ,NABC,CABC,CSID,CSAC,CSDE,LSEQ,ISEQ,IRC)
        End if 
        If(IRC.EQ.-1) go to 100

        If(IRC.NE.0) then 
           Write(0,'(
     *      ''Sequence file unreadable or in wrong format.''
     *        )')
           Stop
        End if 

* compute cut-off in raw score units

        If(LNOR)
     *     Call NtoR(XCUT,KCUT,RNOP,KNPM,MAXN,INOR,IFUN,LSEQ)

* compute optimal alignment score

        Call XALI1
     *    (NABC,CABC,LPRF,LPCI,
     *     KCUT,IDMP,IIPP,IMPP,CHIP,CHMP,IIPX,
     *     IDMS,LSEQ,ISEQ,
     *     IOPM,IOPI,IOPD,
     *     IOPT,LUNI,  
     *     IRC)

        If(OPTA) then
           Continue         
        Else if(IOPT.LT.KCUT) then
           go to  50
        End if 

        If(LNOR) then
           Call RtoN(IOPT,XOPT,RNOP,KNPM,MAXN,INOR,IFUN,LSEQ)
        End if

        If(LUNI) then 
           If(LNOR) then
              Write(RCEX,'(F8.2,I6,'' ''A12,'' '',A104)')
     *        XOPT,IOPT,CSID,CSDE(1:104)
           Else 
              Write(RCEX,'(I6,'' ''A12,'' '',A112)')
     *        IOPT,CSID,CSDE(1:111)
           End if
           Write(6,'(132A)')(RCEX(ii1:ii1),ii1=1,Lnblnk(RCEX))
           Go to  50
        End if 

* initialize sequence lock

        Do  25 I1=1,LSEQ
           LCKS(I1)=.FALSE.
   25   Continue


        Call XALIP
     *    (NABC,CABC,LPRF,LPCI,NDIP(1),NDIP(2),
     *     KCUT,IDMP,IIPP,IMPP,CHIP,CHMP,IIPX,
     *     IDMS,LSEQ,ISEQ,LCKS,
     *     IOPM,IOPI,IOPD,
     *     IOMB,IOM1,IOM2,IOIB,IOI1,IOI2,IODB,IOD1,IOD2,
     *     IDMA,NALI,IALS,IALB,IAL1,IAL2,IALE, 
     *     IRC)

        Do  30 I1=1,NALI
           If(LNOR) then
              Call RtoN(IALS(I1),XOPT,RNOP,KNPM,MAXN,INOR,IFUN,LSEQ) 
              Write(RCEX,
     *         '(F8.4,I6,'' '',A12,'' '',I6,'' -'',I6,'' '',A89)')
     *           XOPT,IALS(I1),CSID,IALB(I1),IALE(I1),CSDE(1:89)
           Else
              Write(RCEX,'(I6,'' ''A12,'' '',I6,'' -'',I6,'' '',A97)')
     *           IALS(I1),CSID,IALB(I1),IALE(I1),CSDE(1:97)
           End if
           Write(6,'(132A)')(RCEX(ii1:ii1),ii1=1,Lnblnk(RCEX))
   30   Continue

   50   If(.NOT.OPTB) go to  20

*----------------------------------------------------------------------*
* Complementary strand 
*----------------------------------------------------------------------*

* generate complementary sequence

           J1=LSEQ          
        Do  54 I1=1,LSEQ/2
           IS=ISEQ(I1)
           ISEQ(I1)=ISEQ(J1)
           ISEQ(J1)=IS
           J1=J1-1
   54   Continue

        Do  55 I1=1,LSEQ
           If(ISEQ(I1).NE.0) ISEQ(I1)=NABC-ISEQ(I1)+1
   55   Continue

* compute optimal alignment score

        Call XALI1
     *    (NABC,CABC,LPRF,LPCI,
     *     KCUT,IDMP,IIPP,IMPP,CHIP,CHMP,IIPX,
     *     IDMS,LSEQ,ISEQ,
     *     IOPM,IOPI,IOPD,
     *     IOPT,LUNI,  
     *     IRC)

        If(OPTA) then
           Continue         
        Else if(IOPT.LT.KCUT) then
           go to  80
        End if 

        If(LNOR) then
           Call RtoN(IOPT,XOPT,RNOP,KNPM,MAXN,INOR,IFUN,LSEQ)
        End if

        If(LUNI) then 
           If(LNOR) then
              Write(RCEX,'(F8.2,I6,'' ''A12,'' (complement)'',A92)')
     *        XOPT,IOPT,CSID,CSDE(1:92)
           Else 
              Write(RCEX,'(I6,'' ''A12,'' (complement)'',A100)')
     *        IOPT,CSID,CSDE(1:100)
           End if
           Write(6,'(132A)')(RCEX(ii1:ii1),ii1=1,Lnblnk(RCEX))
           Go to  80
        End if 

* initialize sequence lock

        Do  65 I1=1,LSEQ
           LCKS(I1)=.FALSE.
   65   Continue


        Call XALIP
     *    (NABC,CABC,LPRF,LPCI,NDIP(1),NDIP(2),
     *     KCUT,IDMP,IIPP,IMPP,CHIP,CHMP,IIPX,
     *     IDMS,LSEQ,ISEQ,LCKS,
     *     IOPM,IOPI,IOPD,
     *     IOMB,IOM1,IOM2,IOIB,IOI1,IOI2,IODB,IOD1,IOD2,
     *     IDMA,NALI,IALS,IALB,IAL1,IAL2,IALE, 
     *     IRC)

        Do  70 I1=1,NALI
           IALB(I1)=LSEQ-IALB(I1)+1
           IALE(I1)=LSEQ-IALE(I1)+1
           If(LNOR) then
              Call RtoN(IALS(I1),XOPT,RNOP,KNPM,MAXN,INOR,IFUN,LSEQ) 
              Write(RCEX,
     *         '(F8.4,I6,'' '',A12,'' '',I6,'' -'',I6,'' '',A89)')
     *           XOPT,IALS(I1),CSID,IALB(I1),IALE(I1),CSDE(1:89)
           Else
              Write(RCEX,'(I6,'' ''A12,'' '',I6,'' -'',I6,'' '',A97)')
     *           IALS(I1),CSID,IALB(I1),IALE(I1),CSDE(1:97)
           End if
           Write(6,'(132A)')(RCEX(ii1:ii1),ii1=1,Lnblnk(RCEX))
   70   Continue

   80   Continue
        Go to  20

  100   Stop
        End
*----------------------------------------------------------------------*     
        Subroutine Repar(
     *     OPTA,OPTB,OPTF,OPTR,OPTU,FPRF,FSEQ,NCUC,KCUC,XCUC,IRC)

        Logical           OPTA 
        Logical           OPTB 
        Logical           OPTF 
        Logical           OPTR 
        Logical           OPTU 
        Character*64      FPRF
        Character*64      FSEQ
        Character*64      CARG

        IRC=0

        N1=Iargc()

           J1=0
        Do  10 I1=1,N1
           Call GetArg(I1,CARG)
           If(CARG(1:1).EQ.'-'.AND.J1.LE.1) then 
              If(Index(CARG,'a').NE.0) OPTA=.TRUE.
              If(Index(CARG,'b').NE.0) OPTB=.TRUE.
              If(Index(CARG,'f').NE.0) OPTF=.TRUE.
              If(Index(CARG,'r').NE.0) OPTR=.TRUE.
              If(Index(CARG,'u').NE.0) OPTU=.TRUE.
           Else 
              J1=J1+1
              If     (J1.EQ.1) then 
                 FPRF=CARG
              Else if(J1.EQ.2) then
                 FSEQ=CARG
              Else if(J1.EQ.3) then

* - cut-off value on command line    

                    NCUC=0
                 If(Index(CARG,'.').EQ.0) then
                    NCUC=1 
                    Read(CARG,*) KCUC
                 Else 
                    NCUC=2
                    Read(CARG,*) XCUC
                 End if 
              End if
           End if
   10   Continue 

        If(J1.LT.2) IRC=1
        Return
        End 
*----------------------------------------------------------------------*     
        Include          'reprf.f'
        Include          'reseq.f'
        Include          'rfseq.f'
        Include          'xali1.f'
        Include          'xalip.f'
        Include          'RtoN.f'
        Include          'NtoR.f'
