*       Version:  This file is part of pftools release 2.0 June 1997
*----------------------------------------------------------------------*     
        Subroutine XALIP(
     *     NABC,CABC,LPRF,LPCI,N1,N2,
     *     KCUT,IDMP,IIPP,IMPP,CHIP,CHMP,IIPX,
     *     IDMS,LSEQ,ISEQ,LCKS,
     *     IOPM,IOPI,IOPD,
     *     IOMB,IOM1,IOM2,IOIB,IOI1,IOI2,IODB,IOD1,IOD2,
     *     IDMN,NALI,IALS,IALB,IAL1,IAL2,IALE, 
     *     IRC)

*----------------------------------------------------------------------* 
* Data Section 
*----------------------------------------------------------------------* 

* profile:

        Include          'gsdat.f'
        Include          'pfdat.f'
        Include          'pxdat.f'
        Include          'sterr.f'

* sequence

        Integer           LSEQ
        Integer*2         ISEQ(IDMS)
        Logical           LCKS(IDMS)

* alignments

        Integer           IALS(IDMN)
        Integer           IALB(IDMN)
        Integer           IAL1(IDMN)
        Integer           IAL2(IDMN)
        Integer           IALE(IDMN)

* alignment scores

        Integer           IOPM(0:IDMP)
        Integer           IOPI(0:IDMP)
        Integer           IOPD(0:IDMP)

* alignment positions

        Integer           IOMB(0:IDMP)
        Integer           IOM1(0:IDMP)
        Integer           IOM2(0:IDMP)

        Integer           IOIB(0:IDMP)
        Integer           IOI1(0:IDMP)
        Integer           IOI2(0:IDMP)

        Integer           IODB(0:IDMP)
        Integer           IOD1(0:IDMP)
        Integer           IOD2(0:IDMP)


* search control values 

        Integer           IBEG 
        Integer           ILCP
        Integer           IFCP
        Integer           NLCP
        Integer           JLCP 
        Integer           IFER 

        Integer           NSCA

*----------------------------------------------------------------------* 
* Inititialization Section
*----------------------------------------------------------------------* 

        IRC=0

* alignment list

        IOPT=NLOW
        NALI=0

* search control fields  

        IBEG=0
        JLCP=LPRF/2
        NSCA=0

*----------------------------------------------------------------------* 
* two-step-forwards-one-step-backwards loop  
*----------------------------------------------------------------------* 

   11   Continue
C       Write(6,'(''Searching range:'',I6,'' - '',I6)') IBEG,LSEQ

        I1=IBEG
        JALS=NLOW

* initiate work arrays 

        Call InitR(
     *     I1,
     *     N1,N2,
     *     IDMP,LPRF,IIPP,IMPP,CHIP,CHMP,IIPX,  
     *     IOPM,IOPI,IOPD,
     *     IOMB,IOM1,IOM2,IOIB,IOI1,IOI2,IODB,IOD1,IOD2)

* intitiate search control values 

        ILCP=I1
        IFCP=I1+1
        NLCP=ILCP+JLCP

* move one sequence position forward 

        I1=I1+1

*----------------------------------------------------------------------* 
* loop over sequence positions 
*----------------------------------------------------------------------* 

   12      Continue 

           NSCA=NSCA+1

           Call NextR( 
     *        I1,
     *        NABC,LPRF,N1,N2,
     *        KCUT,IDMP,IIPP,IMPP,CHIP,CHMP,IIPX,
     *        IDMS,LSEQ,ISEQ,LCKS,
     *        IOPM,IOPI,IOPD,
     *        IOMB,IOM1,IOM2,IOIB,IOI1,IOI2,IODB,IOD1,IOD2,
     *        JALS,JALB,JAL1,JAL2,JALE) 

*----------------------------------------------------------------------* 
* work arrays updated, what next ?
*----------------------------------------------------------------------* 

* if match found 

           If(JALS.GE.KCUT) then

* - determine first ENTRY of current row 
 
                 IFER=I1
              Do 51 I2=N1,LPRF
                 If(IOD1(I2).LT.IFER) IFER=IOD1(I2) 
                 If(IOM1(I2).LT.IFER) IFER=IOM1(I2) 
                 If(IOI1(I2).LT.IFER) IFER=IOI1(I2) 
   51         Continue

              If(IFER.GT.JAL2.OR.I1.EQ.LSEQ) then 

* - accept alingment
 
                 NALI=NALI+1
                 IALS(NALI)=JALS
                 IALB(NALI)=JALB
                 IAL1(NALI)=JAL1
                 IAL2(NALI)=JAL2
                 IALE(NALI)=JALE

                 If(JAL2.LT.JAL1) then  
                    Write(NERR,*) 'Error: Illegal alignment found' //
     *                ' - no list produced.'
                    IRC=1
                    Go to 100
                 End if 
                    
                 If(NALI.GE.IDMN) then 
                    Write(NERR,*) 'Warning: Too many alingments found'
     *                 // ' - list may be imcomplete.'
                    IRC=-1
                    Go to 100
                 End if 

* - protect sequence region 

                 Do  55 I2=JAL1,JAL2
                    LCKS(I2)=.TRUE.
   55            Continue

* - move backwards 

C                Write(6,'('' interrupted at:'',I6)') I1
                 Go to 11

* - or one sequence position forwards  

              Else 
                 I1=I1+1 
                 If(I1.LE.LSEQ) Go to  12
              End if 

* if no match found

           Else

* - next check point reached ? 

              If(I1.GE.NLCP) then

* - determine first ENTRY of current row 

                    IFER=I1
                 Do 56 I2=N1+1,LPRF
                    If(IOD1(I2).LT.IFER) IFER=IOD1(I2) 
                    If(IOM1(I2).LT.IFER) IFER=IOM1(I2) 
                    If(IOI1(I2).LT.IFER) IFER=IOI1(I2) 
   56            Continue

* - update reinitialization point

                 If(IFER.GE.ILCP) then 
                    IBEG=IFCP-1
                    IFCP=IFirst(IDMP,LPRF,IOMB,IOIB,IODB)
                    ILCP=I1
                 End if 

* - calculate next check point

                 NLCP=NLCP+JLCP

              End if  

* - move one sequence positions forwards 
        
              I1=I1+1 
              If(I1.LE.LSEQ) Go to  12

           End if 

* done

   99   Continue 
C       Write(6,'(''Sequence length:'',I6,'' -'',I6,
C    *   '' positions scanned.'')') LSEQ,NSCA

  100   Return
        End
*----------------------------------------------------------------------*
        Subroutine NextR( 
     *     I1,
     *     NABC,LPRF,N1,N2,
     *     KCUT,IDMP,IIPP,IMPP,CHIP,CHMP,IIPX,
     *     IDMS,LSEQ,ISEQ,LCKS,
     *     IOPM,IOPI,IOPD,
     *     IOMB,IOM1,IOM2,IOIB,IOI1,IOI2,IODB,IOD1,IOD2,
     *     JALS,JALB,JAL1,JAL2,JALE) 

* profile:

        Include          'pfdat.f'
        Include          'pxdat.f'

* sequence

        Integer           LSEQ
        Integer*2         ISEQ(IDMS)
        Logical           LCKS(IDMS)

* alignments

        Integer           JALS
        Integer           JALB
        Integer           JAL1
        Integer           JAL2
        Integer           JALE

* alignment scores

        Integer           IOPM(0:IDMP)
        Integer           IOPI(0:IDMP)
        Integer           IOPD(0:IDMP)

* alignment positions

        Integer           IOMB(0:IDMP)
        Integer           IOM1(0:IDMP)
        Integer           IOM2(0:IDMP)

        Integer           IOIB(0:IDMP)
        Integer           IOI1(0:IDMP)
        Integer           IOI2(0:IDMP)

        Integer           IODB(0:IDMP)
        Integer           IOD1(0:IDMP)
        Integer           IOD2(0:IDMP)

* work fields 

        Integer           DZ

* initializations 

        J1=ISEQ(I1)

        IN=I1+1

        If(I1.GE.LSEQ) then
           MZ=MY
           IZ=IY
           DZ=DY
        Else
           MZ=MX
           IZ=IX
           DZ=DX
        End if

* disable match and insert vertices of protected region

        If(LCKS(I1)) then 
              IOPM(N1-1)=NLOW    
           Do 10 I2=N1,N2-1
              IOPM(I2)=NLOW 
              IOPI(I2)=NLOW 
   10      Continue
        End if 

* profile positions 0 

* - save previous match position

        KOPM=IOPM( 0)
        KOMB=IOMB( 0)
        KOM1=IOM1( 0)
        KOM2=IOM2( 0)

        KI=IOPI( 0)+IIPP(J1, 0) 

* - match position

        JI=KI+IIPP(IM, 0)
        If(JI.GT.IIPX(XM, 0)) then
           IOPM( 0)=JI
           IOMB( 0)=IOIB( 0)
        Else
           IOPM( 0)=IIPX(XM, 0)
           IOMB( 0)=IN
        End if
           IOM1( 0)=0
           IOM2( 0)=0

* - deletion position

        JI=KI+IIPP(ID, 0)
        If(JI.GT.IIPX(XD, 0)) then
           IOPD( 0)=JI
           IODB( 0)=IOIB( 0)
        Else
           IOPD( 0)=IIPX(XD, 0)
           IODB( 0)=IN
        End if
           IOD1( 0)=0
           IOD2( 0)=0

* - insert position

        JI=KI+IIPP(II, 0)
        If(JI.GT.IIPX(XI, 0)) then
           IOPI( 0)=JI
        Else
           IOPI( 0)=IIPX(XI, 0)
           IOIB( 0)=IN
        End if
           IOI1( 0)=0
           IOI2( 0)=0

* profile positions 1 to LPRF  

           JE=0
        Do  99 I2=1,LPRF
           If(I2.GE.N1) JE=IN

           KM=KOPM       + IMPP(J1,I2)
           KI=IOPI(I2  ) + IIPP(J1,I2)
           KD=IOPD(I2-1) + IMPP( D,I2)

* - update match vertex

           JB=IIPX(XM,I2)
           JM=KM          + IIPP(MM,I2)
           JI=KI          + IIPP(IM,I2)
           JD=KD          + IIPP(DM,I2)
           KOPM=IOPM(I2)
           Call Nstep(NEWM,IOPM(I2),JB,JM,JI,JD)

* - update insert vertex

           JB=IIPX(XI,I2)
           JM=KM          + IIPP(MI,I2)
           JI=KI          + IIPP(II,I2)
           JD=KD          + IIPP(DI,I2)
           Call Nstep(NEWI,IOPI(I2),JB,JM,JI,JD)

* - update deletion vertex

           JB=IIPX(XD,I2)
           JM=KM          + IIPP(MD,I2)
           JI=KI          + IIPP(ID,I2)
           JD=KD          + IIPP(DD,I2)
           Call Nstep(NEWD,IOPD(I2),JB,JM,JI,JD)

* - compute alignment scores 

           KM=KM+IIPX(MZ,I2)
           KI=KI+IIPX(IZ,I2)
           KD=KD+IIPX(DZ,I2)

           KE=MAX(KM,KI,KD)

* - check for new maxumum 

           If(KE.GT.JALS) then
              JALS=KE
              If     (JALS.EQ.KD) then
                 JALB=IODB(I2-1)
                 JAL1=IOD1(I2-1)
                 JAL2=IOD2(I2-1)
              Else if(JALS.EQ.KM) then
                 JALB=KOMB
                 JAL1=KOM1
                 JAL2=KOM2
              Else
                 JALB=IOIB(I2)
                 JAL1=IOI1(I2)
                 JAL2=IOI2(I2)
              End if
              JALE=I1 
              If(JAL1.EQ.0) JAL1=JALB
              If(JAL2.EQ.0) JAL2=I1
           End if 

* - update alignment positions  

              JOMB=IOMB(I2)
              JOM1=IOM1(I2)
              JOM2=IOM2(I2)
           If     (NEWM.EQ.0) then 
              IOMB(I2)=IN
              IOM1(I2)=JE
              IOM2(I2)=0
           Else if(NEWM.EQ.1) then 
              IOMB(I2)=KOMB
              IOM1(I2)=KOM1
              IOM2(I2)=KOM2
           Else if(NEWM.EQ.2) then 
              IOMB(I2)=IOIB(I2) 
              IOM1(I2)=IOI1(I2) 
              IOM2(I2)=IOI2(I2)
           Else
              IOMB(I2)=IODB(I2-1) 
              IOM1(I2)=IOD1(I2-1) 
              IOM2(I2)=IOD2(I2-1)
           End if  

           If     (NEWD.EQ.0) then 
              IODB(I2)=IN
              IOD1(I2)=JE
              IOD2(I2)=0
           Else if(NEWD.EQ.1) then 
              IODB(I2)=KOMB
              IOD1(I2)=KOM1
              IOD2(I2)=KOM2
           Else if(NEWD.EQ.2) then 
              IODB(I2)=IOIB(I2) 
              IOD1(I2)=IOI1(I2) 
              IOD2(I2)=IOI2(I2)
           Else
              IODB(I2)=IODB(I2-1) 
              IOD1(I2)=IOD1(I2-1) 
              IOD2(I2)=IOD2(I2-1)
           End if  
              If(I2.EQ.N1-1) IOD1(I2)=IN
              If(I2.EQ.N2-1) IOD2(I2)=I1

           If     (NEWI.EQ.0) then 
              IOIB(I2)=IN
              IOI1(I2)=JE
              IOI2(I2)=0
           Else if(NEWI.EQ.1) then 
              IOIB(I2)=KOMB
              IOI1(I2)=KOM1
              IOI2(I2)=KOM2
           Else if(NEWI.EQ.2) then 
              Continue
           Else
              IOIB(I2)=IODB(I2-1) 
              IOI1(I2)=IOD1(I2-1) 
              IOI2(I2)=IOD2(I2-2)
           End if  

              KOMB=JOMB
              KOM1=JOM1
              KOM2=JOM2

   99   Continue

* entry and exit from protected regions:

              IOM1(N1-1)=IN
              IOM2(N2-1)=IN

        Return 
        End
*----------------------------------------------------------------------*
        Subroutine Nstep(JSTEP,NMAX,JB,JM,JI,JD)

           NMAX=JB
           JSTEP=0

        If(JD.GT.NMAX) then
           NMAX=JD
           JSTEP=3
        End if 

        If(JM.GT.NMAX) then
           NMAX=JM
           JSTEP=1
        End if 

        If(JI.GT.NMAX) then
           NMAX=JI
           JSTEP=2
        End if 
   
        Return
        End 
*----------------------------------------------------------------------*
        Subroutine InitR(
     *     I1,
     *     N1,N2,
     *     IDMP,LPRF,IIPP,IMPP,CHIP,CHMP,IIPX,  
     *     IOPM,IOPI,IOPD,
     *     IOMB,IOM1,IOM2,IOIB,IOI1,IOI2,IODB,IOD1,IOD2)

* profile

        Include          'pfdat.f'
        Include          'pxdat.f'

* alignment scores

        Integer           IOPM(0:IDMP)
        Integer           IOPI(0:IDMP)
        Integer           IOPD(0:IDMP)

* alignment positions

        Integer           IOMB(0:IDMP)
        Integer           IOM1(0:IDMP)
        Integer           IOM2(0:IDMP)

        Integer           IOIB(0:IDMP)
        Integer           IOI1(0:IDMP)
        Integer           IOI2(0:IDMP)

        Integer           IODB(0:IDMP)
        Integer           IOD1(0:IDMP)
        Integer           IOD2(0:IDMP)
*
        Integer           ZM 
        Integer           ZI 
        Integer           ZD 
*
        If(I1.EQ.0) then
           ZM=YM
           ZI=YI
           ZD=YD
        Else
           ZM=XM
           ZI=XI
           ZD=XD
        End if

           IOPM( 0)=IIPX(ZM, 0)
           IOPI( 0)=IIPX(ZI, 0)
           IOPD( 0)=IIPX(ZD, 0)

        Do  10 I2=1,LPRF

           KD=IOPD(I2-1)+IMPP( D,I2)
   
           IOPM(I2)=MAX(IIPX(ZM,I2),
     *                  KD+IIPP(DM,I2)) 
           IOPI(I2)=MAX(IIPX(ZI,I2),
     *                  KD+IIPP(DI,I2))
           IOPD(I2)=MAX(IIPX(ZD,I2),
     *                  KD+IIPP(DD,I2))
   10   Continue     

           IN=I1+1
           JE=0
        Do  20 I2=0,LPRF
           If(I2.EQ.N1) JE=IN

              IOMB(I2)=IN
              IOM1(I2)=JE
              IOM2(I2)=0

              IOIB(I2)=IN
              IOI1(I2)=JE
              IOI2(I2)=0

              IODB(I2)=IN
              IOD1(I2)=JE
              IOD2(I2)=0

   20   Continue     

* entry and exit from protected regions:

        IOM1(N1-1)=IN
        IOD1(N1-1)=IN
        IOM2(N2-1)=I1
        IOD2(N2-1)=I1

        Do  30 I2=N1,N2-1
           IOI1(I2)=IN
   30   Continue

        Return
        End 
*----------------------------------------------------------------------*
        Integer Function IFirst(IDMP,LPRF,IOMB,IOIB,IODB)

        Integer           IOMB(0:IDMP)
        Integer           IOIB(0:IDMP)
        Integer           IODB(0:IDMP)

           IFirst=IOMB(LPRF)
        Do   9 I1=LPRF,0,-1
           If(IOMB(I1).LT.IFirst) IFirst=IOMB(I1) 
           If(IOIB(I1).LT.IFirst) IFirst=IOIB(I1) 
           If(IODB(I1).LT.IFirst) IFirst=IODB(I1) 
    9   Continue

        Return
        End
