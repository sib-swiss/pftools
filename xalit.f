*       Version:  This file is part of pftools release 2.2 June 1999
*----------------------------------------------------------------------*     
        Subroutine XALIT
     *    (NABC,CABC,LPRF,LPCI,N1,N2,
     *     KCUT,IDMP,IIPP,IMPP,CHIP,CHMP,IIPX, 
     *     IDMS,LSEQ,ISEQ,LCKS, 
     *     IOPM,IOPI,IOPD, 
     *     LALI,IDMA,CALI,IDMM,CPMA, 
     *     IOPT,JALB,JALE, 
     *     IPMB,IPME,
     *     IRC) 

* profile and sequence fields :

        Include           'gsdat.f'
        Include           'pfdat.f'
        Include           'pxdat.f'

* sequence

        Integer           LSEQ
        Integer*2         ISEQ(IDMS)
        Logical           LCKS(IDMS)

* alignment
 
        Integer           LALI
        Character         CALI(IDMA)
        Character         CPMA(IDMM)
        Character         CH

* work fields

        Integer           IOPM(0:IDMP)
        Integer           IOPI(0:IDMP)
        Integer           IOPD(0:IDMP)

        Integer           ZM
        Integer           ZI
        Integer           ZD

        Integer           MZ
        Integer           IZ
        Integer           DZ

        Integer           KOPM


        IRC=0

C       If(NABC.LT.20) then 
C          CABC(0)='N'
C       Else 
C          CABC(0)='X'
C       End if    

        If((LPRF+1)*(JALE-JALB+1).GT.IDMM) then
           IRC=-1
           Go to 100
        End if  

        K1=0

* beginning of sequence segment  

           If(JALB.EQ.1) then
              ZM=YM
              ZI=YI
              ZD=YD
           Else
              ZM=XM
              ZI=XI
              ZD=XD
           End if
              MZ=MX
              IZ=IX
              DZ=DX

              K1=K1+1
              I2=0

              KM=NLOW
              KI=NLOW
              KD=NLOW

              Call MaxP
     *          (IOPM(I2),IOPI(I2),IOPD(I2),JM,JI,JD,KM,KI,KD,
     *           IIPP(MM,I2),IIPP(MI,I2),IIPP(MD,I2),
     *           IIPP(IM,I2),IIPP(II,I2),IIPP(ID,I2),
     *           IIPP(DM,I2),IIPP(DI,I2),IIPP(DD,I2),
     *           IIPX(ZM,I2),IIPX(ZI,I2),IIPX(ZD,I2))
              Call Ncode(CPMA(K1),JM,JI,JD)

           Do   9, I2=1,LPRF

              K1=K1+1

              KM=NLOW
              KI=NLOW 
              KD=IOPD(I2-1)+IMPP( D,I2)

              Call MaxP
     *          (IOPM(I2),IOPI(I2),IOPD(I2),JM,JI,JD,KM,KI,KD,
     *           IIPP(MM,I2),IIPP(MI,I2),IIPP(MD,I2),
     *           IIPP(IM,I2),IIPP(II,I2),IIPP(ID,I2),
     *           IIPP(DM,I2),IIPP(DI,I2),IIPP(DD,I2),
     *           IIPX(ZM,I2),IIPX(ZI,I2),IIPX(ZD,I2))
              Call Ncode(CPMA(K1),JM,JI,JD)

    9      Continue

* - circular extensions

           If(LPCI) then
                 L1=K1-LPRF
                 Call Dcode(CPMA(L1),LM,LI,LD)
                 If(IOPM( 0).LT.IOPM(LPRF)) then
                    IOPM( 0)=IOPM(LPRF)
                    LM=JM 
                 End if 
                 If(IOPI( 0).LT.IOPI(LPRF)) then
                    IOPI( 0)=IOPI(LPRF)
                    LI=JI 
                 End if 
                 If(IOPD( 0).LT.IOPD(LPRF)) then
                    IOPD( 0)=IOPD(LPRF)
                    LD=JD 
                 End if 
                 Call Ncode(CPMA(L1),LM,LI,LD)

              Do I2=1,LPRF
                 L1=L1+1
                 KD=IOPD(I2-1)+IMPP( D,I2)
                 If(IOPD(I2).GE.KD+IIPP(DD,I2)) then
                    Go to  10
                 Else
                    Call Dcode(CPMA(L1),LM,LI,LD)
                    If(IOPM(I2).LT.KD+IIPP(DM,I2)) then
                       IOPM(I2)=KD+IIPP(DM,I2)
                       LM=3
                    End if 
                    If(IOPI(I2).LT.KD+IIPP(DI,I2)) then
                       IOPI(I2)=KD+IIPP(DI,I2)
                       LI=3
                    End if 
                       IOPD(I2)=KD+IIPP(DD,I2)
                       LD=3
                    Call Ncode(CPMA(L1),LM,LI,LD)
                 End if
              End do
   10         Continue
           End if
C          Call wtrace(IDMP,IDMM,LPRF,CPMA,K1-LPRF,IOPM,IOPI,IOPD)
* -----------------------------------------------------------


* internal sequence positions

              ZM=XM
              ZI=XI
              ZD=XD

        Do  25 I1=JALB,JALE-1

           If(LCKS(I1)) then
                 IOPM(N1-1)=NLOW
              Do I2=N1,N2-1
                 IOPM(I2)=NLOW
                 IOPI(I2)=NLOW
              End do 
           End if

              J1=ISEQ(I1)
              K1=K1+1
              I2=0

              KM=NLOW
              KI=IOPI( 0)+IIPP(J1, 0)
              KD=NLOW

              KOPM=IOPM( 0)

              Call MaxP
     *          (IOPM(I2),IOPI(I2),IOPD(I2),JM,JI,JD,KM,KI,KD,
     *           IIPP(MM,I2),IIPP(MI,I2),IIPP(MD,I2),
     *           IIPP(IM,I2),IIPP(II,I2),IIPP(ID,I2),
     *           IIPP(DM,I2),IIPP(DI,I2),IIPP(DD,I2),
     *           IIPX(ZM,I2),IIPX(ZI,I2),IIPX(ZD,I2))
              Call Ncode(CPMA(K1),JM,JI,JD)

           Do  19 I2=1,LPRF

              K1=K1+1

              KM=KOPM      +IMPP(J1,I2)
              KI=IOPI(I2  )+IIPP(J1,I2)
              KD=IOPD(I2-1)+IMPP( D,I2)

              KOPM=IOPM(I2)

              Call MaxP
     *          (IOPM(I2),IOPI(I2),IOPD(I2),JM,JI,JD,KM,KI,KD,
     *           IIPP(MM,I2),IIPP(MI,I2),IIPP(MD,I2),
     *           IIPP(IM,I2),IIPP(II,I2),IIPP(ID,I2),
     *           IIPP(DM,I2),IIPP(DI,I2),IIPP(DD,I2),
     *           IIPX(ZM,I2),IIPX(ZI,I2),IIPX(ZD,I2))
              Call Ncode(CPMA(K1),JM,JI,JD)

   19      Continue

* - circular extensions

           If(LPCI) then
                 L1=K1-LPRF
                 Call Dcode(CPMA(L1),LM,LI,LD)
                 If(IOPM( 0).LT.IOPM(LPRF)) then
                    IOPM( 0)=IOPM(LPRF)
                 LM=JM 
                 End if 
                 If(IOPI( 0).LT.IOPI(LPRF)) then
                    IOPI( 0)=IOPI(LPRF)
                    LI=JI 
                 End if 
                 If(IOPD( 0).LT.IOPD(LPRF)) then
                    IOPD( 0)=IOPD(LPRF)
                    LD=JD 
                 End if 
                 Call Ncode(CPMA(L1),LM,LI,LD)

              Do I2=1,LPRF
                 L1=L1+1
                 KD=IOPD(I2-1)+IMPP( D,I2)
                 If(IOPD(I2).GE.KD+IIPP(DD,I2)) then
                    Go to  20
                 Else
                    Call Dcode(CPMA(L1),LM,LI,LD)
                    If(IOPM(I2).LT.KD+IIPP(DM,I2)) then
                       IOPM(I2)=KD+IIPP(DM,I2)
                       LM=3
                    End if 
                    If(IOPI(I2).LT.KD+IIPP(DI,I2)) then
                       IOPI(I2)=KD+IIPP(DI,I2)
                       LI=3
                    End if 
                       IOPD(I2)=KD+IIPP(DD,I2)
                       LD=3
                    Call Ncode(CPMA(L1),LM,LI,LD)
                 End if
              End do
   20         Continue
           End if
* -----------------------------------------------------------
C          Call wtrace(IDMP,IDMM,LPRF,CPMA,K1-LPRF,IOPM,IOPI,IOPD)

   25   Continue

* end of sequence 

           If(JALE.EQ.LSEQ) then
              MZ=MY
              IZ=IY
              DZ=DY
           End if  

           If(LCKS(JALE)) then
                 IOPM(N1-1)=NLOW
              Do I2=N1,N2-1
                 IOPM(I2)=NLOW
                 IOPI(I2)=NLOW
              End do 
           End if

              J1=ISEQ(JALE)
              K1=K1+1
              I2=0

              KM=NLOW
              KI=IOPI( 0)+IIPP(J1, 0)
              KD=NLOW 

              KOPM=IOPM( 0)

              Call MaxP
     *          (IOPM(I2),IOPI(I2),IOPD(I2),JM,JI,JD,KM,KI,KD,
     *           IIPP(MM,I2),IIPP(MI,I2),IIPP(MD,I2),
     *           IIPP(IM,I2),IIPP(II,I2),IIPP(ID,I2),
     *           IIPP(DM,I2),IIPP(DI,I2),IIPP(DD,I2),
     *           IIPX(ZM,I2),IIPX(ZI,I2),IIPX(ZD,I2))
              Call Ncode(CPMA(K1),JM,JI,JD)

              I2=0
              If(KI+IIPX(IZ,I2).GE.IOPT) then
                 JS=1
                 Go to  50
              End if

           Do  29 I2=1,LPRF

              K1=K1+1

              KM=KOPM      +IMPP(J1,I2)
              KI=IOPI(I2  )+IIPP(J1,I2)
              KD=IOPD(I2-1)+IMPP( D,I2)

              KOPM=IOPM(I2)

              Call MaxP
     *          (IOPM(I2),IOPI(I2),IOPD(I2),JM,JI,JD,KM,KI,KD,
     *           IIPP(MM,I2),IIPP(MI,I2),IIPP(MD,I2),
     *           IIPP(IM,I2),IIPP(II,I2),IIPP(ID,I2),
     *           IIPP(DM,I2),IIPP(DI,I2),IIPP(DD,I2),
     *           IIPX(ZM,I2),IIPX(ZI,I2),IIPX(ZD,I2))
              Call Ncode(CPMA(K1),JM,JI,JD)

              If     (KI+IIPX(IZ,I2).GE.IOPT) then 
                 JS=1
                 Go to  50
              Else if(KM+IIPX(MZ,I2).GE.IOPT) then 
                 JS=2
                 Go to  50
              Else if(KD+IIPX(DZ,I2).GE.IOPT) then 
                 JS=3
                 Go to  50
              End if 

   29      Continue

* - circular extensions

           If(LPCI) then
                 L1=K1-LPRF
                 Call Dcode(CPMA(L1),LM,LI,LD)
                 If(IOPD( 0).LT.IOPD(LPRF)) then
                    IOPD( 0)=IOPD(LPRF)
                    LD=JD 
                 End if 
                 Call Ncode(CPMA(L1),LM,LI,LD)

              Do I2=1,LPRF
                 L1=L1+1
                 KD=IOPD(I2-1)+IMPP( D,I2)
                 If(KD+IIPX(DZ,I2).GE.IOPT) then
                    JS=3
                    K1=L1
                 End if 
                 If(IOPD(I2).GE.KD+IIPP(DD,I2)) then
                    Go to  30
                 Else
                    Call Dcode(CPMA(L1),LM,LI,LD)
                    IOPD(I2)=KD+IIPP(DD,I2)
                    LD=3
                    Call Ncode(CPMA(L1),LM,LI,LD)
                 End if
              End do
   30         Continue
           End if
* -----------------------------------------------------------

   50   Continue

C          Call wtrace(IDMP,IDMM,LPRF,CPMA,K1-LPRF,IOPM,IOPI,IOPD)

        K2=JALE
        K3=I2
        IPME=K3-LPRF-1

* trace back

           J1=0
        Do  I1=LPRF,K3+1,-1
           J1=J1+1
           CALI(J1)='-'
        End do

   60   Continue
C       Write(6,'(5I5)') K1,K2,K3,JS
        If     (JS.EQ.1) then 
           J1=J1+1
           CALI(J1)=Char(Ichar(CABC(ISEQ(K2)))+32)
           K1=K1-LPRF-1
           K2=K2-1
        Else if(JS.EQ.2) then
           J1=J1+1
           CALI(J1)=CABC(ISEQ(K2))
           K1=K1-LPRF-2
           K2=K2-1
           K3=K3-1
           If(LPCI.AND.K3.EQ.0) then 
              K3=K3+LPRF
              K1=K1+LPRF
           End if
        Else if(JS.EQ.3) then 
           J1=J1+1
           CALI(J1)='-'
           K1=K1-1
           K3=K3-1
           If(LPCI.AND.K3.EQ.0) then 
              K3=K3+LPRF
              K1=K1+LPRF
           End if
        End if 

        Call Dcode(CPMA(K1),JM,JI,JD)
        If     (JS.EQ.1) then 
           JS=JI
        Else if(JS.EQ.2) then
           JS=JM
        Else if(JS.EQ.3) then
           JS=JD
        End if

        If(JS.NE.0) Go to 60

        If(K3.GE.LPRF) K3=0

        Do I1=K3,1,-1
           J1=J1+1
           CALI(J1)='-'
        End do

        LALI=J1

* reverse alignment

           J1=(LALI+1)/2
        Do I1=LALI/2+1,LALI 
           CH=CALI(I1)
           CALI(I1)=CALI(J1)
           CALI(J1)=CH         
           J1=J1-1
        End do

        IPMB=K3+1

  100   Return

        End 
*----------------------------------------------------------------------*
        Subroutine MaxP
     *    (IOPM,IOPI,IOPD,JM,JI,JD,KM,KI,KD,
     *     IMM,IMI,IMD,IIM,III,IID,
     *     IDM,IDI,IDD,IZM,IZI,IZD)

           JMM=KM+IMM
           JMI=KM+IMI
           JMD=KM+IMD
           JIM=KI+IIM
           JII=KI+III
           JID=KI+IID
           JDM=KD+IDM
           JDI=KD+IDI
           JDD=KD+IDD

           IOPM=IZM
           JM=0
           IOPI=IZI
           JI=0
           IOPD=IZD
           JD=0

        If(JIM.GT.IOPM) then
           IOPM=JIM
           JM=1
        End if
        If(JMM.GT.IOPM) then
           IOPM=JMM
           JM=2
        End if
        If(JDM.GT.IOPM) then
           IOPM=JDM
           JM=3
        End if

        If(JII.GT.IOPI) then
           IOPI=JII
           JI=1
        End if
        If(JMI.GT.IOPI) then
           IOPI=JMI
           JI=2
        End if
        If(JDI.GT.IOPI) then
           IOPI=JDI
           JI=3
        End if

        If(JID.GT.IOPD) then
           IOPD=JID
           JD=1
        End if
        If(JMD.GT.IOPD) then
           IOPD=JMD
           JD=2
        End if
        If(JDD.GT.IOPD) then
           IOPD=JDD
           JD=3
        End if

        Return
        End
*----------------------------------------------------------------------*
        Subroutine Ncode(CPMA,JM,JI,JD)

        Character         CPMA

        CPMA=Char(JM*16+JI*4+JD) 
        Return 
        End 
*----------------------------------------------------------------------*
        Subroutine Dcode(CPMA,JM,JI,JD)

        Character         CPMA

        JD=Ichar(CPMA)
        JM=JD/16
        JD=JD-JM*16
        JI=JD/ 4
        JD=JD-JI* 4
        Return 
        End 
*----------------------------------------------------------------------*
        Subroutine wtrace(IDMP,IDMM,LPRF,CPMA,K1,IOPM,IOPI,IOPD)

        Character CPMA(IDMM)
        Integer   IOPM(0:IDMP)
        Integer   IOPI(0:IDMP)
        Integer   IOPD(0:IDMP)

        Integer   IM(20)
        Integer   II(20)
        Integer   ID(20)

           J1=1
        Do I1=K1,K1+LPRF
           Call Dcode(CPMA(I1),IM(J1),II(J1),ID(J1))
           J1=J1+1
        End do

        Write(6,'(I4)') K1/(LPRF+1)
        Write(6,'(20(I4,I2))')(IOPM(ii1-1),IM(ii1),ii1=1,LPRF+1)
        Write(6,'(20(I4,I2))')(IOPI(ii1-1),II(ii1),ii1=1,LPRF+1)
        Write(6,'(20(I4,I2))')(IOPD(ii1-1),ID(ii1),ii1=1,LPRF+1)
        Return
        End

