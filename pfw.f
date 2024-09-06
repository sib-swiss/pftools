*       Program pfw
*----------------------------------------------------------------------*     
*       Function: Caluclate weights for individual sequences of a 
*                 multiple sequence alignments.      
*       Author:   Philipp Bucher
*       Version:  This file is part of pftools release 2.2 June 1999
*----------------------------------------------------------------------*     
* DATA
*----------------------------------------------------------------------*

* array sizes, I/O units

C       Parameter        (IDM1=16777216)
        Parameter        (IDM1=1048576)
        Parameter        (IDM2=   9999)
        Parameter        (IDM3=   2024)

        Parameter        (NMSF=     11)

        Include          'sterr.f'

* weights and distances 

        Real              RWGT(IDM3)
        Integer           NDIS(IDM3) 
        Integer           NIND(IDM3) 

* multiple sequence alignment:

        Character*64      FMSF

        Character*32      SQID(IDM3)
        Character         CSEQ(IDM1)
        Character         CSQR(IDM2)

        Logical           LXSC(IDM2)

* character set 

        Character         CSET(26,IDM3)
        Integer           NSET(   IDM3)

* work fields 

        Character*256     RCIO
        Character         B

* functions

        Integer           Getc
C       Integer           Fputc

* character translation 

        Character*27      ABCU
        Character*27      ABCL

        Data              ABCU/'ABCDEFGHIJKLMNOPQRSTUVWXYZ-'/
        Data              ABCL/'abcdefghijklmnopqrstuvwxyz.'/

*----------------------------------------------------------------------*
* INPUT SECTION
*----------------------------------------------------------------------*

        IRC=0
        IDUM=-2

*read command line 

        Call Repar
     *    (FMSF,NRAN,RX,RW,IRAN,IRC) 
        If(IRC.NE.0) then
           Write(NERR,'(
     *      ''Usage: pfw [ msf-file | - ] [N=#] [X=#] [R=#] [W=#]'',/
     *     ,''       N=mumber of shuffles per sequence (integer)'',/
     *     ,''       X=gap excisions threshold (0 < X <= 1)'',/
     *     ,''       R=random number seed (negative integer)'',/ 
     *     ,''       W=total weight (positive real)''
     *        )')
           Stop
        End if

* read msf-file

C       If(FMSF.EQ.'-') then 
C   1      Open(NMSF,Status='SCRATCH') 
C          Do I1=1,2*IDM1
C             If(Getc(B).NE.0) go to   2
C             N1=Fputc(NMSF,B) 
C          End do 
C   2      Rewind(NMSF)
C       End if 

        If(FMSF.EQ.'-') then 
    1      Open(NMSF,Status='SCRATCH')
    2      Continue 
           Do I1=1,512
              If(Getc(B).NE.0) go to 3
              If(Ichar(B).EQ.10) then
                 Write(NMSF,'(512A)')(CSEQ(ii1),ii1=1,I1-1)
                 Go to   2   
              Else 
                 CSEQ(I1)=B
              End if
           End do  
           Go to   2
    3      Rewind(NMSF)

        End if 

        Call REMSF
     *    (NERR,NMSF,FMSF,
     *     IDM1,CSEQ,NSEQ,LSEQ,
     *     IDM3,RWGT,SQID,
     *     IRC)
        If(IRC.NE.0) go to 100

*----------------------------------------------------------------------*
* DATA PROCESSING
*----------------------------------------------------------------------*

* make sequence uppercase 

        Do I1=1,LSEQ*NSEQ
           If     (Index(ABCU,CSEQ(I1)).NE.0) then 
              Continue
           Else
              IX=Index(ABCL,CSEQ(I1))
              If(IX.GT.0) then
                 CSEQ(I1)=ABCU(IX:IX)
              Else 
                 CSEQ(I1)='-'
              End if
           End if
        End do

* gap escision

        NXSC=INT(RX*NSEQ)

        Do I1=1,LSEQ
           J1=I1
           K1=0
           Do I2=1,NSEQ
              If(CSEQ(J1).EQ.'-') K1=K1+1
              J1=J1+LSEQ
           End do
           If(K1.LE.NXSC) then 
              LXSC(I1)=.TRUE.
           Else
              LXSC(I1)=.FALSE.
           End if
        End do

* generate character set profile 

        Call GCSET
     *    (IDM1,CSEQ,NSEQ,LSEQ,
     *     IDM2,NSET,CSET) 

* major loop 

* - initialize new weights

*       Do  10 I1=1,NSEQ
           RWGT(I1)=0
   10   Continue 

        Do  20 I1=1,NRAN*NSEQ

* - generate random sequences 

           Call RanSQ(IRAN,IDM2,NSET,CSET,LSEQ,CSQR)

* - compare random sequence to real sequences
*
*      NDIS(i): distance of random sequence to real sequence i  
*      MIND   : minimal distance 
*      KMIN   : # of real sequences with minimal distanc
*      NIND(i): indices of real sequences with minimal distance   

                 J3=1 
                 MIND=LSEQ
                 KMIN=0
           Do  15 I2=1,NSEQ
                 NDIS(I2)=0
              Do  12 I3=1,LSEQ
                 If(LXSC(I3)) then
                    If(CSQR(I3).NE.CSEQ(J3)) NDIS(I2)=NDIS(I2)+1
                 End if
                 J3=J3+1
   12         Continue
                 If     (NDIS(I2).LT.MIND) then
                    MIND=NDIS(I2)
                    KMIN=1
                    NIND(1)=I2
                 Else if(NDIS(I2).EQ.MIND) then
                    KMIN=KMIN+1
                    NIND(KMIN)=I2
                 End if 
   15      Continue

           R1=1.0/KMIN
           Do  16 I2=1,KMIN
              J2=NIND(I2)
              RWGT(J2)=RWGT(J2)+R1
   16      Continue

   20   Continue

        Do  30 I1=1,NSEQ
           RWGT(I1)=RWGT(I1)/(NSEQ*NRAN)
C          Write(6,'(A16,''Weight: '',F6.4)')
C    *        SQID(I1)(1:16),RWGT(I1)  
   30   Continue 

*----------------------------------------------------------------------*
* OUTPUT SECTION
*----------------------------------------------------------------------*

        Rewind(NMSF)

   51   Read(NMSF,'(A)',End=900) RCIO
        L=Lblnk(RCIO)
        Write(6,'(256A)')(RCIO(ii1:ii1),ii1=1,L)
        If(Index(RCIO(1:L),'..').EQ.0) go to  51
 
        K1=1
   52   Read(NMSF,'(A)',End=900) RCIO
        L=Lblnk(RCIO)
        IX=Index(RCIO(1:L),'Weight: ') 
        If(IX.NE.0) then 
           Write(RCIO(IX+8:),'(F6.4)') RW*RWGT(K1)
           L=IX+13 
           K1=K1+1
        End if   
        Write(6,'(256A)')(RCIO(ii1:ii1),ii1=1,L)
        If(RCIO(1:2).NE.'//') go to  52

   53   Read(NMSF,'(A)',End=100) RCIO
        L=Lblnk(RCIO)
        Write(6,'(256A)')(RCIO(ii1:ii1),ii1=1,L)
        Go to  53 

  100   Stop
  900   Go to 100 
        End
*----------------------------------------------------------------------*
        Subroutine Repar
     *    (FMSF,NRAN,RX,RW,IRAN,IRC) 

        Character*64      FMSF 
        Character*64      CPAR

        NRAN=100
        RX=0.5
        IRAN=-123456789
        RW=1.0
        
        N1=Iargc()
        If(N1.LT.1) go to 900  
 
        Call GetArg(1,FMSF)

        Do  50 I1=1,N1
 
           Call GetArg(I1,CPAR)
 
           If     (CPAR(1:2).EQ.'N=') then
              Read(CPAR(3:64),*,Err=900) NRAN 
           Else if(CPAR(1:2).EQ.'X=') then
              Read(CPAR(3:64),*,Err=900) RX 
           Else if(CPAR(1:2).EQ.'R=') then
              Read(CPAR(3:64),*,Err=900) IRAN  
           Else if(CPAR(1:2).EQ.'W=') then
              Read(CPAR(3:64),*,Err=900) RW 
           End if

   50   Continue

        If(IRAN.GT.0) IRAN=-1-IRAN

  100   Return 
  900   IRC=1
        Go to 100
        End
*----------------------------------------------------------------------*
        Subroutine GCSET
     *    (IDM1,CSEQ,NSEQ,LSEQ,
     *     IDM2,NSET,CSET) 

        Character         CSEQ(   IDM1)
        Integer           NSET(   IDM2)
        Character         CSET(26,IDM2)
 
        Do  10 I1=1,LSEQ
              NSET(   I1)=0
              J2=I1-LSEQ
           Do   8 I2=1,NSEQ
              J2=J2+LSEQ
              Do   5 I3=1,NSET(I1)
                 If(CSEQ(J2).EQ.CSET(I3,I1)) go to   8 
    5         Continue
                 NSET(I1)=NSET(I1)+1 
                 CSET(NSET(I1),I1)=CSEQ(J2)
    8      Continue
   10   Continue

C       Do  20 I1=1,LSEQ
C          Write(6,'(I6,'' '',26A)')
C    *        NSET(I1),(CSET(ii1,I1),ii1=1,NSET(I1))
C  20   Continue

        Return
        End
*----------------------------------------------------------------------*
        Subroutine RanSQ(IRAN,IDM2,NSET,CSET,LSEQ,CSQR)

        Integer         NSET(   IDM2)
        Character       CSET(26,IDM2) 
        Character       CSQR(   IDM2)
        
        Do  10 I1=1,LSEQ
           If(NSET(I1).EQ.0) then
              CSQR(I1)='-'
           Else
              K1=RAN2(IRAN)*NSET(I1)+1
              CSQR(I1)=CSET(K1,I1)
           End if
   10   Continue

        Return
        End
*----------------------------------------------------------------------*
        FUNCTION RAN2(IDUM)
        PARAMETER (M=714025,IA=1366,IC=150889,RM=1.4005112E-6)
        DIMENSION IR(97)
        DATA IFF /0/
        IF(IDUM.LT.0.OR.IFF.EQ.0)THEN
          IFF=1
          IDUM=MOD(IC-IDUM,M)
          DO 11 J=1,97
            IDUM=MOD(IA*IDUM+IC,M)
            IR(J)=IDUM
   11     CONTINUE
          IDUM=MOD(IA*IDUM+IC,M)
          IY=IDUM
        ENDIF
        J=1+(97*IY)/M
        IF(J.GT.97.OR.J.LT.1) PAUSE
        IY=IR(J)
        RAN2=IY*RM
        IDUM=MOD(IA*IDUM+IC,M)
        IR(J)=IDUM
        RETURN
        END
*----------------------------------------------------------------------*
        Include          'remsf.f'
        Include          'lblnk.f'
