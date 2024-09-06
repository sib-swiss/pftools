*       Version:  This file is part of pftools release 1.0 January 1996
*----------------------------------------------------------------------*     
        Subroutine REPRF
     *    (NPRF,FPRF,
     *     CPID,CPAC,CPDE,NABC,CABC,LPRF,LPCI,
     *     CDIS,JDIP,MDIS,NDIP,
     *     CNOR,JNOP,JNOR,MNOR,NNOR,NNPR,CNTX,RNOP,
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT,
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

* profile data fields

        Character*(*)     FPRF

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'dfdat.f'
        Include          'pfdat.f'
        Include          'sterr.f'

        Character*132     RCIN

* work fields

        Integer           NKEY 
        Character*16      CPAR
        Character*256     CVAL 
        Logical           LNEW
        Integer           ISCO(26)

        Character*64      CH64
        
        IRC=0
        RCIN=' '

* open profile file 

        If(FPRF.NE.'-'.OR.NPRF.NE.5)
     *     Open(NPRF,File=FPRF,Status='OLD',Err=999)

* profile-id 
       
    1   Read(NPRF,'(A)',Err=999,End=901) RCIN
        If(RCIN(1:2).NE.'ID') go to   1   
        LR=Lblnk(RCIN)
        IC=Index(RCIN,';')
        CPID=RCIN( 6:IC-1)
        If(Index(RCIN(IC:LR),'MATRIX').EQ.0) go to   1 

* ac-number 
       
    2   Read(NPRF,'(A)',Err=999,End=999) RCIN
        If(RCIN(1:2).EQ.'//') go to 999 
        If(RCIN(1:2).NE.'AC') go to   2   
        IX=Index(RCIN,';')-1
        CPAC=RCIN( 6:IC-1)
        CPAC=RCIN(6:IX) // '|'

* description 

    3   Read(NPRF,'(A)',Err=999,End=999) RCIN
        If(RCIN(1:2).EQ.'//') go to 999
        If(RCIN(1:2).NE.'DE') go to   3   
        LR=Lblnk(RCIN)
        CPDE=RCIN( 6:LR)

* go to first MA line

    5   Read(NPRF,'(A)',Err=999,End=999) RCIN
        If(RCIN(1:2).EQ.'//') go to   1 
        If(RCIN(1:2).NE.'MA') go to   5   
        LR=Lblnk(RCIN)

C       Print *,CPID
C       Print *,CPAC
C       Print *,CPDE

* initialize position-independent profile parameters

* - general specifications

        CVAL='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
        NABC=26
        Read(CVAL,'(26A)')(CABC(ii1),ii1=1,NABC)

        LPRF=0
        LENP=0

        LPCI=.FALSE.

* - disjoint mode

        MDIS=1

* - normalization modes

        JNOR=0 
        Do  11 I1=1,MAXN
           NNOR(I1)=I1
           NNPR(I1)=I1
   11   Continue

* - cut-off

        JCUT=0

* - defaults for match and insert position  

        JI=-1
        JM=0

        CHID='-'
        Do  15 I1=1,26 
           IIPD(I1)=0
   15   Continue
        
        IIPD(B0)=0
        IIPD(B1)=0
        IIPD(E0)=0
        IIPD(E1)=0

        IIPD(BM)=0
        IIPD(BI)=NLOW
        IIPD(BD)=NLOW
        IIPD(BE)=NLOW
        IIPD(MM)=0
        IIPD(MI)=NLOW
        IIPD(MD)=NLOW
        IIPD(ME)=0
        IIPD(IM)=NLOW
        IIPD(II)=0
        IIPD(ID)=NLOW
        IIPD(IE)=NLOW
        IIPD(DM)=NLOW
        IIPD(DI)=NLOW
        IIPD(DD)=0
        IIPD(DE)=NLOW

        IIPD(I0)=0

        CHMD='X'
        Do  16 I1=1,26 
           IMPD(I1)=0
   16   Continue

        IMPD(M0)=0 
        IMPD(D )=0

        Do  18 I1=0,27
           IMPP(I1,0)=NLOW
   18   Continue 

* intialize profile input buffers.

        LR=Lblnk(RCIN)
        If(Ichar(RCIN(LR:LR)).EQ.13)LR=LR-1 
        JR=5

* read next parameter 

   20   Continue
        Call NEXTP(NPRF,RCIN,LR,JR,NKEY,LNEW,CPAR,CVAL,IRC) 
        If(IRC.EQ.-1) go to  90
        If(IRC.NE.0 ) go to 999

* interpret parameters

* - GENERAL_SPEC:

        If     (NKEY.EQ.1) then 

           If(CPAR.EQ.'ALPHABET') then
              Read(CVAL,*,Err=999) CH64
              NABC=Lblnk(CH64)
              Read(CH64,'(64A)',Err=999)(CABC(ii1),ii1=1,NABC)
           End if
   
           If(CPAR.EQ.'LENGTH') then
              Read(CVAL,*,Err=999) LENP
           End if

           If(CPAR.EQ.'TOPOLOGY'.AND.CPAR.EQ.'CIRCULAR') then 
              LPCI=.TRUE.
           End if 
           
* - DISJOINT:

        Else if(NKEY.EQ.2) then 

           If     (CPAR.EQ.'DEFINITION') then 
              Do  31 I1=1,KDIS
                 If(CVAL.EQ.CDIS(I1)) MDIS=I1
   31         Continue
           Else
              If     (CPAR.EQ.'N1') then
                 Read(CVAL,*,Err=999) NDIP(1) 
              Else if(CPAR.EQ.'N2') then
                 Read(CVAL,*,Err=999) NDIP(2) 
              End if
           End if           

* - NORMALIZATION:

        Else if(NKEY.EQ.3) then 

           If(LNEW) then
              JNOR=JNOR+1
              CNTX(JNOR)=' '
           End if

           If     (CPAR.EQ.'FUNCTION') then
              If(CVAL.EQ.'GRIBSKOV') CVAL='GLE_ZSCORE'
              Do  41 I1=1,KNOR
                 If(CVAL.EQ.CNOR(I1)) MNOR(JNOR)=I1
   41         Continue
           Else if(CPAR.EQ.'MODE') then
              Read(CVAL,*,Err=999) NNOR(JNOR)
           Else if(CPAR.EQ.'PRIORITY') then
              Read(CVAL,*,Err=999) NNPR(JNOR)
           Else if(CPAR.EQ.'TEXT') then
              Read(CVAL,*,Err=999) CNTX(JNOR)
           Else
              If     (CPAR.EQ.'R1') then
                 Read(CVAL,*,Err=999) RNOP(1,JNOR) 
              Else if(CPAR.EQ.'R2') then
                 Read(CVAL,*,Err=999) RNOP(2,JNOR) 
              Else if(CPAR.EQ.'R3') then
                 Read(CVAL,*,Err=999) RNOP(3,JNOR) 
              Else if(CPAR.EQ.'R4') then
                 Read(CVAL,*,Err=999) RNOP(4,JNOR) 
              Else if(CPAR.EQ.'R5') then
                 Read(CVAL,*,Err=999) RNOP(5,JNOR) 
              End if
           End if

* - CUT_OFF:

        Else if(NKEY.EQ.4) then 

           If(LNEW) then 
              JCUT=JCUT+1
              MCLE(JCUT)=0
              CCUT(JCUT)=' '
           End if

           If     (CPAR.EQ.'LEVEL') then
              Read(CVAL,*,Err=999) MCLE(JCUT)
           Else if(CPAR.EQ.'SCORE') then
              Read(CVAL,*,Err=999) ICUT(JCUT)
           Else if(CPAR.EQ.'TEXT') then
              Read(CVAL,*,Err=999) CCUT(JCUT)
           Else if(CPAR.EQ.'N_SCORE') then
              L1=Lblnk(CVAL) 
                 J1=1
              Do  51 I1=1,L1
                 If(CVAL(I1:I1).EQ.',') J1=J1+1
   51         Continue
              Read(CVAL,*,Err=999)(RCUT(ii1,JCUT),ii1=1,J1)
              JCNM(JCUT)=J1
           Else if(CPAR.EQ.'MODE') then
              L1=Lblnk(CVAL) 
                 J1=1
              Do  52 I1=1,L1
                 If(CVAL(I1:I1).EQ.',') J1=J1+1
   52         Continue
              Read(CVAL,*,Err=999)(MCUT(ii1,JCUT),ii1=1,J1)
           End if

* - DEFAULT:

        Else if(NKEY.EQ.5) then 

           If     (CPAR.EQ.'SY_I') then
              Read(CVAL,*,Err=999) CHID 
           Else if(CPAR.EQ.'SY_M') then
              Read(CVAL,*,Err=999) CHMD
           Else if(CPAR.EQ.'M0'.OR.CPAR.EQ.'M'.OR.CPAR.EQ.'D') then
              Call GETPM(CPAR,CVAL,INDX,ISCO,NABC,NLOW,IRC)
              If(IRC.NE.0) Go to 999
              If(INDX.EQ.1) then
                 Do  61 I1=1,NABC
                    IMPD(I1)=ISCO(I1)
   61            Continue
              Else
                 IMPD(INDX)=ISCO(1)
              End if
           Else
              Call GETPI(CPAR,CVAL,INDX,ISCO,NABC,NLOW,IRC)
              If(IRC.NE.0) Go to 999
              If(INDX.EQ.1) then
                 Do  62 I1=1,NABC
                    IIPD(I1)=ISCO(I1)
   62            Continue
              Else
                 IIPD(INDX)=ISCO(1)
              End if
           End if
 
* - I:

        Else if(NKEY.EQ.6) then 

           If(LNEW) then
                 JI=JI+1
                    CHIP(JI)=CHID 
                 Do  71 I1=0,46
                    IIPP(I1,JI)=IIPD(I1)
   71            Continue
              If(JI.GT.JM) then
                 JM=JM+1
                    CHMP(JM)=CHMD 
                 Do  72 I1=0,27
                    IMPP(I1,JM)=IMPD(I1)
   72            Continue
              End if
           End if 

           If     (CPAR.EQ.' ') then
              Go to  20
           Else if(CPAR.EQ.'SY') then
              Read(CVAL,*,Err=999) CHIP(JI) 
           Else
              Call GETPI(CPAR,CVAL,INDX,ISCO,NABC,NLOW,IRC)
              If(IRC.NE.0) Go to 999
              If(INDX.EQ.1) then
                 Do  73 I1=1,NABC
                    IIPP(I1,JI)=ISCO(I1)
   73            Continue
              Else
                 IIPP(INDX,JI)=ISCO(1)
              End if
           End if

* - M:

        Else if(NKEY.EQ.7) then 

           If(LNEW) then
                 JM=JM+1
                    CHMP(JM)=CHMD 
                 Do  81 I1=0,27
                    IMPP(I1,JM)=IMPD(I1)
   81            Continue
              If(JM-1.GT.JI) then
                 JI=JI+1
                    CHIP(JI)=CHID 
                 Do  82 I1=0,46
                    IIPP(I1,JI)=IIPD(I1)
   82            Continue
              End if
           End if 

           If     (CPAR.EQ.' ') then
              Go to  20
           Else if(CPAR.EQ.'SY') then
              Read(CVAL,*,Err=999) CHMP(JM) 
           Else
              Call GETPM(CPAR,CVAL,INDX,ISCO,NABC,NLOW,IRC)
              If(IRC.NE.0) Go to 999
              If(INDX.EQ.1) then
                 Do  83 I1=1,NABC
                    IMPP(I1,JM)=ISCO(I1)
   83            Continue
              Else
                 IMPP(INDX,JM)=ISCO(1)
              End if
           End if
        Else 
           Continue
        End if  

C       Print *,JI,JM,NKEY,' ',LNEW,' ',
C    *        CPAR(1:Lblnk(CPAR)),'=',
C    *        CVAL(1:Lblnk(CVAL))

        Go to  20

   90   Continue
        IRC=0

* last insert position 

        If(LPCI) then
          If(JI.EQ.JM) then 
                 CHIP(    0)=CHIP(   JI)
              Do  91 I1=0,46
                 IIPP(I1, 0)=IIPP(I1,JI)
   91         Continue
           Else
              JI=JI+1
                 CHIP(   JI)=CHIP(    0)
              Do  92 I1=0,46
                 IIPP(I1,JI)=IIPP(I1, 0)
   92         Continue
           End if
        Else if(JI.LT.JM) then 
              JI=JI+1
                 CHIP(   JI)=CHID
              Do  93 I1=0,46
                 IIPP(I1,JI)=IIPD(I1)
   93         Continue
        End if

        LPRF=JI
           
  100   Return
  901   IRC=-1
        Go to 100
  999   Write(NERR,*)  'Profile format error, last record read: ',
     *     RCIN(1:Lblnk(RCIN))
           If(IRC.EQ.0) IRC=1
        Go to 100
        End
*----------------------------------------------------------------------*
        Subroutine NEXTP(NPRF,RCIN,LR,JR,NKEY,LNEW,CPAR,CVAL,IRC) 

        Character*(*)        RCIN
        Character*(*)        CPAR 
        Character*(*)        CVAL 
        Logical              LNEW 

        LNEW=.FALSE.

    3   Do  4 I1=JR+1,LR
           JR=JR+1
           If(Index(' ;,.	',RCIN(JR:JR)).EQ.0) go to 10
    4   Continue

    5   Read(NPRF,'(A)',Err=999,End=101) RCIN
        If(RCIN(1:2).EQ.'CC') go to   5 
        LR=Lblnk(RCIN)
        If(Ichar(RCIN(LR:LR)).EQ.13) LR=LR-1 
        If(RCIN(1:2).NE.'MA') go to 101
        JR=5
        Go to   3

* identify keyword 

   10   If(RCIN(JR:JR).EQ.'/') then 
           LNEW=.TRUE.
           If     (RCIN(JR:JR+13).EQ.'/GENERAL_SPEC:') then
              NKEY=1
              JR=JR+13
           Else if(RCIN(JR:JR+ 9).EQ.'/DISJOINT:') then
              NKEY=2
              JR=JR+ 9
           Else if(RCIN(JR:JR+14).EQ.'/NORMALIZATION:') then
              NKEY=3
              JR=JR+14
           Else if(RCIN(JR:JR+ 8).EQ.'/CUT_OFF:') then
              NKEY=4
              JR=JR+ 8
           Else if(RCIN(JR:JR+ 8).EQ.'/DEFAULT:') then
              NKEY=5
              JR=JR+ 8
           Else if(RCIN(JR:JR+ 2).EQ.'/I:') then
              NKEY=6
              JR=JR+ 2
           Else if(RCIN(JR:JR+ 2).EQ.'/M:') then
              NKEY=7
              JR=JR+ 2
           Else
              Go to 999
           End if
        Else 
           JR=JR-1
        End if 

* read parameter 

        LPAR=0
        CPAR=' '  

   20   JR=JR+1  

        If(JR.GT.LR) then 
   21      Read(NPRF,'(A)',Err=999,End=101) RCIN
           If(RCIN(1:2).EQ.'CC') go to  21
           LR=Lblnk(RCIN)
           If(Ichar(RCIN(LR:LR)).EQ.13) LR=LR-1 
           If(RCIN(1:2).NE.'MA') then 
              JR=LR
              CVAL=' '
              Go to  100
           End if
           JR=6
        End if  

        If(RCIN(JR:JR).EQ.'/') then
           JR=JR-1
           CVAL=' '
           Go to  100
        End if   

        If(RCIN(JR:JR).NE.'=') then
           If(LPAR.NE.0.OR.RCIN(JR:JR).NE.' ') then 
              LPAR=LPAR+1
              CPAR(LPAR:LPAR)=RCIN(JR:JR)
           End if 
           Go to  20
        End if

* read value 

        LVAL=0
        CVAL=' '  

   30   JR=JR+1  

        If(JR.GT.LR) then 
   31      Read(NPRF,'(A)',Err=999,End=101) RCIN
           If(RCIN(1:2).EQ.'CC') go to  31
           LR=Lblnk(RCIN)
           If(Ichar(RCIN(LR:LR)).EQ.13) LR=LR-1
           If(RCIN(1:2).NE.'MA') go to 101
           JR=6
        End if  

        If(Index('/;',RCIN(JR:JR)).EQ.0) then
           If(LVAL.NE.0.OR.RCIN(JR:JR).NE.' ') then 
              LVAL=LVAL+1
              CVAL(LVAL:LVAL)=RCIN(JR:JR)
           End if
           Go to  30
        End if
*  
  100   Return
  101   IRC=-1
        Go to 100
  999   IRC= 1
        Go to 100

        End
*----------------------------------------------------------------------*
        Integer Function PINTR(CVAL,NLOW)

        Integer           NLOW
        Character*(*)     CVAL 

           If(Index(CVAL,'*').NE.0) then
              PINTR=NLOW
           Else     
              Read(CVAL,*,Err= 90) PINTR
           End if
   90   Continue
  100   Return
        End
*----------------------------------------------------------------------*
        Subroutine PINTS(NSCO,ISCO,CVAL,NLOW)

        Integer           ISCO(*)
        Character*(*)     CVAL 
        Integer           NLOW

        L1=Lblnk(CVAL) 
        NSCO=0
        J1=1
        Do  10 I1=1,L1
           If(CVAL(I1:I1).EQ.',') then
              J2=I1-1
              NSCO=NSCO+1
              If(J2.LT.J1) then 
                 ISCO(NSCO)=0 
              Else
                 If(Index(CVAL(J1:J2),'*').NE.0) then
                    ISCO(NSCO)=NLOW
                 Else
                    Read(CVAL(J1:J2),*,Err= 90) ISCO(NSCO) 
                 End if
              End if 
              J1=I1+1
           End if
   10   Continue
              J2=L1
              NSCO=NSCO+1
              If(J2.LT.J1) then 
                 ISCO(NSCO)=0 
              Else
                 If(Index(CVAL(J1:J2),'*').NE.0) then
                    ISCO(NSCO)=NLOW
                 Else
                    Read(CVAL(J1:J2),*,Err= 90) ISCO(NSCO) 
                 End if
              End if 
                    
   90   Continue
        Return
        End
*----------------------------------------------------------------------*
        Subroutine GETPI(CPAR,CVAL,INDX,ISCO,NABC,NLOW,IRC)

        Character*(*)     CPAR
        Character*(*)     CVAL
        Integer           ISCO(*)
        Integer           NLOW
        Integer           PINTR

        If     (CPAR.EQ.'I0') then
           INDX= 0
        Else if(CPAR.EQ.'I' ) then
           INDX= 1
        Else if(CPAR.EQ.'B0') then
           INDX=27 
        Else if(CPAR.EQ.'B1') then
           INDX=28
        Else if(CPAR.EQ.'E0') then
           INDX=29
        Else if(CPAR.EQ.'E1') then
           INDX=30

        Else if(CPAR.EQ.'BM') then
           INDX=31
        Else if(CPAR.EQ.'BI') then
           INDX=32
        Else if(CPAR.EQ.'BD') then
           INDX=33
        Else if(CPAR.EQ.'BE') then
           INDX=34

        Else if(CPAR.EQ.'MM') then
           INDX=35
        Else if(CPAR.EQ.'MI') then
           INDX=36
        Else if(CPAR.EQ.'MD') then
           INDX=37
        Else if(CPAR.EQ.'ME') then
              INDX=38

        Else if(CPAR.EQ.'IM') then
           INDX=39
        Else if(CPAR.EQ.'II') then
           INDX=40
        Else if(CPAR.EQ.'ID') then
           INDX=41
        Else if(CPAR.EQ.'IE') then
           INDX=42

        Else if(CPAR.EQ.'DM') then
           INDX=43
        Else if(CPAR.EQ.'DI') then
           INDX=44
        Else if(CPAR.EQ.'DD') then
           INDX=45
        Else if(CPAR.EQ.'DE') then
           INDX=46
        Else 
           Go to 900
        End if 

        If(INDX.EQ.1) then
           Call PINTS(NSCO,ISCO,CVAL,NLOW)
           If(NSCO.EQ.1) then
              Do  10 I1=2,NABC
                 ISCO(I1)=ISCO( 1)
   10         Continue
           End if
        Else
           ISCO(1)=PINTR(CVAL,NLOW)
        End if 

  100   Return 
  900   IRC=-1
        Go to 100
        End 
*----------------------------------------------------------------------*
        Subroutine GETPM(CPAR,CVAL,INDX,ISCO,NABC,NLOW,IRC)

        Character*(*)     CPAR
        Character*(*)     CVAL
        Integer           ISCO(*)
        Integer           NLOW
        Integer           PINTR

        If     (CPAR.EQ.'M0') then
           INDX= 0
        Else if(CPAR.EQ.'M' ) then
           INDX= 1
        Else if(CPAR.EQ.'D ') then
           INDX=27 
        Else
           Go to 900
        End if

        If(INDX.EQ.1) then
           Call PINTS(NSCO,ISCO,CVAL,NLOW)
           If(NSCO.EQ.1) then
              Do  10 I1=2,NABC
                 ISCO(I1)=ISCO( 1)
   10         Continue
           End if
        Else
           ISCO(1)=PINTR(CVAL,NLOW)
        End if 

  100   Return 
  900   IRC=-1
        Go to 100
        End 
