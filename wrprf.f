        Subroutine WRPRF
     *    (NOUT,FPRF,
     *     CPID,CPAC,CPDE,NABC,CABC,LPRF,LPCI,
     *     MDIS,NDIP,
     *     JNOR,MNOR,NNOR,NNPR,CNTX,RNOP, 
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT, 
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD, 
     *     IRC)

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'pfdat.f'
        Include          'dfdat.f'

* work fields

        Character*1024    CBLK
        Character*256     CPAR
        Character*80      RCEX

        Logical           LPRI

* initiation, transition and termination score names

        Character*2       CPSN(27:46)
           Data              CPSN(27)/'B0'/
           Data              CPSN(28)/'B1'/
           Data              CPSN(29)/'E0'/
           Data              CPSN(30)/'E1'/
           Data              CPSN(31)/'BM'/
           Data              CPSN(32)/'BI'/
           Data              CPSN(33)/'BD'/
           Data              CPSN(34)/'BE'/
           Data              CPSN(35)/'MM'/
           Data              CPSN(36)/'MI'/
           Data              CPSN(37)/'MD'/
           Data              CPSN(38)/'ME'/
           Data              CPSN(39)/'IM'/
           Data              CPSN(40)/'II'/
           Data              CPSN(41)/'ID'/
           Data              CPSN(42)/'IE'/
           Data              CPSN(43)/'DM'/
           Data              CPSN(44)/'DI'/
           Data              CPSN(45)/'DD'/
           Data              CPSN(46)/'DE'/
 
        IRC=0
 
* write header

* - ID line
 
        RCEX='ID   ' 
     *    // CPID(1:Lnblnk(CPID))
     *    // '; MATRIX.'
        Write(NOUT,'(78A)')(RCEX(ii1:ii1),ii1=1,Lnblnk(RCEX))

* - AC line 
 
        RCEX='AC   ' 
     *    // CPAC(1:Lnblnk(CPAC))
     *    // ';'
        Write(NOUT,'(78A)')(RCEX(ii1:ii1),ii1=1,Lnblnk(RCEX))

* - DE line
 
        RCEX='DE   ' 
     *    // CPDE(1:Lnblnk(CPDE))
        Write(NOUT,'(78A)')(RCEX(ii1:ii1),ii1=1,Lnblnk(RCEX))

* write /GENERAL_SPEC: block 

        CBLK='/GENERAL_SPEC:'   
        JB=14

* - alphapet
        
        Write(CPAR,'(''ALPHABET='''''',26A)')(CABC(ii1),ii1=1,NABC) 
        JP=12+NABC
        CPAR(JP-1:JP)=''';'
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1

* - length
        
        Write(CPAR,'(''LENGTH='',I6,'';'')') LPRF
        JP=14
        Call slpar(CPAR,JP)
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1

* - topology 

        If(LPCI) then
           CBLK=CBLK(1:JB) // ' TOPOLOGY=CIRCULAR;'
           JB=JB+19
        End if

        Call wrblk(NOUT,CBLK,JB)

* write /DISJOINT: block 

        CBLK='/DISJOINT:'
        JB=10

* - definition

        CPAR='DEFINITION=' // CDIS(MDIS)
        JP=Lnblnk(CPAR)+1
        CPAR(JP:JP)=';'
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1

* - parameters

        KDIP=JDIP(MDIS)
        If(KDIP.NE.0) then
           Write(CPAR,'(8('' N'',I1,''='',I6,'';''))')
     *       (ii1,NDIP(ii1),ii1=1,KDIP)
           JP=11*KDIP
           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

        Call wrblk(NOUT,CBLK,JB)

* write /NORMALIZATION: block

        LPRI=.FALSE.
        Do  21 I1=1,JNOR
           If(NNOR(I1).NE.NNPR(I1)) LPRI=.TRUE.
   21   Continue

        Do  30 I1=1,JNOR

        CBLK='/NORMALIZATION:'
        JB=15

* - mode 

        Write(CPAR,'(''MODE='',I6,'';'')') NNOR(I1) 
        JP=12
        Call slpar(CPAR,JP)
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1

* - priority

        If(LPRI) then
           Write(CPAR,'(''PRIORITY='',I6,'';'')') NNPR(I1) 
           JP=16
           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if         

* - function

        CPAR='FUNCTION=' // CNOR(MNOR(I1))
        JP=Lnblnk(CPAR)+1
        CPAR(JP:JP)=';'
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1

* - paramaters
 
        KNOP=JNOP(MNOR(I1))
        Write(CPAR,'(8('' R'',I1,''='',G12.5,'';''))')
     *    (ii1,RNOP(ii1,I1),ii1=1,KNOP)
        JP=17*KNOP
        Call slpar(CPAR,JP)
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1

* - text

        If(CNTX(I1).NE.' ') then 
           CPAR='TEXT=''' // CNTX(I1)(1:Lnblnk(CNTX(I1))) // ''''
           JP=Lnblnk(CPAR)+1
           CPAR(JP:JP)=';'
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
         End if

        Call wrblk(NOUT,CBLK,JB)

   30   Continue

* write /CUT_OFF: block

        Do  40 I1=1,JCUT

        CBLK='/CUT_OFF:'
        JB=9

* - level

        Write(CPAR,'(''LEVEL='',I6,'';'')') MCLE(I1) 
        JP=13
        Call slpar(CPAR,JP)
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1

* - score

        Write(CPAR,'(''SCORE='',I6,'';'')') ICUT(I1) 
        JP=13
        Call slpar(CPAR,JP)
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1

* - normalized scores
 
        KCNM=JCNM(I1)
        Write(CPAR,'(''N_SCORE='',G12.5,7('','',G12.5))')
     *    (RCUT(ii1,I1),ii1=1,KCNM)
        JP=8+13*KCNM
        CPAR(JP:JP)=';'
        Call slpar(CPAR,JP)
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1

* - normalization modes
 
        Write(CPAR,'(''MODE='',I6,7('','',I6))')
     *    (MCUT(ii1,I1),ii1=1,KCNM)
        JP=5+7*KCNM
        CPAR(JP:JP)=';'
        Call slpar(CPAR,JP)
        CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
        JB=JB+JP+1
       
* - text

        If(CCUT(I1).NE.' ') then 
           CPAR='TEXT=''' // CCUT(I1)(1:Lnblnk(CCUT(I1))) // ''''
           JP=Lnblnk(CPAR)+1
           CPAR(JP:JP)=';'
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
         End if

        Call wrblk(NOUT,CBLK,JB)

   40   Continue

* write /DEFAULT: block 

* - transfer defaults to pos. LPR+1

        I1=LPRF+1

        Do  51 I2=0,46
           IIPP(I2,I1)=IIPD(I2)
   51   Continue
           CHIP(I1)=CHID
        Do  52 I2=0,27
           IMPP(I2,I1)=IMPD(I2)
   52   Continue
           CHMP(I1)=CHMD

* - reinitialize defaults for match and insert position  

        CHID='-'
        Do  53 I2=1,26 
           IIPD(I2)=0
   53   Continue
        
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
        Do  54 I2=1,26 
           IMPD(I2)=0
   54   Continue

        IMPD(M0)=0 
        IMPD(D )=0

        Do  55 I2=0,27
           IMPP(I2,0)=NLOW
   55   Continue 

* - symbols

        CBLK='/DEFAULT:'
        JB=9

        If(CHMP(I1).NE.CHMD) then
           Write(CPAR,'(''SY_M='''''',A,'''''';'')') CHMP(I1)
           JP=9
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

        If(CHIP(I1).NE.CHID) then
           Write(CPAR,'(''SY_I='''''',A,'''''';'')') CHIP(I1)
           JP=9
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

* - match extension scores

        K1=0
        Do  56 I2=1,NABC
          If(IMPP(I2,I1).NE.IMPD(I2)) K1=1
   56   Continue 
        Do  57 I2=1,NABC
          If(IMPP(I2,I1).NE.IMPP( 1,I1)) K1=2
   57   Continue 

        If(K1.EQ.0) then
           JP=0 
        Else
           If(K1.EQ.1) then
              If(IMPP( 1,I1).EQ.NLOW) then
                 Write(CPAR,'(''M=     *'')')
              Else 
                 Write(CPAR,'(''M='',I6)') IMPP( 1,I1)
              End if 
              JP=9
              CPAR(JP:JP)=';'
           Else
              Write(CPAR,'(''M='',I6,25('','',I6))')
     *          (IMPP(ii1,I1),ii1=1,NABC)
              JP=2+NABC*7
              CPAR(JP:JP)=';'
                 J2=JP-NABC*7+1
              Do  58 I2=1,NABC
                 If(IMPP(I2,I1).EQ.NLOW) CPAR(J2:J2+5)='     *'
                 J2=J2+7
   58         Continue
           End if

           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if

* match extension score for unknown character

        If(IMPP( 0,I1).NE.IMPD( 0)) then
           If(IMPP( 0,I1).EQ.NLOW) then
              Write(CPAR,'(''M0=     *'')') 
           Else
              Write(CPAR,'(''M0='',I6)') IMPP( 0,I1)
           End if
           JP=10
           CPAR(JP:JP)=';'
           Call slpar(CPAR,JP)
           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

* - deletion extension score

        If(IMPP(27,I1).NE.IMPD(27)) then
           If(IMPP(27,I1).EQ.NLOW) then
              Write(CPAR,'(''D=     *'')') 
           Else
              Write(CPAR,'(''D='',I6)') IMPP(27,I1)
           End if
           JP=9
           CPAR(JP:JP)=';'
           Call slpar(CPAR,JP)
           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

* - insert extension scores

        K1=0
        Do  61 I2=1,NABC
          If(IIPP(I2,I1).NE.IIPD(I2)) K1=1
   61   Continue 
        Do  62 I2=1,NABC
          If(IIPP(I2,I1).NE.IIPP( 1,I1)) K1=2
   62   Continue 

        If(K1.EQ.0) then
           JP=0 
        Else
           If(K1.EQ.1) then
              Write(CPAR,'(''I='',I6)') IIPP( 1,I1)
              JP=9
              CPAR(JP:JP)=';'
           Else
              Write(CPAR,'(''I='',I6,25('','',I6))')
     *          (IIPP(ii1,I1),ii1=1,NABC)
              JP=2+NABC*7
              CPAR(JP:JP)=';'
                 J2=JP-NABC*7+1
              Do  65 I2=1,NABC
                 If(IIPP(I2,I1).EQ.NLOW) CPAR(J2:J2+5)='     *'
                 J2=J2+7
   65         Continue
           End if

           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if

        If(IIPP( 0,I1).NE.IIPD( 0)) then
           Write(CPAR,'(''I0='',I6)') IIPP( 0,I1)
           JP=10
           CPAR(JP:JP)=';'
           Call slpar(CPAR,JP)
           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

* - initiation, transition and termination scores

        Do  67 I2=27,46
           If(IIPP(I2,I1).NE.IIPD(I2)) then
              If(IIPP(I2,I1).EQ.NLOW) then
                 Write(CPAR,'(A2,''=     *'')') CPSN(I2)
              Else
                 Write(CPAR,'(A2,''='',I6)') CPSN(I2),IIPP(I2,I1)
              End if 
              JP=10
              CPAR(JP:JP)=';'
              Call slpar(CPAR,JP)
              CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
              JB=JB+JP+1
           End if 
   67   Continue  

        Call wrblk(NOUT,CBLK,JB)

* - restore previous defaults from pos. LPRF+1 

        I1=LPRF+1

        Do  68 I2=0,46
           IIPD(I2)=IIPP(I2,I1)
   68   Continue
           CHID=CHIP(I1)
        Do  69 I2=0,27
           IMPD(I2)=IMPP(I2,I1)
   69   Continue
           CHMD=CHMP(I1)

        Do  90 I1=0,LPRF

* write /M: block 

        If(I1.NE.0) then

* - symbol

        CBLK='/M:'
        JB=3

        If(CHMP(I1).NE.CHMD) then
           Write(CPAR,'(''SY='''''',A,'''''';'')') CHMP(I1)
           JP=7
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

* - match extension scores

        K1=0
        Do  71 I2=1,NABC
          If(IMPP(I2,I1).NE.IMPD(I2)) K1=1
   71   Continue 
        Do  72 I2=1,NABC
          If(IMPP(I2,I1).NE.IMPP( 1,I1)) K1=2
   72   Continue 

        If(K1.EQ.0) then
           JP=0 
        Else
           If(K1.EQ.1) then
              If(IMPP( 1,I1).EQ.NLOW) then
                 Write(CPAR,'(''M=     *'')')
              Else 
                 Write(CPAR,'(''M='',I6)') IMPP( 1,I1)
              End if 
              JP=9
              CPAR(JP:JP)=';'
           Else
              Write(CPAR,'(''M='',I6,25('','',I6))')
     *          (IMPP(ii1,I1),ii1=1,NABC)
              JP=2+NABC*7
              CPAR(JP:JP)=';'
                 J2=JP-NABC*7+1
              Do  75 I2=1,NABC
                 If(IMPP(I2,I1).EQ.NLOW) CPAR(J2:J2+5)='     *'
                 J2=J2+7
   75         Continue
           End if

           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if

* match extension score for unknown character

        If(IMPP( 0,I1).NE.IMPD( 0)) then
           If(IMPP( 0,I1).EQ.NLOW) then
              Write(CPAR,'(''M0=     *'')') 
           Else
              Write(CPAR,'(''M0='',I6)') IMPP( 0,I1)
           End if
           JP=10
           CPAR(JP:JP)=';'
           Call slpar(CPAR,JP)
           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

* - deletion extension score

        If(IMPP(27,I1).NE.IMPD(27)) then
           If(IMPP(27,I1).EQ.NLOW) then
              Write(CPAR,'(''D=     *'')') 
           Else
              Write(CPAR,'(''D='',I6)') IMPP(27,I1)
           End if
           JP=9
           CPAR(JP:JP)=';'
           Call slpar(CPAR,JP)
           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

        Call wrblk(NOUT,CBLK,JB)

        End if

* write /I: block 

        If(I1.NE.0.OR..NOT.LPCI) then 

        CBLK='/I:'
        JB=3

* - symbol

        If(CHIP(I1).NE.CHID) then
           Write(CPAR,'(''SY='''''',A,'''''';'')') CHIP(I1)
           JP=7
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

* - insert extension scores

        K1=0
        Do  81 I2=1,NABC
          If(IIPP(I2,I1).NE.IIPD(I2)) K1=1
   81   Continue 
        Do  82 I2=1,NABC
          If(IIPP(I2,I1).NE.IIPP( 1,I1)) K1=2
   82   Continue 

        If(K1.EQ.0) then
           JP=0 
        Else
           If(K1.EQ.1) then
              Write(CPAR,'(''I='',I6)') IIPP( 1,I1)
              JP=9
              CPAR(JP:JP)=';'
           Else
              Write(CPAR,'(''I='',I6,25('','',I6))')
     *          (IIPP(ii1,I1),ii1=1,NABC)
              JP=2+NABC*7
              CPAR(JP:JP)=';'
                 J2=JP-NABC*7+1
              Do  85 I2=1,NABC
                 If(IIPP(I2,I1).EQ.NLOW) CPAR(J2:J2+5)='     *'
                 J2=J2+7
   85         Continue
           End if

           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if

        If(IIPP( 0,I1).NE.IIPD( 0)) then
           Write(CPAR,'(''I0='',I6)') IIPP( 0,I1)
           JP=10
           CPAR(JP:JP)=';'
           Call slpar(CPAR,JP)
           Call slpar(CPAR,JP)
           CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
           JB=JB+JP+1
        End if 

* - initiation, transition and termination scores

        Do  87 I2=27,46
           If(IIPP(I2,I1).NE.IIPD(I2)) then
              If(IIPP(I2,I1).EQ.NLOW) then
                 Write(CPAR,'(A2,''=     *'')') CPSN(I2)
              Else
                 Write(CPAR,'(A2,''='',I6)') CPSN(I2),IIPP(I2,I1)
              End if 
              JP=10
              CPAR(JP:JP)=';'
              Call slpar(CPAR,JP)
              CBLK(JB+2:JB+JP+1)=CPAR(1:JP)
              JB=JB+JP+1
           End if 
   87   Continue  

        If(CBLK.NE.'/I:') Call wrblk(NOUT,CBLK,JB)

        End if 

   90   Continue

        Return
        End
*----------------------------------------------------------------------*
        Subroutine slpar(CPAR,JP)

        Character*(*)     CPAR

           K1=0
        Do  10 I1=1,JP
           If(CPAR(I1:I1).NE.' '.OR.CPAR(I1-1:I1-1).EQ.';') then 
              K1=K1+1
              CPAR(K1:K1)=CPAR(I1:I1)
           Else if
     *       (CPAR(I1:I1+1).EQ.' ;'.AND.CPAR(K1:K1).EQ.'.') then
              K1=K1+1
              CPAR(K1:K1)='0' 
           Else if
     *       (CPAR(I1:I1+1).EQ.' ,'.AND.CPAR(K1:K1).EQ.'.') then
              K1=K1+1
              CPAR(K1:K1)='0' 
           End if
   10   Continue
           JP=K1
               
        Return
        End
*----------------------------------------------------------------------*
        Subroutine wrblk(NOUT,CBLK,JB)

        Character*(*)     CBLK
        Character*80      RCEX

        RCEX='MA'

        If(JB.LE.73) then
           RCEX(6:)=CBLK(1:JB)
           Write(NOUT,'(78A)')(RCEX(ii1:ii1),ii1=1,JB+5)
        Else

           Do  14 I1=73,1,-1
              If(CBLK(I1:I1).EQ.';'.OR.CBLK(I1:I1).EQ.',') go to 15 
   14      Continue
   15      RCEX(6:)=CBLK(1:I1)
           Write(NOUT,'(78A)')(RCEX(ii1:ii1),ii1=1,I1+5)
           RCEX='MA'
           KB=I1+1

   20      Continue

           Do  22 I1=KB,JB
              If(CBLK(I1:I1).NE.' ') go to 23 
   22      Continue
   23      KB=I1

           Do  24 I1=MIN(KB+69,JB),KB,-1
              If(   CBLK(I1:I1).EQ.';'
     *          .OR.CBLK(I1:I1).EQ.','
     *          .OR.CBLK(I1:I1).EQ.' ') go to  25
   24      Continue
   25      RCEX(9:)=CBLK(KB:I1)
           Write(NOUT,'(78A)')(RCEX(ii1:ii1),ii1=1,Lnblnk(RCEX))
           KB=I1+1
           If(KB.LT.JB) go to  20 

        End if

        Return
        End
