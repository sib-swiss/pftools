        Subroutine RGPRF
     *    (NGPR,FPRF,
     *     CPID,CPAC,CPDE,NABC,CABC,LPRF,LPCI,
     *     MDIS,NDIP,
     *     JNOR,MNOR,NNOR,NNPR,CNTX,RNOP, 
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT, 
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

        Character*64      FGPR
        Character*256     RCIN  

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'pfdat.f'
        Include          'dfdat.f'
 
        Integer           IPRF(32)
        Character         CPRF

        Logical           LSYM

        IRC=0

    1   Write(0,'(''Input file ? '',$)') 
        Read(5,'(A)',Err=  1) FGPR 
        Open(NGPR,File=FGPR(1:64),Status='OLD',Err=  1)

    2   GO=1
        Write(0,'(''Gap weight (default=1.0) ? '',$)')  
        Read(5,'(A)',Err=  2) RCIN
        If(RCIN.EQ.' ') go to   3
        Read(RCIN,*,Err=2) GO 

    3   GE=1
        Write(0,'(''Gap length weight (default=1.0) ? '',$)')  
        Read(5,'(A)',Err=  3) RCIN
        If(RCIN.EQ.' ') go to   4
        Read(RCIN,*,Err=3) GE 

    4   XFAC=100
        Write(0,'(''Rescaling factor (default=100) ? '',$)')  
        Read(5,'(A)',Err=  4) RCIN
        If(RCIN.EQ.' ') go to   5
        Read(RCIN,*,Err=4) XFAC 

    5   LSYM=.TRUE. 
        Write(0,
     *    '(''Symmetric gap weighting (Yes/No, default=Y) ? '',$)')
        Read(5,'(A)',Err=  5) RCIN
        If(RCIN.EQ.' ') go to   6
        If     (RCIN(1:1).EQ.'Y'.OR.RCIN(1:1).EQ.'y') then 
           LSYM=.TRUE.         
        Else if(RCIN(1:1).EQ.'N'.OR.RCIN(1:1).EQ.'n') then
           LSYM=.FALSE.
        Else
           Go to   5
        End if 

    6   Continue

* initialization

* - header

        CPID='GCG_PROFILE'
        CPAC='ZZ99999'
        CPDE='Automatically reformatted from file ''' 
     *    // FGPR(1:Lnblnk(FGPR))
     *    // '''.' 

* - accessories

        LPCI=.FALSE.

        MDIS=1

        JNOR=1
        MNOR(1)=1
        NNOR(1)=1
        NNPR(1)=1   
        CNTX(1)='OrigScore'
        RNOP(1,1)=0.0
        RNOP(2,1)=1/XFAC

        JCUT=1
        MCLE(1)=0
        CCUT(1)=' '
        ICUT(1)=0
        JCNM(1)=1
        RCUT(1,1)=0.0
        MCUT(1,1)=1 

* - defaults for match and insert position  

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
        IIPD(IM)=0
        IIPD(II)=0
        IIPD(ID)=NLOW
        IIPD(IE)=NLOW
        IIPD(DM)=0
        IIPD(DI)=NLOW
        IIPD(DD)=0
        IIPD(DE)=NLOW

        IIPD(I0)=0

        CHMD='X'
        Do  16 I1=1,26 
           IMPD(I1)=0
   16   Continue

        IIPD(M0)=0 
        IMPD(D )=0

        Do  18 I1=0,27
           IMPP(I1,0)=NLOW
   18   Continue 

* read alphabet

   25   Read(NGPR,'(A)',End=999) RCIN
        If(RCIN( 1: 4).NE.'Cons') go to  25
    
        IC1=Index(RCIN,'Gap')
           K1=0
        Do  29 I1=5,IC1-1
           If(RCIN(I1:I1).NE.' ') then
              K1=K1+1
              CABC(K1)=RCIN(I1:I1)
           End if
   29   Continue
        NABC=K1

* read numbers

           K1=0
   30   Read(NGPR,'(A)',End= 50) RCIN
           If(RCIN( 1: 1).EQ.'!') go to  30

* - input line

           RCIN(256:256)='@'
           CPRF=RCIN(2:2)
           Read(RCIN(3:256),*,Err=50,End= 50)
     *        (IPRF(ii1),ii1=1,NABC+2)
           Do  34 I2=1,NABC
              IPRF(I2)=NINT(Real(IPRF(I2))/100*XFAC)
   34      Continue
           If(LSYM) then 
              NGO=-NINT(Real(IPRF(NABC+1))/200*XFAC*GO)
           Else
              NGO=-NINT(Real(IPRF(NABC+1))/100*XFAC*GO)
           End if 
              NGE=-NINT(Real(IPRF(NABC+2))/100*XFAC*GE)

* - build insert position 

              CHIP(K1)=CHID 
           Do  36 I1=0,46
              IIPP(I1,K1)=IIPD(I1)
   36      Continue
           Do  37 I1=1,NABC
              IIPP(I1,K1)=NGE
   37      Continue
              IIPP(MI,K1)=NGO
              IIPP(MD,K1)=NGO
           If(LSYM) then
              IIPP(IM,K1)=NGO
              IIPP(DM,K1)=NGO
           End if 

* - build match position

           K1=K1+1
              CHMP(K1)=CPRF
           Do  43 I1=1,NABC
              IMPP(I1,K1)=IPRF(I1)
   43      Continue 
              IMPP( 0,K1)=0
              IMPP(D ,K1)=NGE

        Go to  30

   50   LPRF=K1
        If(LPRF.LE.0) go to 999

* - defaults for gap weights

           NGO=IIPP(MI,0) 
           NGE=IMPP( D,1) 
        Do  53 I1=1,LPRF-1
           NGO=MIN(NGO,IIPP(MI,I1))
           NGE=MIN(NGE,IMPP( D,I1+1))
   53   Continue

           IIPD(MI)=NGO
           IIPD(MD)=NGO
        If(LSYM) then 
           IIPD(DM)=NGO
           IIPD(IM)=NGO
        End if
           
        Do  54 I1=1,NABC
           IIPD(I1)=NGE
   54   Continue 
           IMPD( D)=NGE

* - last insert position 

           CHIP(K1)=CHID 
        Do  60 I1=0,46
           IIPP(I1,K1)=IIPD(I1)
   60   Continue

  100   Return 
  999   Write(0,*)  'Profile format error, last record read: ',
     *     RCIN(1:Lnblnk(RCIN))
        IRC=1
        Go to 100
        End
