*       Program pfmake
*----------------------------------------------------------------------*     
*       Function: Constructs a profile from a multiple sequence 
*                 alignment 
*       Author:   Philipp Bucher
*       Version:  This file is part of pftools release 2.1 February 1998
*----------------------------------------------------------------------*     
* DATA
*----------------------------------------------------------------------*

* array sizes, I/O units

        Parameter        (IDM1=1048576)
        Parameter        (IDM2=   2048)

        Parameter        (NOUT=      6)
        Parameter        (NMSF=      5)
        Parameter        (NCMP=     12)

* profile and sequence fields :

        Parameter        (IDMP=   9999)

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'pfdat.f'
        Include          'dfdat.f'
        Include          'sterr.f'

        Character*64      FOUT

* weights and distances 

        Real              RWGT(IDM2)

* multiple sequence alignment:

        Character*64      FMSF

        Character*32      SQID(IDM2)
        Character         CSEQ(IDM1)

* frequency profile

        Real              FPRF(26,IDMP)
        Real              FUNK(IDMP)
        Real              FRES(IDMP)
        Real              FDEL(IDMP)

* score profile

        Real              SPRF(26,IDMP)
        Real              SDEL(IDMP)
        Real              SINS(0:IDMP)

* symbol comparison table

        Character*64      FCMP

        Real              RCMP(26,26)

* options

        Logical           OPT0
        Logical           OPT1
        Logical           OPT2
        Logical           OPT3

        Logical           LBLK  
        Logical           LSYM  
        Logical           LWGE
        Real              RE
        Real              RF
        Real              RG
        Real              RH 
        Real              RI 
        Real              RL 
        Real              RM
        Real              RS
        Real              RT
        Real              RX

* character translation 
 
        Character*27      ABCU
        Character*27      ABCL
 
        Data              ABCU/'ABCDEFGHIJKLMNOPQRSTUVWXYZ-'/
        Data              ABCL/'abcdefghijklmnopqrstuvwxyz.'/
    
* initialization of controlled vocabularies
 
        Include          'cvini.f'

*----------------------------------------------------------------------*
* INPUT SECTION
*----------------------------------------------------------------------*

        IRC=0
        IDUM=-2

* read command line 

        Call Repar
     *    (FMSF,FCMP,OPT0,OPT1,OPT2,OPT3,LSYM,LWGE,LBLK,
     *     RE,RF,RG,RH,RI,RL,RM,RS,RT,RX,NLOW,IRC)
        If(IRC.NE.0) then
           Write(NERR,'(
     *      ''Usage: pfmake [ -0123abes] [ msf-file | - ] '',
     *      ''score-matrix [ parameters ]'',//, 
     *      ''   valid parameters are:'',//,
     *      ''                 [E=gap-extension-weight]     '',/
     *      ''                 [F=output-score-multiplier]  '',/
     *      ''                 [G=gap-weigth]               '',/
     *      ''                 [H=high-cost-init-term-score]'',/
     *      ''                 [I=Ginc-multiplier]          '',/
     *      ''                 [L=low-cost-init-term-score] '',/
     *      ''                 [M=Gmax-multiplier]          '',/
     *      ''                 [S=score-matrix-multiplier]  '',/
     *      ''                 [T=gap-region-threshhold]    '',/
     *      ''                 [X=gap-excision-threshold]   '',/
     *        )')
           Stop
        End if

* read msf-file

        Call REMSF
     *    (NERR,NMSF,FMSF,
     *     IDM1,CSEQ,NSEQ,LSEQ,
     *     IDM2,RWGT,SQID,
     *     IRC)
	If(IRC.NE.0) go to 100

* read score-matrix 

        Call RECMP(NERR,NCMP,FCMP,NABC,CABC,RCMP,IRC)
 
*----------------------------------------------------------------------*
* DATA PROCESSING SECTION 
*----------------------------------------------------------------------*

* normalize weights 

           R1=0.0
        Do I1=1,NSEQ
           R1=R1+RWGT(I1)
        End do
        If(R1.EQ.0.0) then 
           R1=1/Real(NSEQ)
           Do I1=1,NSEQ
              RWGT(I1)=R1
           End do
        Else
           Do I1=1,NSEQ
              RWGT(I1)=RWGT(I1)/R1
           End do
        End if 

* make sequences uppercase 

        Do I1=1,LSEQ*NSEQ
           If     (Index(ABCU,CSEQ(I1)).EQ.0) then
              IX=Index(ABCL,CSEQ(I1))
              If(IX.GT.0) then
                 CSEQ(I1)=ABCU(IX:IX)
              Else
                 CSEQ(I1)='-'
              End if
           End if
        End do

* remove endgaps

        If(.NOT.LWGE) then 
                J1=0 
             Do  10 I1=1,NSEQ
                Do   6 I2=J1+1,J1+LSEQ
                   If(CSEQ(I2).NE.'-') go to  7
                   CSEQ(I2)=' '
    6           Continue
    7           Continue

                Do   8 I2=J1+LSEQ,J1+1,-1
                   If(CSEQ(I2).NE.'-') go to  9
                   CSEQ(I2)=' '
    8           Continue
    9           Continue
                J1=J1+LSEQ
   10       Continue
         End if

* make frequency profile
*    
*   for residue i and profile position j: 
*      FPRF(i,j) = residue frequency(residue,position)
*   for profile position j: 
*      FUNK(  j) = fraction of unknown residues
*      FRES(  j) = fraction of residues residues and gaps 
*      RDEL(  l) = fraction of gaps 

* - initialize 

        Do  12  I1=1,LSEQ
           Do  11 I2=1,NABC
              FPRF(I2,I1)=0.0
   11      Continue
           FUNK(I1)=0.0
           FRES(I1)=0.0
           FDEL(I1)=0.0
   12   Continue

           J1=0
           K1=1
        Do  20 I1=1,NSEQ
              K2=1
           Do  19 I2=J1+1,J1+LSEQ
                 IX=0
              Do  15 I3=1,NABC
                 If(CSEQ(I2).EQ.CABC(I3)) then
                    IX=I3
                    Go to  16
                 End if
   15         Continue
   16         Continue

                 FRES(   K2)=FRES(   K2)+RWGT(K1)
              If     (IX.NE.0) then 
                 FPRF(IX,K2)=FPRF(IX,K2)+RWGT(K1)
              else if(CSEQ(I2).EQ.'-') then
                 FDEL(   K2)=FDEL(   K2)+RWGT(K1)
              else if(CSEQ(I2).EQ.' ') then
                 FRES(   K2)=FRES(   K2)-RWGT(K1)
              else 
                 FUNK(   K2)=FUNK(   K2)+RWGT(K1)
              end if  
              K2=K2+1
   19      Continue
           J1=J1+LSEQ
           K1=K1+1
   20   Continue

* normalize frequencies: 

        Do  25 I1=1,LSEQ
              FDEL(I1)=FDEL(I1)/FRES(I1)
              R1=FRES(I1)-FUNK(I1)
              If(R1.EQ.0) go to  25
           Do  23 I2=1,NABC
              FPRF(I2,I1)=FPRF(I2,I1)/R1
   23      Continue
   25   Continue

* construct score profile:
*    
*   for known residue i and profile match position j: 
*      SPRF(i,j) = match extension score
*   for profile position j: 
*      SDEL(  j) = gap penalty multipliers for deletion gaps  
*      SINS(  j) = gap penalty multipliers for insertion gaps 

* - gap penalty multipliers:

           K1=0
           J1=0
           SINS(0)=1.0
        Do   30 I1=1,LSEQ
           If(FDEL(I1).LE.RT) then
              If(K1.GT.0) then 
                 XDEL=RM/(1.0+K1*RI)
                 SINS(J1-1)=XDEL
                 Do  27 I2=J1,I1-1
                    SINS(I2)=XDEL
                    SDEL(I2)=XDEL
   27            Continue
                 K1=0 
              End if  
              SINS(I1)=1.0
              SDEL(I1)=1.0
           else 
              If(K1.EQ.0) J1=I1
              K1=K1+1
           end if
   30   Continue

        K1=0
        J1=0
        Do  50 I1=1,LSEQ  
           If(FDEL(I1).GT.1-RX) then 

* - gap excision

              J1=J1+1 
           else 
              K1=K1+1

* - score matrix application 

              Do  33 I2=1,NABC  
                 SPRF(I2,K1)=0.0
              Do  32 I3=1,NABC 
                 SPRF(I2,K1)=SPRF(I2,K1)+RCMP(I3,I2)*FPRF(I3,I1)
   32         Continue
                 SPRF(I2,K1)=RS*SPRF(I2,K1)
C                Write(6,'(I4,1x,A,F10.4)') K1,CABC(I2),SPRF(I2,K1)
   33         Continue
              If(J1.GT.0) then 
                 SINS(K1-1)=-SINS(I1-1)
                 J1=0
              End if 
              SDEL(K1)=SDEL(I1)
              SINS(K1)=SINS(I1)
              FDEL(K1)=FDEL(I1)
           end if 
   50   Continue
        If(J1.GT.0) SINS(K1)=-SINS(K1)  
        LPRF=K1

* initialize generalized profile

* - header
 
        CPID='SEQUENCE_RPOFILE'
        CPAC='ZZ99999'
        If(FMSF.EQ.'-') FMSF='stdin'
        CPDE='Generated from MSF file: '''
     *    // FMSF(1:Lblnk(FMSF))
     *    // '''.'
 
* - accessories
 
        LPCI=.FALSE.

        MDIS=2
        N1=MIN(5,LPRF/10) 
        NDIP(1)=1   +N1
        NDIP(2)=LPRF-N1
 
        JNOR=1
        MNOR(1)=1
        NNOR(1)=1
        NNPR(1)=1
        CNTX(1)='OrigScore'
        RNOP(1,1)=0.0
        RNOP(2,1)=1/RF
 
        JCUT=1
        MCLE(1)=0
        CCUT(1)=' '
        ICUT(1)=0
        JCNM(1)=1
        RCUT(1,1)=0.0
        MCUT(1,1)=1
 
* - defaults for match and insert position
 
        CHID='-'
        Do  65 I1=1,26
           IIPD(I1)=0
   65   Continue

        If(LSYM) then
           N1=-NINT(RF*RG/2) 
           N2=-NINT(RF*RG/2) 
        Else
           N1=-NINT(RF*RG) 
           N2=0
        End if
           N3=-NINT(RF*RE)
 
        IIPD(B0)=0
        IIPD(B1)=NLOW
        IIPD(E0)=0
        IIPD(E1)=NLOW
 
        IIPD(BM)=0
        IIPD(BI)=NLOW
        IIPD(BD)=NLOW
        IIPD(BE)=NLOW
        IIPD(MM)=0
        IIPD(MI)=N1
        IIPD(MD)=N1
        IIPD(ME)=0
        IIPD(IM)=N2
        IIPD(II)=0
        IIPD(ID)=NLOW
        IIPD(IE)=NLOW
        IIPD(DM)=N2
        IIPD(DI)=NLOW
        IIPD(DD)=0
        IIPD(DE)=NLOW
 
        IIPD(I0)=0

        CHMD='X'
        CHID='-'
        Do  66 I1=1,26
           IIPD(I1)=N3
           IMPD(I1)=0
   66   Continue
 
        IIPD(M0)=0

        IMPD(D )=N3

        Do  68 I1=0,27
           IMPP(I1,0)=NLOW
   68   Continue

* build insert positions

        Do  75 I1=0,LPRF

              CHIP(I1)=CHID

           Do I2=0,46
              IIPP(I2,I1)=IIPD(I2)
           End do  
              
           If(SINS(I1).LT.0) then 
              XINS=0.0
              SINS(I1)=-SINS(I1)
           Else
              XINS=SINS(I1)
           End if
            
              NINS=-NINT(RF*RE*SINS(I1))
           Do  I2=1,NABC
              IIPP(I2,I1)=NINS
           End do

           If     (LSYM) then
                                IIPP(MI,I1)=-NINT(RF*RG*XINS      /2)
                                IIPP(IM,I1)=-NINT(RF*RG*XINS      /2)
              If(XINS.EQ.0) then
                 If(I1.NE.LPRF) IIPP(MD,I1)=-NINT(RF*RG*SINS(I1)  /2)
                 If(I1.NE.   0) IIPP(DM,I1)=-NINT(RF*RG*SINS(I1)  /2)
              Else 
                 If(I1.NE.LPRF) IIPP(MD,I1)=-NINT(RF*RG*SDEL(I1+1)/2)
                 If(I1.NE.   0) IIPP(DM,I1)=-NINT(RF*RG*SDEL(I1  )/2)
              End if
           Else if(I1.NE.LPRF) then 
              IIPP(MI,I1)=-NINT(RF*RG*SDEL(I1+1))
              IIPP(MD,I1)=-NINT(RF*RG*SDEL(I1+1))
           End if

   75     Continue
 
* - build match position

        Do  80 I1=1,LPRF
           Do  I2=0,27
              IMPP(I2,I1)=IMPD(I2)
           End do
           IMPP( D,I1)=-NINT(RF*RE*SDEL(I1)) 
           Do  I2=1,NABC
              IMPP(I2,I1)=NINT(RF*SPRF(I2,I1))
           End do  

              J2=0 
              K2=0 
           Do I2=1,NABC
              If(IMPP(I2,I1).GT.K2) then 
                 J2=I2 
                 K2=IMPP(I2,I1)
              End if 
           End do
           If(J2.NE.0) then 
              CHMP(I1)=CABC(J2)
           Else
              CHMP(I1)=CHMD  
           End if 
   80   Continue

* block profile

        If(LBLK) then 
        K1=0
        J1=0
   85   J1=J1+1 
        If((FDEL(J1).LE.RT.AND.K1+1.NE.J1).OR.
     *     (FDEL(J1).GT.RT.AND.J1.EQ.LPRF)) then 
        
* - gap region: define center position 

          N1=K1
          N3=J1-1
          If(FDEL(J1).GT.RT) N3=LPRF

* - - beginning, end

          If(N1.EQ.0) then
             N2=N3
             GO to 90
          End if

          If(N3.EQ.LPRF) then
             N2=N1
             Go to 90 
          End if 

* - - gap excision position ?

             N2=N1
          Do I1=N1,N3
             If(IIPP(MI,I1).EQ.0) then 
                N2=I1
                Go to  90
             End if
          End do

* - - find center position

             R1=0
             R2=0
          Do I1=N1+1,N3
             R1=R1+FDEL(I1)-FDEL(I1-1)
             If(R1.GT.R2) then 
                N2=I1
                R2=R1
             End if 
          End do 

* - edit gap region 

   90     Continue 
          Do I1=N1,N3
             If(I1.LT.N2) then
                IIPP(DM,I1)=IIPD(DM)
                IIPP(IM,I1)=IIPD(IM) 
                IIPP(MI,I1)=IIPD(MI) 
             End if
             If(I1.GT.N2) then
                IIPP(MD,I1)=IIPD(MD)
                IIPP(IM,I1)=IIPD(IM) 
                IIPP(MI,I1)=IIPD(MI) 
             End if
          End do 

* - gap region done

          K1=J1

* - no gap region

        Else
          If(FDEL(J1).LE.RT) K1=J1
        End if
          If(J1.LT.LPRF) go to 85

        End if

* begin-to, to-end scores 

        IIPP(BM,   0)=IIPP(MM,   0)
        IIPP(BI,   0)=IIPP(MI,   0)
        IIPP(BD,   0)=IIPP(MD,   0)
        IIPP(ME,LPRF)=IIPP(MM,LPRF)
        IIPP(IE,LPRF)=IIPP(IM,LPRF)
        IIPP(DE,LPRF)=IIPP(DM,LPRF)
      
* alignment mode

* - define high and low init/term scores in integer units 

           NH=NLOW 
        If(RH.GT.Real(NLOW)) then
           RH=-RH*RF
           If(RH.GT.Real(NLOW)) NH=NINT(RH) 
        End if      
           NL=NLOW
           If(-RF*RL.GT.Real(NLOW)) NL=NINT(-RL*RF) 

* - global alignment mode (also initialization for other modes)

        Do I1=0,LPRF
           IIPP(B0,  I1)=NH
           IIPP(B1,  I1)=NH
           IIPP(E0,  I1)=NH
           IIPP(E1,  I1)=NH
           IIPP(BE,  I1)=IIPD(BE)
        End do
           IIPP(B0,   0)=NL
           IIPP(E0,LPRF)=NL

           IIPD(B0)=NH
           IIPD(B1)=NH
           IIPD(E0)=NH
           IIPD(E1)=NH


* - domain-global alignment mode

        If(OPT1.OR.OPT2.OR.OPT3) then
           IIPP(B1,   0)=NL
           IIPP(E1,LPRF)=NL
        End if
           
* - semiglobal alignment mode        

        If(OPT2.OR.OPT3) then
           Do I1=0,LPRF
             IIPP(B0,I1)=NL
             IIPP(E0,I1)=NL
           End do
             IIPD(B0)=NL
             IIPD(E0)=NL
        End if

* - local alignment mode
        
        If(OPT3) then
           Do I1=1,LPRF-1
             IIPP(B1,I1)=NL
             IIPP(E1,I1)=NL
           End do 
             IIPD(B1)=NL
             IIPD(E1)=NL
        End if

*----------------------------------------------------------------------*
* OUTPUT SECTION 
*----------------------------------------------------------------------*

        FOUT='stdout'

        Call WRPRF
     *    (NOUT,
     *     CPID,CPAC,CPDE,NABC,CABC,LPRF,LPCI,
     *     CDIS,JDIP,MDIS,NDIP,
     *     CNOR,JNOP,JNOR,MNOR,NNOR,NNPR,CNTX,RNOP,
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT,
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     BLOG,FABC,P0,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

  100   Stop
  900   Go to 100 
        End
*----------------------------------------------------------------------*
        Subroutine Repar
     *    (FMSF,FCMP,OPT0,OPT1,OPT2,OPT3,LSYM,LWGE,LBLK,
     *     RE,RF,RG,RH,RI,RL,RM,RS,RT,RX,NLOW,IRC)

        Character*64      CPAR
        Character*64      FMSF 
        Character*64      FCMP

        Logical           OPT0
        Logical           OPT1
        Logical           OPT2
        Logical           OPT3

        Logical           LBLK 
        Logical           LSYM
        Logical           LWGE

        OPT0=.FALSE.
        OPT1=.FALSE.
        OPT2=.TRUE.
        OPT3=.FALSE.

        LBLK=.FALSE.
        LSYM=.TRUE.
        LWGE=.FALSE.
       
        RE=0.2
        RF=100
        RG=2.1
        RH=Real(NLOW)
        RI=0.1
        RL=0.0
        RM=0.333
        RS=0.1
        RT=0.01
        RX=0.5

        FMSF=' '
        FCMP=' '

        N1=Iargc()

        Do  50 I1=1,N1 

           Call GetArg(I1,CPAR)

           If     (CPAR(1:1).EQ.'-'.AND.CPAR(2:2).NE.' ') then
              Do  I2=2,Lblnk(CPAR)
                 If     (CPAR(I2:I2).EQ.'0') then
                    OPT0=.TRUE. 
                    OPT2=.FALSE.
                 else if(CPAR(I2:I2).EQ.'1') then
                    OPT1=.TRUE. 
                    OPT2=.FALSE.
                 else if(CPAR(I2:I2).EQ.'2') then
                    OPT2=.TRUE. 
                 else if(CPAR(I2:I2).EQ.'3') then
                    OPT3=.TRUE. 
                    OPT2=.FALSE.
                 else if(CPAR(I2:I2).EQ.'a') then
                    LSYM=.FALSE.
                 else if(CPAR(I2:I2).EQ.'b') then
                    LBLK=.TRUE.
                 else if(CPAR(I2:I2).EQ.'e') then
                    LWGE=.TRUE. 
                 else if(CPAR(I2:I2).EQ.'s') then 
                    LSYM=.TRUE. 
                 End if
              End do 
           Else if(CPAR(1:2).EQ.'E=') then
              Read(CPAR(3:64),*,Err=900) RE
           Else if(CPAR(1:2).EQ.'F=') then
              Read(CPAR(3:64),*,Err=900) RF
           Else if(CPAR(1:2).EQ.'G=') then
              Read(CPAR(3:64),*,Err=900) RG
           Else if(CPAR(1:2).EQ.'H=') then
              Read(CPAR(3:64),*,Err=900) RH
           Else if(CPAR(1:2).EQ.'I=') then
              Read(CPAR(3:64),*,Err=900) RI
           Else if(CPAR(1:2).EQ.'L=') then
              Read(CPAR(3:64),*,Err=900) RL
           Else if(CPAR(1:2).EQ.'M=') then
              Read(CPAR(3:64),*,Err=900) RM
           Else if(CPAR(1:2).EQ.'S=') then
              Read(CPAR(3:64),*,Err=900) RS
           Else if(CPAR(1:2).EQ.'T=') then
              Read(CPAR(3:64),*,Err=900) RT
           Else if(CPAR(1:2).EQ.'X=') then
              Read(CPAR(3:64),*,Err=900) RX
           Else if(FMSF     .EQ.' ' ) then
              FMSF=CPAR
           Else if(FCMP     .EQ.' ' ) then
              FCMP=CPAR
           End if

   50   Continue

        If(FMSF.EQ.' '.OR.FCMP.EQ.' ') go to 900
       
        If(LBLK) then
           LSYM=.TRUE.
           LWGE=.FALSE.
        End if

  100   Return 
  900   IRC=1
        Go to 100
        End
*----------------------------------------------------------------------*
        Subroutine RECMP(NERR,NCMP,FCMP,NABC,CABC,RCMP,IRC)

        Character*64      FCMP

        Integer           NABC 
        Character         CABC(0:26) 

        Real              RCMP(26,26)

        Character*256     RCIN 

        Open(NCMP,File=FCMP,Status='OLD',Err=901)

    1   Read(NCMP,'(A)',Err=902,End=902) RCIN        
        IX=Index(RCIN,'..')
        If(IX.EQ.0) go to   1

* read alphabet  
 
              NABC=0
        Do  10 I1=1,IX-1
           If(RCIN(I1:I1).NE.' ') then
              NABC=NABC+1
              CABC(NABC)=RCIN(I1:I1)
           End if
   10   Continue

        Do  20  I1=1,NABC
   11      Read(NCMP,'(A)',End=903) RCIN        
           If(RCIN.EQ.' ') go to  11 
           Read(RCIN,*,Err=904)(RCMP(I1,ii1),ii1=I1,NABC)
           Read(RCIN,*,Err=904)(RCMP(ii1,I1),ii1=I1,NABC)
   20   Continue
     
  100   Return

  904   IRC=IRC+1
  903   IRC=IRC+1
  902   IRC=IRC+1
  901   IRC=IRC+1
        Write(NERR,'(''RECMP: Error termination.'',I4)') IRC
        Go to 100

        End
*----------------------------------------------------------------------*
        Include          'remsf.f'
        Include          'wrprf.f'
        Include          'lblnk.f'
