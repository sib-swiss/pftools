*       Program HtoP 
*----------------------------------------------------------------------*     
*       Function: Reformats profiles: in-fmt=HMMER / out-fmt=PROSITE    
*       Author:   Philipp Bucher
*       Version:  This file is part of pftools release 2.0 June 1997
*----------------------------------------------------------------------*     
*
* DATA
*----------------------------------------------------------------------*     

* array dimensions and I/O units

        Parameter        (IDMP=9999)

        Parameter        (NPRF=   5)
        Parameter        (NOUT=   6)
        Parameter        (NNUL=  12)

* profile and sequence fields :

        Character*64      FPRF

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'pfdat.f'
        Include          'dfdat.f'

        Include          'sterr.f'

* HMM 
        Include          'hmdat.f'

        Character*64      FNUL

* command line options and parameters

        Real*8            DB
        Real*8            DL

        Logical           OPTF
        Logical           OPTI    
        Logical           OPTS    

* initialization of controlled vocabularies

        Include          'cvini.f' 

*----------------------------------------------------------------------*     
* INPUT SECTION 
*----------------------------------------------------------------------*     

        IRC=0

* read command line arguments

        Call Repar
     *    (OPTF,OPTI,OPTS,FPRF,FNUL,DB,RC,DL,NM,RP,RQ,IRC)
        If(IRC.NE.0) then
           Write(NERR,'(
     *      ''Usage: gtop [ -fsi ] [ hmm-file | - ] ''
     *      ''[ random-model-file ] [ parameters ]'',//,
     *      ''   valid parameters are:'',//,
     *      ''                 [B=norm-score-logbase]'',/,
     *      ''                 [C=cut-off-value]'',/,
     *      ''                 [L=profile-logbase]'',/,
     *      ''                 [M=length-unprotected-ends'',/,
     *      ''                 [P=percent-unprotected-ends'',/
     *      ''                 [Q=prob-of-unknown-residue'',/
     *        )')
           Stop
        End if

* read profile

    1   Continue           
  
        Call RHMMER(NPRF,FPRF,LPRF,RIHM,RMHM,IDMP,NABC,CABC,IRC)
        If(IRC.NE.0) go to 100

* read null-model

        If(FNUL.NE.' ') then 
           Call RHNUL(NNUL,FNUL,FABC,NABC,IRC)      
           If(IRC.NE.0) go to 100 
        Else
              Do I2=1,NABC
                 FABC(I2)=0.0
              End do
           Do I1=1,LPRF
              Do I2=1,NABC
                 FABC(I2)=FABC(I2)+RIHM(I2,I1)
              End do
           End do 
              Do I2=1,NABC
                 FABC(I2)=FABC(I2)/LPRF
              End do
        End if

* header line definitions 

        If(FPRF.EQ.'-') FPRF='stdin'
        CPID='HMMER-HMM'
        CPAC='HH99999'
        CPDE='Automatically reformatted from file '''
     *    // FPRF(1:Lblnk(FPRF))
     *    // '''.'

* accessories 

        LPCI=.FALSE.

        BLOG=DL
        DL=1/LOG(DL)
        P0=1.0

        MDIS=2
        If(NM.EQ.0) NM=LPRF/2
        N1=MIN(NM,NINT(LPRF*RP/100))
        NDIP(1)=1   +N1
        NDIP(2)=LPRF-N1

        JNOR=1
        MNOR(1)=1
        NNOR(1)=1
        NNPR(1)=1
        CNTX(1)='nscore'
        RNOP(2,1)=1/LOG(DB)/DL
        If(OPTF) then
           RNOP(1,1)=DL*(LOG(Real(1-1000.0/1001))-LOG(4.0))*RNOP(2,1)
        Else
           RNOP(1,1)=0.0
        End if
 
        JCUT=1
        MCLE(1)=0
        CCUT(1)=' '
        JCNM(1)=1
        RCUT(1,1)=RC
        MCUT(1,1)=1
        ICUT(1)=NINT((RCUT(1,1)-RNOP(1,1))/RNOP(2,1))+1

* defaults for match and insert positions

        If(OPTF) then
           IIPD(B1)=NINT(-DL*(LOG(Real(LPRF-1))))
           IIPD(E1)=IIPD(B1)
           IIPD(BM)=0
           IIPD(BI)=NLOW
           IIPD(BD)=NLOW
           IIPD(ME)=0
           IIPD(IE)=NLOW
           IIPD(DE)=NLOW
        Else 
           IIPD(B1)=NLOW
           IIPD(E1)=NLOW
        End if 

        If(OPTS) then 
           IIPD(B0)=0
           IIPD(E0)=0
        Else 
           IIPD(B0)=IIPD(B1)
           IIPD(E0)=IIPD(E1)
        End if

        IMPD( D)=0

        IIPD(BM)=0
        IIPD(BI)=NLOW
        IIPD(BD)=NLOW

        IIPD(ME)=0
        IIPD(IE)=NLOW
        IIPD(DE)=NLOW

        IIPD(BE)=NLOW
        IIPD(MM)=0
        IIPD(MI)=0
        IIPD(MD)=0
        IIPD(IM)=0
        IIPD(II)=0
        IIPD(ID)=0
        IIPD(DM)=0
        IIPD(DI)=0
        IIPD(DD)=0
 
        IIPD(I0)=NINT(DL*LOG(RQ))
        If(OPTI) IIPD(I0)=0
 
        CHID='-'
        Do I1=1,26
           IIPD(I1)=0
        End do 
    
        IMPD(M0)=NINT(DL*LOG(RQ))
        IMPD(D )=0
        CHMD='X'
        Do I1=1,26
           IMPD(I1)=0
        End do 

        Do I1=0,LPRF
           Do I2=0,27
              IMPP(I2,I1)=IMPD(I2)
           End do
           Do I2=0,46
              IIPP(I2,I1)=IIPD(I2)
           End do
        End do

* convert probabilities into profile-scores

        Do I1=0,LPRF
           IIPP(MM,I1)=NLOW
           IIPP(MD,I1)=NLOW
           IIPP(MI,I1)=NLOW
           IIPP(DM,I1)=NLOW
           IIPP(DD,I1)=NLOW
           IIPP(DI,I1)=NLOW
           IIPP(IM,I1)=NLOW
           IIPP(ID,I1)=NLOW
           IIPP(II,I1)=NLOW
           If(RIHM(MM,I1).GT.0) IIPP(MM,I1)=NINT(DL*LOG(RIHM(MM,I1)))
           If(RIHM(MD,I1).GT.0) IIPP(MD,I1)=NINT(DL*LOG(RIHM(MD,I1)))
           If(RIHM(MI,I1).GT.0) IIPP(MI,I1)=NINT(DL*LOG(RIHM(MI,I1)))
           If(RIHM(DM,I1).GT.0) IIPP(DM,I1)=NINT(DL*LOG(RIHM(DM,I1)))
           If(RIHM(DD,I1).GT.0) IIPP(DD,I1)=NINT(DL*LOG(RIHM(DD,I1)))
           If(RIHM(DI,I1).GT.0) IIPP(DI,I1)=NINT(DL*LOG(RIHM(DI,I1)))
           If(RIHM(IM,I1).GT.0) IIPP(IM,I1)=NINT(DL*LOG(RIHM(IM,I1)))
           If(RIHM(ID,I1).GT.0) IIPP(ID,I1)=NINT(DL*LOG(RIHM(ID,I1)))
           If(RIHM(II,I1).GT.0) IIPP(II,I1)=NINT(DL*LOG(RIHM(II,I1)))

           Do I2=1,NABC
           If(RMHM(I2,I1).GT.0) then
              IMPP(I2,I1)=NINT(DL*LOG(RMHM(I2,I1)/FABC(I2)))
           Else
              IMPP(I2,I1)=NLOW
           End if
           End do 

           If(OPTI) then
              IIPP(I2,I1)=0
           Else 
              Do I2=1,NABC
              If(RIHM(I2,I1).GT.0) then
                 IIPP(I2,I1)=NINT(DL*LOG(RIHM(I2,I1)/FABC(I2)))
              Else
                 IIPP(I2,I1)=NLOW
              End if
              End do 
           End if 
        End do

* beginning and end

        If(.NOT.OPTF) then
           IIPP(BM,   0)=IIPP(MM,   0)
           IIPP(BD,   0)=IIPP(MD,   0)
           IIPP(BI,   0)=IIPP(MI,   0)
           IIPP(ME,LPRF)=IIPP(MM,LPRF)
           IIPP(DE,LPRF)=IIPP(DM,LPRF)
           IIPP(IE,LPRF)=IIPP(IM,LPRF)
        End if

        IIPP(B0,   0)=0
        IIPP(B1,   0)=0
        IIPP(E0,LPRF)=0
        IIPP(E1,LPRF)=0

* Generate consensus sequence 
 
           CHIP( 0)='-'
        Do I1=1,LPRF 
           CHMP(I1)=CABC( 1)
           CHIP(I1)=CHID
           K1=IMPP( 1,I1)
           Do I2=1,NABC
              If(IMPP(I2,I1).GT.K1) then 
                 K1=IMPP(I2,I1)
                 CHMP(I1)=CABC(I2)
              End if
           End do 
        End do

* write profile

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

        Go to   1

  100   Stop
        End
*----------------------------------------------------------------------*     
        Subroutine Repar
     *    (OPTF,OPTI,OPTS,FPRF,FNUL,DB,RC,DL,NM,RP,RQ,IRC)

        Character*64      CARG
        Character*(*)     FPRF
        Character*(*)     FNUL

        Real*8            DB
        Real*8            DL

        Logical           OPTF
        Logical           OPTI
        Logical           OPTS

* initializations

        OPTF=.FALSE.
        OPTI=.FALSE.
        OPTS=.FALSE.
        DB=2.0
        RC=0.0
        DL=1.0233739
        NM=5
        RP=0.0
        RQ=0.8

        FPRF=' '
        FNUL=' '

        IRC=0

        N1=Iargc()

           K1=0
       Do I1=1,N1
           Call GetArg(I1,CARG)
           If     (CARG(1:1).EQ.'-'.AND.CARG(2:2).NE.' ') then
              If(Index(CARG,'f').NE.0) OPTF=.TRUE.
              If(Index(CARG,'i').NE.0) OPTI=.TRUE.
              If(Index(CARG,'s').NE.0) OPTS=.TRUE.
           Else if(CARG(1:2).EQ.'B=') then
              Read(CARG(3:64),*,Err=900) DB
           Else if(CARG(1:2).EQ.'C=') then
              Read(CARG(3:64),*,Err=900) RC
           Else if(CARG(1:2).EQ.'L=') then
              Read(CARG(3:64),*,Err=900) DL
           Else if(CARG(1:2).EQ.'M=') then
              Read(CARG(3:64),*,Err=900) NM 
           Else if(CARG(1:2).EQ.'P=') then
              Read(CARG(3:64),*,Err=900) RP
           Else if(CARG(1:2).EQ.'Q=') then
              Read(CARG(3:64),*,Err=900) RQ
           Else if(K1.LE.1) then
              K1=K1+1
              If     (K1.EQ.1) then
                 FPRF=CARG
              Else if(K1.EQ.2) then
                 FNUL=CARG
              End if
           Else 
              Go to 900
           End if
        End do 

        If(FPRF.EQ.' ') Go to 900

  100   Return
  900   IRC=-1
        Go to 100
        End
*----------------------------------------------------------------------*     
        Include          'rhmmer.f'
        Include          'rhnul.f'
        Include          'wrprf.f'
        Include          'lblnk.f'
