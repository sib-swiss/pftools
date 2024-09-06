*       Program ptoh 
*----------------------------------------------------------------------*     
*       Function: Reformats profile -> hmm: in-fmt=PROSITE / out-fmt=SAM    
*       Author:   Philipp Bucher
*       Version:  This file is part of pftools release 2.2 June 1999
*----------------------------------------------------------------------*     
* DATA
*----------------------------------------------------------------------*     

* array dimensions and I/O units

        Parameter        (IDMP=9999)

        Parameter        (NOUT=   6)    

        Parameter        (NPRF=  11)
        Parameter        (NNUL=  12)

* profile 

        Character*64      FPRF

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'pfdat.f'
        Include          'dfdat.f'

        Include          'sterr.f'

* null model 

        Character*64      FNUL

        Integer           IABC(26)
        Integer           JABC(26)
        Character*20      DABC

* HMM 

        Include          'hmdat.f' 

* parameters and options

        Logical           OPTF
        Logical           OPFF
        Logical           OPTH
        Logical           OPTS

        Real              RD 
        Real              RI
        Real*8            DL
 
* initialization of controlled vocabularies

        Include          'cvini.f' 

*----------------------------------------------------------------------*     
* INPUT SECTION 
*----------------------------------------------------------------------*     

        IRC=0
        FPRF='stdout'

* command line arguments
 
        Call Repar(FPRF,FNUL,OPTF,OPFF,OPTH,OPTS,RD,RI,DL,IRC)
        If(IRC.NE.0) then
           Write(NERR,'(
     *      ''Usage: PtoH profile null-model log-factor''
     *        )')
           Stop
        End if

* read profile
 
        If(FPRF.EQ.'-') then
           MPRF=5
        Else
           MPRF=NPRF
        End if

        Call REPRF
     *    (MPRF,FPRF,
     *     CPID,CPAC,CPDT,CPDE,LHDR,CHDR,LFTR,CFTR,NABC,CABC,LPRF,LPCI,
     *     BLOG,FABC,P0,
     *     CDIS,JDIP,MDIS,NDIP,
     *     CNOR,JNOP,JNOR,MNOR,NNOR,NNPR,CNTX,RNOP,
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT,
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

* read null model

        If(NABC.GE.20) then 
           DABC='ACDEFGHIKLMNPQRSTVWY'
           MABC=20
        Else
           DABC='ACGT'
           MABC=4
        End if

        If     (FNUL.NE.' ') then 
           Call RHNUL(NNUL,FNUL,FABC,MABC,IRC)
        Else if(P0.NE.0.0) then
           Call NRNUL(NABC,CABC,FABC,P0,DABC,MABC) 
        Else
           Call DFNUL(FABC,P0,DABC,MABC)
        End if

* reduce / rearrange alphabet

           K1=0
        Do I1=1,NABC
           N1=Index(DABC(1:MABC),CABC(I1)) 
           If(N1.NE.0) then
              IABC(N1)=I1
              K1=K1+1
           End if
        End do 

        If(K1.NE.MABC) then
           Write(NERR,'(
     *      ''Error termination: ''
     *      ''Alphabets of profile and null model incompatible.''
     *        )')
           Stop
        End if

        Do I1=0,LPRF
              Do I2=1,MABC
                 JABC(I2)=IIPP(IABC(I2),I1)
              End do
              Do I2=1,MABC
                 IIPP(I2,I1)=JABC(I2)
              End do
           If(I1.NE.0) then  
              Do I2=1,MABC
                 JABC(I2)=IMPP(IABC(I2),I1)
              End do
              Do I2=1,MABC
                 IMPP(I2,I1)=JABC(I2)
              End do
           End if
        End do 
        NABC=MABC
        Do I1=1,NABC
           CABC(I1)=DABC(I1:I1) 
        End do 

* convert profile into Log(Prob) 

        If     (DL  .NE.0.0) then 
           DL=LOG(DL)
        Else if(BLOG.NE.0.0) then 
           DL=LOG(BLOG)
        Else
           DL=(LOG(2.0)/30.0)
        End if

        Do I1=0,LPRF
           Do I2=0,46 
              If(I1.EQ.0.OR.I2.GT.27) then
                 RIHM(I2,I1)=Real(IIPP(I2,I1))*DL
              Else
                 RMHM(I2,I1)=Real(IMPP(I2,I1))*DL
                 RIHM(I2,I1)=Real(IIPP(I2,I1))*DL
              End if
           End do 
        End do 
        FLOW=Real(NLOW)*DL

* subtract null model

        Do I1=1,NABC
           R1=LOG(FABC(I1))
              RIHM(I1, 0)=RIHM(I1, 0)+R1
           Do I2=1,LPRF
              RIHM(I1,I2)=RIHM(I1,I2)+R1
              RMHM(I1,I2)=RMHM(I1,I2)+R1
           End do
        End do
          
* modify begin state

        RIHM(MM,   0)=RIHM(BM,   0)
        RIHM(MI,   0)=RIHM(BI,   0)
        RIHM(MD,   0)=RIHM(BD,   0)
        RIHM(DM,   0)=FLOW
        RIHM(DI,   0)=FLOW
        RIHM(DD,   0)=FLOW

* modify end state 

        RIHM(MM,LPRF)=RIHM(ME,LPRF)
        RIHM(IM,LPRF)=RIHM(IE,LPRF)
        RIHM(DM,LPRF)=RIHM(DE,LPRF)
        RIHM(MD,   0)=FLOW
        RIHM(ID,   0)=FLOW
        RIHM(DD,   0)=FLOW
         
* scale HMM

        If(OPTS) then 
           Call SCHMM(NOUT,
     *        IDMP,RIHM,RMHM,LPRF,NABC,FLOW,FSCA,OPTF,OPFF,RD,RI)  
        Else 
           Call SCHMM(NERR,
     *        IDMP,RIHM,RMHM,LPRF,NABC,FLOW,FSCA,OPTF,OPFF,RD,RI)  
        End if

* print HMM  

        If(OPTS) then
           If(NABC.EQ.4) then
                 R         =RIHM(3, 0) 
                 RIHM(3, 0)=RIHM(2, 0) 
                 RIHM(2, 0)=R
              Do I1=1,LPRF
                 R         =RIHM(3,I1) 
                 RIHM(3,I1)=RIHM(2,I1) 
                 RIHM(2,I1)=R
                 R         =RMHM(3,I1) 
                 RMHM(3,I1)=RMHM(2,I1) 
                 RMHM(2,I1)=R
              End do
           End if
           Call WRSAM(NOUT,
     *        IDMP,RIHM,RMHM,LPRF,NABC,FABC,FLOW,FSCA,DL) 
        Else
           Call WRHMR(NOUT,NERR,
     *        IDMP,RIHM,RMHM,LPRF,NABC,CABC,FLOW,FSCA,DL) 
        End if

  100   Stop
        End
*----------------------------------------------------------------------*
        Subroutine Repar(FPRF,FNUL,OPTF,OPFF,OPTH,OPTS,RD,RI,DL,IRC)

        Character*64      FPRF
        Character*64      FNUL
        Character*64      CARG 

        Logical           OPTF
        Logical           OPFF
        Logical           OPTH
        Logical           OPTS

        Real*8            DL

        IRC=0

* initializations

        FPRF='-'
        FNUL=' '

        OPTF=.FALSE.
        OPFF=.FALSE.
        OPTH=.TRUE.
        OPTS=.FALSE.

        DL=1.0233739
        RI=0.99
        RD=0.9
 
        N1=Iargc()

           K1=0
        Do I1=1,N1
           Call GetArg(I1,CARG)
           If     (CARG(1:1).EQ.'-'.AND.CARG(2:2).NE.' ') then
              If(Index(CARG,'f').NE.0) OPTF=.TRUE.
              If(Index(CARG,'F').NE.0) OPFF=.TRUE.
              If(Index(CARG,'h').NE.0) OPTH=.TRUE.
              If(Index(CARG,'s').NE.0) OPTS=.TRUE.
           Else if(CARG(1:2).EQ.'D=') then
              Read(CARG(3:64),*,Err=900) RD 
           Else if(CARG(1:2).EQ.'I=') then
              Read(CARG(3:64),*,Err=900) RI
           Else if(CARG(1:2).EQ.'L=') then
              Read(CARG(3:64),*,Err=900) DL
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
 
  100   Return
  900   IRC=-1   
        End
*----------------------------------------------------------------------*
        Subroutine NRNUL(NABC,CABC,FABC,P0,DABC,MABC) 

        Character         CABC(0:26)
        Real              FABC(0:26)
        Character*20      DABC
        Real              RABC(20)

           X=0
        Do I1=1,NABC
           N1=Index(DABC(1:MABC),CABC(I1))
           If(N1.NE.0) then
              RABC(N1)=FABC(I1)
              X=X+RABC(N1)
           End if
        End do

        X=P0/X 

        Do I1=1,MABC
           FABC(I1)=RABC(I1)*X
        End do

        Return
        End
*----------------------------------------------------------------------*
        Include          'reprf.f'
        Include          'rhnul.f'
        Include          'wrsam.f' 
        Include          'wrhmr.f' 
        Include          'schmm.f' 
        Include          'dfnul.f'
        Include          'lblnk.f' 
