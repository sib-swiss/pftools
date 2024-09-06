*       Program ptof 
*----------------------------------------------------------------------*     
*       Function: converts a protein profile into a framsearch profile
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

* parameters and options

        Logical           OPTR

        Real              RB
        Real              RF
        Real              RI
        Real              RX 
        Real              RY 
        Real              RZ 
 
* initialization of controlled vocabularies

        Include          'cvini.f' 

*----------------------------------------------------------------------*     
* INPUT SECTION 
*----------------------------------------------------------------------*     

        IRC=0
        FPRF='stdout'

* command line arguments
 
        Call Repar(FPRF,OPTR,RB,RF,RI,RX,RY,RZ,IRC)
        If(IRC.NE.0) then
           Write(NERR,'(
     *      ''Usage: ptof [ profile | - ]  parameters '',/
     *      ''   valid parameters are:'',//,
     *      ''       B=minimal-begin-end-score(default=- 50)'',/
     *      ''       F=deletion-frameshift    (default=-100)'',/
     *      ''       I=insert-score-multiplier(default= 1/3)'',/
     *      ''       X=stop-codon-match-score (default=-100)'',/
     *      ''       Y=intron-opening-penalty (default=-300)'',/
     *      ''       Z=intron-length-penalty  (default=  -1)'',/
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

        If(IRC.NE.0) Stop

* add command-line to footer lines

        LFTR=LFTR+1
        Do I1=LFTR,2,-1
           CFTR(I1)=CFTR(I1-1)
        End do 

        CFTR(1)='CC   /GENERATED_BY="'
        Call Recmd(CFTR(1)(21:130))
        IC=Lblnk(CFTR(1))
        CFTR(1)(IC+1:)='";'

*----------------------------------------------------------------------*     
* CONVERSION SECTION
*----------------------------------------------------------------------*     

* Adjust frame search parameters to normalization function      

        If(OPTR) then
            R2=0.01 
               K1=1
               J1=NNPR( 1)
            Do I1=2,JNOR
               If(NNPR(I1).LT.J1) then
                  K1=I1
                  JN=NNPR(I1) 
               End if
            End do
            If(MNOR(K1).EQ.1) R2=RNOP(2,K1)
 
            RB=RB / R2
            RF=RF / R2
            RX=RX / R2
            RY=RY / R2
            RZ=RZ / R2
        End if 

* Expand alphabet

        NABC=NABC+1
        CABC(NABC)='O'
        NX=NINT(RX)
              J2=0
           Do I2=1,NABC-1
              J2=J2+IIPD(I2)
           End do
           IIPD(NABC)=J2/(NABC-1)
              J2=0
           Do I2=1,NABC-1
              J2=J2+IIPP(I2,0)
           End do
           IIPP(NABC,0)=J2/(NABC-1)
        Do I1=1,LPRF
           IMPP(NABC,I1)=NX
              J2=0
           Do I2=1,NABC-1
              J2=J2+IIPP(I2,I1)
           End do
           IIPP(NABC,I1)=J2/(NABC-1)
        End do 

* - default begin, end score

        IIPD(B0)=MAX(IIPD(B0),NINT(RB))
        IIPD(B1)=MAX(IIPD(B1),NINT(RB))
        IIPD(E0)=MAX(IIPD(E0),NINT(RB))
        IIPD(E1)=MAX(IIPD(E1),NINT(RB))

* - eliminate II transitions

        Do I1=0,LPRF
           If(IIPP(BI,I1).NE.NLOW) IIPP(BI,I1)=IIPP(BI,I1)-IIPP(II,I1)
           If(IIPP(MI,I1).NE.NLOW) IIPP(MI,I1)=IIPP(MI,I1)-IIPP(II,I1)
           If(IIPP(DI,I1).NE.NLOW) IIPP(DI,I1)=IIPP(DI,I1)-IIPP(II,I1)
           Do I2=0,26
              If(IIPP(I2,I1).NE.NLOW)
     *           IIPP(I2,I1)=IIPP(I2,I1)+IIPP(II,I1)
           End do
           IIPP(II,I1)=0
        End do       

* Expand profile

* -define architecture of profile according to topology 

        If(LPCI) then 
           NBB=1
           NB=2
           NE=3*LPRF-1
           NEE=3*LPRF
        Else 
           NBB=1
           NB=1
           NE=3*LPRF-2
           NEE=3*LPRF-2
        End if       

* - move last insert position to new end of profile

        Do I1=0,46
           IIPP(I1,NEE)=IIPP(I1,LPRF)
        End do 
        CHIP(NEE)=CHIP(LPRF)

* - move other positions 

           K1=NE
        Do I1=LPRF,1,-1
           Do I2=0,27
              IMPP(I2,K1)=IMPP(I2,I1) 
           End do
           CHMP(K1)=CHMP(I1)
           K1=K1-3
        End do 

           K1=NE-2
        Do I1=LPRF-1,1,-1
           Do I2= 0,26
              IIPP(I2,K1)=NINT(IIPP(I2,I1)*RI)
           End do
           Do I2=27,46
              IIPP(I2,K1)=IIPP(I2,I1)
           End do
           CHIP(K1)=CHIP(I1)
           K1=K1-3
        End do 

* - fill in m-1, m+1 positions

        Do I1=NB+1,NEE,3
           Do I2=0,27
              IMPP(I2,I1)=0
           End do
           CHMP(I1)='>'
        End do

        J1=NB-1
        If(J1.LE.0) J1=J1+3
        Do I1=J1,NEE,3
           Do I2=0,27
              IMPP(I2,I1)=0
           End do
           CHMP(I1)='<'
        End do
       
*- fill i-1 position 
     
           Do I2= 0,26
              IIPP(I2,NB)=0
           End do
           Do I2=27,46
              IIPP(I2,NB)=NLOW
           End do
              IIPP(MM,NB)=0
              IIPP(DD,NB)=0
              IIPP(MI,NB)=NINT(RF/2)
              IIPP(II,NB)=NINT(RF)
              IIPP(IM,NB)=NINT(RF/2)
           CHIP(NB)='*'
        Do I1=NB+3,NEE-1,3
           Do I2= 0,47
              IIPP(I2,I1)=IIPP(I2,NB)
           End do
           CHIP(I1)='*'
        End do 

* - fill i+1 position 
     
           J1=3
           If(LPCI) J1=1
           Do I2= 0,26
              IIPP(I2,J1)=0
           End do
           Do I2=27,46
              IIPP(I2,J1)=NLOW
           End do
              IIPP(MM,J1)=0
              IIPP(DD,J1)=0
              IIPP(MI,J1)=NINT(RY/2)
              IIPP(II,J1)=MIN(-1,NINT(RZ))
              IIPP(IM,J1)=NINT(RY/2)
           CHIP(J1)=':'
        Do I1=J1+3,NEE-1,3
           Do I2=0,46
              IIPP(I2,I1)=IIPP(I2,J1)
           End do
           CHIP(I1)=':'
        End do 

* frameshift deletetion scores: 

        J1=2   
        If(LPCI) J1=1
        K1=3
        If(LPCI) K1=1
        Do I1=J1,LPRF-1
           IIPP(MD,K1)=(NINT(RF-IMPP( D,K1+1))/2)
           IIPP(DM,K1+1)=NINT(RF-IIPP(MD,K1)-IMPP( D,K1+1))
           K1=K1+3
        End do 

* begin and end scores

        If(.NOT.LPCI) then
           Do I1=2,NEE,3
              IIPP(B0,I1-1)=IIPP(B0,I1)
              IIPP(B1,I1-1)=IIPP(B1,I1) 
              IIPP(BM,I1-1)=IIPP(BM,I1) 
              IIPP(BI,I1-1)=IIPP(BI,I1) 
              IIPP(BD,I1-1)=IIPP(BD,I1) 
              IIPP(E0,I1-1)=IIPP(E0,I1)
              IIPP(E1,I1-1)=IIPP(E1,I1)
              IIPP(ME,I1-1)=IIPP(ME,I1) 
              IIPP(IE,I1-1)=IIPP(IE,I1) 
              IIPP(DE,I1-1)=IIPP(DE,I1) 
              IIPP(B0,I1+1)=IIPP(B0,I1)
              IIPP(B1,I1+1)=IIPP(B1,I1)
              IIPP(BM,I1+1)=IIPP(BM,I1) 
              IIPP(BI,I1+1)=IIPP(BI,I1) 
              IIPP(BD,I1+1)=IIPP(BD,I1) 
              IIPP(E0,I1+1)=IIPP(E0,I1)
              IIPP(E1,I1+1)=IIPP(E1,I1)
              IIPP(ME,I1+1)=IIPP(ME,I1) 
              IIPP(IE,I1+1)=IIPP(IE,I1) 
              IIPP(DE,I1+1)=IIPP(DE,I1) 
           End do
        Else
           Do I1=2,NEE,3
              IIPP(B0,I1-1)=IIPP(B0,I1-2)
              IIPP(B1,I1-1)=IIPP(B1,I1-2) 
              IIPP(BM,I1-1)=IIPP(BM,I1-2) 
              IIPP(BI,I1-1)=IIPP(BI,I1-2) 
              IIPP(BD,I1-1)=IIPP(BD,I1-2) 
              IIPP(E0,I1-1)=IIPP(E0,I1-2)
              IIPP(E1,I1-1)=IIPP(E1,I1-2)
              IIPP(ME,I1-1)=IIPP(ME,I1-2) 
              IIPP(IE,I1-1)=IIPP(IE,I1-2) 
              IIPP(DE,I1-1)=IIPP(DE,I1-2) 
              IIPP(B0,I1  )=IIPP(B0,I1+1)
              IIPP(B1,I1  )=IIPP(B1,I1+1)
              IIPP(BM,I1  )=IIPP(BM,I1+1) 
              IIPP(BI,I1  )=IIPP(BI,I1+1) 
              IIPP(BD,I1  )=IIPP(BD,I1+1) 
              IIPP(E0,I1  )=IIPP(E0,I1+1)
              IIPP(E1,I1  )=IIPP(E1,I1+1)
              IIPP(ME,I1  )=IIPP(ME,I1+1) 
              IIPP(IE,I1  )=IIPP(IE,I1+1) 
              IIPP(DE,I1  )=IIPP(DE,I1+1) 
           End do
        End if 

        Do I1=NBB,NEE-1
           IIPP(B0,I1)=MAX(IIPD(B0),IIPP(B0,I1)) 
           IIPP(B1,I1)=MAX(IIPD(B1),IIPP(B1,I1)) 
           IIPP(E0,I1)=MAX(IIPD(E0),IIPP(E0,I1)) 
           IIPP(E1,I1)=MAX(IIPD(E1),IIPP(E1,I1)) 
        End do

* disjointness definition

        If(MDIS.EQ.2) then
           NDIP(1)=NDIP(1)*3-2
           NDIP(2)=NDIP(2)*3-2
        End if

* adjust length

        LPRF=NEE

*----------------------------------------------------------------------*     
* OUTPUT SECTION
*----------------------------------------------------------------------*     

* write profile

        Call WRPRF
     *    (NOUT,
     *     CPID,CPAC,CPDT,CPDE,LHDR,CHDR,LFTR,CFTR,NABC,CABC,LPRF,LPCI,
     *     CDIS,JDIP,MDIS,NDIP,
     *     CNOR,JNOP,JNOR,MNOR,NNOR,NNPR,CNTX,RNOP,
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT,
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     BLOG,FABC,P0,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

  100   Stop
        End
*----------------------------------------------------------------------*
        Subroutine Repar(FPRF,OPTR,RB,RF,RI,RX,RY,RZ,IRC)

        Character*64      FPRF
        Character*64      CARG 

        Logical           OPTR
        Real              RB
        Real              RF
        Real              RI
        Real              RX
        Real              RY
        Real              RZ

        IRC=0
     

* initializations

        FPRF='-'

        OPTR=.FALSE.
        RB=-50
        RF=-100
        RI=1/3.0
        RX=-100
        RY=-300
        RZ=-1
   
        N1=Iargc()

           K1=0
        Do I1=1,N1
           Call GetArg(I1,CARG)
           If     (CARG(1:1).EQ.'-'.AND.CARG(2:2).NE.' ') then
              If(CARG(1:2).EQ.'-h') go to 900
              If(Index(CARG,'r').NE.0) then
                 OPTR=.TRUE.
                 RB=-0.5
                 RF=-1.0
                 RX=-1.0
                 RY=-3.0
                 RZ=-0.01
              End if
           Else if(CARG(1:2).EQ.'B=') then
              Read(CARG(3:64),*,Err=900) RB 
           Else if(CARG(1:2).EQ.'F=') then
              Read(CARG(3:64),*,Err=900) RF 
           Else if(CARG(1:2).EQ.'I=') then
              Read(CARG(3:64),*,Err=900) RI
           Else if(CARG(1:2).EQ.'X=') then
              Read(CARG(3:64),*,Err=900) RX
           Else if(CARG(1:2).EQ.'Y=') then
              Read(CARG(3:64),*,Err=900) RY
           Else if(CARG(1:2).EQ.'Z=') then
              Read(CARG(3:64),*,Err=900) RZ
           Else if(K1.LE.0) then
              K1=K1+1
              If     (K1.EQ.1) then
                 FPRF=CARG
              End if
           Else
              Go to 900
           End if
        End do
 
  100   Return
  900   IRC=-1   
        Go to 100
        End
*----------------------------------------------------------------------*
        Include          'reprf.f'
        Include          'recmd.f'
        Include          'wrprf.f'
        Include          'lblnk.f' 
