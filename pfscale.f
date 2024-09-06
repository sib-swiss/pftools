*       Program pfscale 
*----------------------------------------------------------------------*     
*       Function: Fits paramters of an extreme value distribution to a
*                 profile score distribution. Input: sorted score list.
*       Author:   Philipp Bucher
*       Version:  This file is part of pftools release 2.2 June 1999
*----------------------------------------------------------------------*     
* DATA
*----------------------------------------------------------------------*

* array sizes, I/O units

        Parameter        (IDM1=131072)

        Parameter        (NOUT=     6)
        Parameter        (NSCL=    10)

* Parameters and options

        Character*64      FSCL  
        Character*64      FPRF  

        Real*8            DL
        Integer           NN
        Real              RP
        Real              RQ

* Profile fields

        Parameter        (IDMP=   9999)

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'pfdat.f'
        Include          'dfdat.f'
        Include          'sterr.f'
        Include          'cvini.f'

* score statistics  

        Real              XSCO(IDM1)
        Real              XFRQ(IDM1)
        Real              XWGT(IDM1)

*----------------------------------------------------------------------*
* INPUT SECTION
*----------------------------------------------------------------------*

        IRC=0

* read command line 

        Call Repar(FSCL,FPRF,DL,NN,RP,RQ,IRC) 
        If(FSCL.NE.' '.AND.FSCL.NE.'-') then
           MSCL=NSCL 
           Open(MSCL,File=FSCL,Status='OLD',Iostat=IOP)
        Else
           MSCL=5
        End if
        If(IRC.NE.0.OR.IOP.NE.0) then
           Write(NERR,'(
     *      ''pfscale [ score-list | - ] [ profile-file ] ''
     *      ''[L=#] [N=#] [P=#] [Q=#]'',/, 
     *      ''  e.g. pfscale - N=14147368 P=0.0001 Q=0.000001'',
     *      '' < score-list''
     *        )')
           Stop
        End if

        RL=1/LOG(DL)

        RDBS=RL*LOG(Real(NN))
        If(RQ.NE.0) then
           EMAX=-RL*(LOG(RQ))
        Else
           EMAX=100
        End if
           EMIN=-RL*(LOG(RP))

        Do   5 I1=1,IDM1
           Read(MSCL,*,End= 10) XSCO(I1)
           XFRQ(I1)=RDBS-RL*LOG(I1-0.5)
           XWGT(I1)=I1*0.5
    5   Continue

   10   NSCO=I1-1       

              XSM=0
              XFM=0
              XWM=0
        Do  20 I1=1,NSCO
           If(XFRQ(I1).GE.EMIN.AND.XFRQ(I1).LE.EMAX) then 
              XSM=XSM+XWGT(I1)*XSCO(I1)
              XFM=XFM+XWGT(I1)*XFRQ(I1)
              XWM=XWM+XWGT(I1)
           End if
   20   Continue
              XSM=XSM/XWM
              XFM=XFM/XWM


              XSV=0
              XFV=0
              XCO=0
        Do  30 I1=1,NSCO
           If(XFRQ(I1).GE.EMIN.AND.XFRQ(I1).LE.EMAX) then 
              XSV=XSV+XWGT(I1)*( (XSCO(I1)-XSM)**2 ) 
              XFV=XFV+XWGT(I1)*( (XFRQ(I1)-XFM)**2 )
              XCO=XCO+XWGT(I1)*(XFRQ(I1)-XFM)*(XSCO(I1)-XSM)
           End if
   30   Continue
              XSV=(XSV/XWM)**0.5
              XFV=(XFV/XWM)**0.5
              XCO=(XCO/XWM)/(XFV*XSV)

        XB=XCO*XFV/XSV
        XA=XFM-XB*XSM

        If(FPRF.NE.' ') Go to  50 

* Case 1: no profile input file - print list

        Write(NOUT,
     *  '(''# -LogP ='',F8.4,'' + '',F12.8,'' * raw-score'')') XA,XB 
 
        Write(NOUT,'(''#'')')
        Write(NOUT,'(''#   rank raw-score  -logFreq  -logProb'')')
        Write(NOUT,'(''#'')')

        Do  40 I1=1,NSCO
           XPRE=XA+XB*XSCO(I1) 
           Write(NOUT,'(I8,F10.2,F10.4,F10.4)')
     *        I1,XSCO(I1),XFRQ(I1),XPRE
   40   Continue
        Go to 100

* Case 2: Modify profile input file

   50   Continue 

        Call REPRF
     *    (NPRF,FPRF,
     *     CPID,CPAC,CPDT,CPDE,LHDR,CHDR,LFTR,CFTR,NABC,CABC,LPRF,LPCI,
     *     BLOG,FABC,P0,
     *     CDIS,JDIP,MDIS,NDIP,
     *     CNOR,JNOP,JNOR,MNOR,NNOR,NNPR,CNTX,RNOP,
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT,
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

* Check normalization modes (NO mode J1 will be updated)

           J1=1
           K1=NNPR(J1)
        Do I1=2,JNOR
           If(NNPR(I1).LT.K1) then
              K1=NNPR(I1)
              J1=I1
           End if 
        End do 
        CNTX(J1)='-LogE'
        If(MNOR(J1).NE.1) then
           Write(NERR,'(
     *      ''1st priority normalization mode not linear - '',
     *      ''scaling impossible.''
     *        )')
            Stop
        End if 

* add normalisation parameters: 

        RNOP(1,J1)=XA
        RNOP(2,J1)=XB

* define cut-offs::

        Do I1=1,JCUT
           Call NtoR
     *       (RCUT(J1,I1),ICUT(I1),RNOP,KNPM,MAXN,J1,1,LSEQ,RAVE)
        End do 

* rescaling command

        LFTR=LFTR+1
        Do I1=LFTR,2,-1
           CFTR(I1)=CFTR(I1-1)
        End do

        CFTR(1)='CC   /RESCALED_BY="'
        Call Recmd(CFTR(1)(21:130))
        IC=Lblnk(CFTR(1))
        CFTR(1)(IC+1:)='";'

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
        Subroutine Repar(FSCL,FPRF,DL,NN,RP,RQ,IRC) 


        Character*64      FSCL  
        Character*64      FPRF

        Real*8            DL
        Integer           NN
        Real              RP
        Real              RQ

        Character*64      CARG 

* initializations

        FSCL=' '
        FPRF=' '
        DL=10.0
        NN=14147368
        RP=0.0001
        RQ=0.000001

* interpret command line arguments 

        N1=Iargc()

           K1=0
        Do I1=1,N1
           Call GetArg(I1,CARG)
           If     (CARG(1:1).EQ.'-'.AND.CARG(2:2).NE.' ') then
              Go to 900 
           Else if(CARG(1:3).EQ.'L=e'.OR.CARG(1:3).EQ.'L=E') then
              DL=EXP(1.0) 
           Else if(CARG(1:2).EQ.'L=') then
              Read(CARG(3:64),*,Err=900) DL 
           Else if(CARG(1:2).EQ.'N=') then
              Read(CARG(3:64),*,Err=900) NN 
           Else if(CARG(1:2).EQ.'P=') then
              Read(CARG(3:64),*,Err=900) RP
           Else if(CARG(1:2).EQ.'Q=') then
              Read(CARG(3:64),*,Err=900) RQ
           Else if(K1.LE.1) then
              K1=K1+1
              If     (K1.EQ.1) then
                FSCL=CARG
              Else if(K1.EQ.2) then
                FPRF=CARG
              End if
           Else
              Go to 900
           End if
        End do
 
  100   Return
  900   IRC=-1  
        Return 
        End
*----------------------------------------------------------------------*
        Include          'reprf.f'
        Include          'wrprf.f'
        Include          'recmd.f'
        Include          'NtoR.f'
        Include          'lblnk.f'
