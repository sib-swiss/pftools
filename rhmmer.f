*       Version:  This file is part of pftools release 2.0 June 1997
*----------------------------------------------------------------------*     
        Subroutine RHMMER(NPRF,FPRF,LPRF,RIHM,RMHM,IDMP,NABC,CABC,IRC)

        Include          'pfind.f' 

        Character*(*)     FPRF

        Include          'hmdat.f'

        Character         CABC(0:26)

        Character*80      RCIN

* open HMM file

        If(FPRF.NE.'-') then
           Open(NPRF,File=FPRF,Status='OLD',Err=900)
        End if

* read header lines

    1   Read(NPRF,'(A)',Err=900,End=900) RCIN
        If(RCIN(1:1).EQ.'#') Go to   1
        Read(RCIN,*,Err=900,End=900) LPRF 
    2   Read(NPRF,'(A)',Err=900,End=900) RCIN
        If(RCIN(1:1).EQ.'#') Go to   2
        Read(RCIN,*,Err=900,End=900) NABC
    3   Read(NPRF,'(A)',Err=900,End=900) RCIN
        If(RCIN(1:1).EQ.'#') Go to   3

    4   Read(NPRF,'(A)',Err=900,End=900) RCIN
        If(RCIN(1:1).EQ.'#') Go to   4
        Read(RCIN,'(26A)',Err=900,End=900)
     *    (CABC(ii1),ii1=1,NABC)
    5   Read(NPRF,'(A)',Err=900,End=900) RCIN
        If(RCIN(1:1).EQ.'#') Go to   5

    6   Read(NPRF,'(A)',Err=900,End=900) RCIN
        If(RCIN(1:1).EQ.'#') Go to   5

* read model parameters

        Do I1=0,LPRF

* - match state

           If(next(NPRF,RCIN).NE.0) Go to 900 
              Read(RCIN,*,Err=900) RIHM(MM,I1)
           If(next(NPRF,RCIN).NE.0) Go to 900 
              Read(RCIN,*,Err=900) RIHM(MD,I1)
           If(next(NPRF,RCIN).NE.0) Go to 900 
              Read(RCIN,*,Err=900) RIHM(MI,I1)

           Do I2=1,NABC
              If(next(NPRF,RCIN).NE.0) Go to 900 
                 Read(RCIN,*,Err=900) RMHM(I2,I1)
           End do 

* - delete state

           If(next(NPRF,RCIN).NE.0) Go to 900 
              Read(RCIN,*,Err=900) RIHM(DM,I1)
           If(next(NPRF,RCIN).NE.0) Go to 900 
              Read(RCIN,*,Err=900) RIHM(DD,I1)
           If(next(NPRF,RCIN).NE.0) Go to 900 
              Read(RCIN,*,Err=900) RIHM(DI,I1)

           RMHM( D,I1)=1.0

* - insert state

           If(next(NPRF,RCIN).NE.0) Go to 900 
              Read(RCIN,*,Err=900) RIHM(IM,I1)
           If(next(NPRF,RCIN).NE.0) Go to 900 
              Read(RCIN,*,Err=900) RIHM(ID,I1)
           If(next(NPRF,RCIN).NE.0) Go to 900 
              Read(RCIN,*,Err=900) RIHM(II,I1)

           Do I2=1,NABC
              If(next(NPRF,RCIN).NE.0) Go to 900 
                 Read(RCIN,*,Err=900) RIHM(I2,I1)
           End do 

        End do 

  100   Return
  900   IRC=-1
        Go to 100
        End
*----------------------------------------------------------------------*
        Integer function next(NPRF,RCIN)
        Character*(*)     RCIN       
    1   Read(NPRF,'(A)',Iostat=NEXT) RCIN
        If(RCIN(1:1).EQ.'#') go to   1
        Return
        End
