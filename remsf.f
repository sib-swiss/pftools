*       Version:  This file is part of pftools release 2.0 June 1997
*----------------------------------------------------------------------*     
        Subroutine REMSF
     *    (NERR,NMSF,FMSF,
     *     IDM1,CSEQ,NSEQ,LSEQ,
     *     IDM3,RWGT,SQID,
     *     IRC)

        Character*(*)    FMSF

        Character        CSEQ(IDM1) 
        Real             RWGT(IDM3)
        Character*(*)    SQID(IDM3) 

        Logical          LBKS
        Character*512    RCIN 

        IRC=0
        LBKS=.FALSE.

        If(FMSF.NE.'-') then 
           Open(NMSF,File=FMSF,Status='OLD',Err=901)
        End if 

    1   Read(NMSF,'(A)',End=902) RCIN        
        If(Index(RCIN,'..').EQ.0) go to   1

        IX=Index(RCIN,'MSF:')
        Read(RCIN(IX+4:),*,Err=903) LSEQ

* names

        NSEQ=0
    2   Read(NMSF,'(A)',End=904) RCIN        
        If(RCIN(1:2).EQ.'//') go to   3
        IX1=Index(RCIN,'Name: ')
        If(IX1.EQ.0) go to   2 
        IX2=Index(RCIN,'Len: ')
        If(IX2.EQ.0) go to   2 
        NSEQ=NSEQ+1
        SQID(NSEQ)=RCIN(IX1+5:IX2-1)
        IX=Index(RCIN,'Weight: ') 
        Read(RCIN(IX+9:),*,Err=905)  RWGT(NSEQ)
        Go to   2

* sequence offset -> NOFS

    3   Read(NMSF,'(A)',End=906) RCIN        
        If(RCIN.EQ.' ') go to   3
        N1=1
        N2=0

* read sequences 

   10   Read(RCIN,*,Iostat=IOS) K1,K2
        If(IOS.NE.0.OR.K1.LE.0.OR.K2.LE.0) then 
           L1=Lblnk(RCIN) 
           Do I1=1,L1
              If(RCIN(I1:I1).NE.' ') go to  11
           End do
   11      J1=I1
           Do I1=J1,L1
              If(RCIN(I1:I1).EQ.' ') go to  12
           End do
   12         J1=I1
           Do I1=J1+1,L1
              If(RCIN(I1:I1).NE.' ') go to  13
           End do 
   13         NOFS=I1-1
              J1=I1
           Do I1=J1,L1
              If(RCIN(I1:I1).NE.' ') N2=N2+1 
           End do 
C          Backspace(NMSF,Err=907)
           LBKS=.TRUE.
        Else if(NOFS.EQ.0) then
           NOFS=Index(RCIN,' 1')
           N2=K2
           If(NOFS.EQ.0) go to   3
        Else
           N1=K1
           N2=K2
        End if 

        Do  20 I1=0,NSEQ-1

           If(LBKS) then
              LBKS=.FALSE.
              Go to 16
           End if           

   15      Read(NMSF,'(A)',End=908) RCIN        
   16      If(RCIN(1:NOFS).EQ.' ') go to  15

           J1=I1*LSEQ+N1  
           Do  19 I2=NOFS+1,Lblnk(RCIN) 
              If(RCIN(I2:I2).NE.' ') then
                 CSEQ(J1)=RCIN(I2:I2)
                 J1=J1+1
              End if 
   19      Continue
           If(J1.NE.I1*LSEQ+N2+1) then
              N3=J1-I1*LSEQ-1
              Write(NERR,*) 'Character count error',N1,N2,N3,J1
              Write(NERR,*) RCIN( 1:79)
              Go to 909
           End if 

   20   Continue

        If(N2.GE.LSEQ) go to 100 

   25   Read(NMSF,'(A)',End=910) RCIN        
        If(RCIN.EQ.' ') go to  25 
        N1=N2+1
        Go to  10

  100   Return

  910   IRC=IRC+1
  909   IRC=IRC+1
  908   IRC=IRC+1
  907   IRC=IRC+1
  906   IRC=IRC+1
  905   IRC=IRC+1
  904   IRC=IRC+1
  903   IRC=IRC+1
  902   IRC=IRC+1
  901   IRC=IRC+1
        Write(NERR,'(''REMSF: Error termination.'',I4)') IRC
        Go to 100

        End
