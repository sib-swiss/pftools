*       Version:  This file is part of pftools release 0.1 January 1995
*----------------------------------------------------------------------*     
        Subroutine RFSEQ
     *    (NSEQ,FSEQ,NABC,CABC,CSID,CSDE,LSEQ,ISEQ,IRC)
           
* reads sequence file in Pearson Fasta format 

        Character*(*)     FSEQ
        Character         CABC(0:26)
        Character*(*)     CSID
        Character*(*)     CSDE
        Integer*2         ISEQ(*)

        Character*256     RCIN
        
        IRC=0

        Open(NSEQ,File=FSEQ,Status='OLD',Err=999)
    1   Read(NSEQ,'(Q,A)',Err=999,End=901) L,RCIN
        If(RCIN(1:1).NE.'>') go to   1   
        IC=MIN(13,Index(RCIN(1:L),' '))
        CSID=RCIN( 2:IC)
        CSDE=RCIN( 2:L)

           J1=0
   10   Read(NSEQ,'(Q,A)',Err=999,End= 20) L,RCIN
        If(RCIN(1:1).EQ.'>') go to  20

        Do 15 I1=1,L
              N1=0
              K1=Ichar(RCIN(I1:I1))
              If(K1.GE.97) then 
                 K1=K1-32
                 RCIN(I1:I1)=Char(K1)
              End if
              If(K1.GT.90.OR.K1.LT.65) go to  15 
                  
           Do 13 I2=1,NABC
              If(CABC(I2).EQ.RCIN(I1:I1)) then
                 N1=I2
                 Go to   14
              End if
   13      Continue
   14      J1=J1+1
           ISEQ(J1)=N1
   15   Continue
        Go to  10 

   20   LSEQ=J1
        Backspace(NSEQ)

  100   Return

  901   IRC=-1
        Go to 100 
  999   IRC=IRC+1 
        Go to 100 
        End
