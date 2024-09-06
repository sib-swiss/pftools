*       Version:  This file is part of pftools release 1.0 January 1996
*----------------------------------------------------------------------*     
        Subroutine RFSEQ
     *    (NSEQ,FSEQ,NABC,CABC,CSID,CSAC,CSDE,LSEQ,ISEQ,IRC)
           
* reads sequence file in Pearson Fasta format 

        Character*(*)     FSEQ
        Character         CABC(0:26)
        Character*(*)     CSID
        Character*(*)     CSAC
        Character*(*)     CSDE
        Integer*2         ISEQ(*)

        Character*256     RCIN
        
        IRC=0

        CSID=' '
        CSAC=' '
        CSDE=' '

        If(RCIN(1:1).EQ.'>') Go to  2
 
        If(FSEQ.NE.'-') Open(NSEQ,File=FSEQ,Status='OLD',Err=999)
    1   Read(NSEQ,'(A)',Err=999,End=901) RCIN
        If(RCIN(1:1).NE.'>') go to   1   

    2   L=Lblnk(RCIN)
        IX2=Index(RCIN(1:L),' ')-1
        Do I1=IX2,2,-1
           If(Index(':;|',RCIN(I1:I1)).NE.0) go to   3
        End do
    3   IX1=I1
        Do I1=IX2+1,L
           If(RCIN(I1:I1).NE.' ') go to  4
        End do
    4   IX3=I1 
        CSAC=RCIN(2:IX1)
        CSID=RCIN(IX1+1:IX2)
        CSDE=RCIN(IX3:L)

           J1=0
   10   Read(NSEQ,'(A)',Err=999,End= 20) RCIN
        L=Lblnk(RCIN)
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

  100   Return

  901   IRC=-1
        Go to 100 
  999   IRC=IRC+1 
        Go to 100 
        End
