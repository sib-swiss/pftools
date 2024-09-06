*        Version:  This file is part of pftools release 2.0 June 1997
*----------------------------------------------------------------------*     
        Subroutine RESEQ
     *    (NSEQ,FSEQ,NABC,CABC,CSID,CSAC,CSDE,LSEQ,ISEQ,LEOF,RCIN,IRC)
           
* reads sequence file in Swiss-Prot/EMBL format 

        Character*(*)     FSEQ
        Character         CABC(0:26)
        Character*(*)     CSID
        Character*(*)     CSAC
        Character*(*)     CSDE
        Integer*2         ISEQ(*)
        Logical           LEOF 
        Logical           LOPN
        Character*(*)     RCIN
        
        IRC=0
        Inquire(File=FSEQ,OPENED=LOPN)
        If(LOPN) go to   1
        If(FSEQ.NE.'-') Open(NSEQ,File=FSEQ,Status='OLD',Err=902)
    1   Read(NSEQ,'(A)',Err=999,End=901,Iostat=IOS) RCIN
        If(RCIN(1:1).EQ.'>') Go to 903
        If(RCIN(1:2).NE.'ID') go to   1   
        IC=Index(RCIN(6:17),' ')+5
        CSID=RCIN( 6:IC)

    2   Read(NSEQ,'(A)',Err=999,End=999,Iostat=IOS) RCIN
        If(RCIN(1:2).NE.'AC'.AND.RCIN(1:2).NE.'DE') go to   2   
        If(RCIN(1:2).EQ.'AC') then 
           IX=Index(RCIN,';')-1
           CSAC=RCIN(6:IX) // '|'
        Else
           CSDE=RCIN( 6:80)
           Go to   4
        End if 

    3   Read(NSEQ,'(A)',Err=999,End=999,Iostat=IOS) RCIN
        If(RCIN(1:2).NE.'DE') go to   3   
        CSDE=RCIN( 6:80)

    4   Read(NSEQ,'(A)',Err=999,End=999,Iostat=IOS) RCIN
        If(RCIN(1:2).NE.'DE') go to   5   
        LR=Lblnk(CSDE)
        If(CSDE(LR:LR).EQ.Char(13)) LR=LR-1
        CSDE=CSDE(1:LR) // ' ' // RCIN( 6:80)

    5   Continue
        If(RCIN(1:2).EQ.'SQ') go to   7

    6   Read(NSEQ,'(A)',Err=999,End=999,Iostat=IOS) RCIN
        If(RCIN(1:2).NE.'SQ') go to   6   

    7   Continue

           J1=0
   10   Read(NSEQ,'(A)',Err=999,End= 20,Iostat=IOS) RCIN
        If(RCIN(1:2).EQ.'//') go to  20

        Do 15 I1=1,80
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

  100   If(IOS.EQ.-1) LEOF=.TRUE.
        Return

  901   IRC=-1
        Go to 100
  903   IRC=IRC+1
  902   IRC=IRC+1
  999   IRC=IRC+1 
        Go to 100 
        End
