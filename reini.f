*       Version:  This file is part of pftools release 1.1 March 1996
*----------------------------------------------------------------------*     
        Subroutine REINI(NINI,FINI,NABC,CABC,FABC,MABC,IABC,IRC)
      
        Character*64      FINI
        Character         CABC(0:26)
        Real              FABC(*)
        Integer           IABC(*) 
        Character*132     RCIN

* open profile file 

        Do I1=1,NABC
           FABC(I1)=0.0
        End do  

        Open(NINI,File=FINI,Status='OLD',Err=900)

        K1=0
    1   Read(NINI,'(A)',Err=900,End=100) RCIN
        L=Lblnk(RCIN
        If(RCIN.EQ.' '.OR.RCIN(1:1).EQ.'#') go to   1

        Do I1=1,L
           If(RCIN(I1:I1).NE.' ') go to   2
        End Do 
        Go to   1
    2   J1=I1 

        Do I1=1,NABC
           If(RCIN(J1:J1).EQ.CABC(I1)) go to   3
        End Do 
        Go to   1
    3   Read(RCIN(J1+1:L),*,Err=1) R1
        K1=K1+1
        FABC(K1)=R1
        IABC(K1)=I1
        Go to   1 

  100   MABC=K1 
        Return
  900   IRC=-1
        Go to 100
        End 
