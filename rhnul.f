*       Version:  This file is part of pftools release 2.2 June 1999
*----------------------------------------------------------------------*     
        Subroutine RHNUL(NNUL,FNUL,FABC,NABC,IRC)   

        Character*(*)     FNUL 

        Real              FABC(0:26)

        Character*256     RCIN
        Character*1024    CBUF     

        Open(NNUL,File=FNUL,Status='OLD',Err=900)

    1   Read(NNUL,'(A)',Err=900) RCIN
        If(RCIN(1:1).EQ.'#') Go to   1

        If     (RCIN(1:5).EQ.'Amino'.AND.NABC.EQ.20) then
           Continue
        Else if(RCIN(1:7).EQ.'Nucleic'.AND.NABC.EQ.4) then
           Continue
        Else 
           Go to 900
        End if 

        M=0
    2   Read(NNUL,'(A)',Err=900,End=900) RCIN
        If(RCIN(1:1).EQ.'#') Go to   2
        L=Index(RCIN,'#')
        If(L.EQ.0) then 
           L=Lblnk(RCIN)
        Else
           L=Lblnk(RCIN(1:L-1))
        End if 
        CBUF(M+1:)=RCIN(1:L)
        M=M+L
        CBUF(M+2:M+2)=' @'
        Read(CBUF(1:M+1),*,End=  2,Err=  2)(FABC(ii1),ii1=1,NABC)

  100   Return
  900   End
