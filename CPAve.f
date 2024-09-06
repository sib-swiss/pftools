*       Version:  This file is part of pftools release 2.2 June 1999
*----------------------------------------------------------------------*     
        Subroutine CPAve(IMPP,IDMP,LPRF,CABC,NABC,PAVE)

        Integer           IMPP(0:27,0:IDMP)
        Character         CABC(0:26)
        Real              PAVE(0:26)

        Do  I1=0,NABC
           PAVE(I1)=0
        End Do
        Do  I1=1,LPRF
           Do  I2=0,NABC
              PAVE(I2)=PAVE(I2)+IMPP(I2,I1) 
           End do  
        End Do 

        Return
        End
