*       Version:  This file is part of pftools release 1.1 March 1996
*----------------------------------------------------------------------*     
        Subroutine CFAve(ISEQ,IDMS,LSEQ,CABC,NABC,FAVE)

        Integer*2         ISEQ(IDMS)
        Character         CABC(0:26)
        Real              FAVE(0:26)

        Do  I1=0,NABC
           FAVE(I1)=0
        End Do
        Do  I1=1,LSEQ
           J1=ISEQ(I1)
           FAVE(J1)=FAVE(J1)+1
        End Do 
        Do  I1=0,NABC
           FAVE(I1)=FAVE(I1)/LSEQ
        End Do

        Return
        End
