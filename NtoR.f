*       Version:  This file is part of pftools release 2.1 February 1998
*----------------------------------------------------------------------*     
        Subroutine NtoR(R,N,RNOP,KNPM,MAXN,INOR,IFUN,LSEQ,RAVE)

        Real              RNOP(KNPM,MAXN)

        If     (IFUN.EQ.1) then 
           X=(R-RNOP(1,INOR)) / RNOP(2,INOR) 
        Else if(IFUN.EQ.2) then 
           X=( RNOP(1,INOR)*(1.0-EXP(RNOP(2,INOR)*LSEQ-RNOP(3,INOR) )))
     *      *( RNOP(5,INOR) * R + RNOP(4,INOR) )
        Else if(IFUN.EQ.3) then 
           X=( RNOP(1,INOR)*(1.0-EXP(RNOP(2,INOR)*LSEQ-RNOP(3,INOR) )))
     *      *( RNOP(5,INOR) * R + RNOP(4,INOR) ) + RAVE
        End if 
           N=INT(X)
           If(Real(N).LT.X) N=N+1
        Return
        End 
