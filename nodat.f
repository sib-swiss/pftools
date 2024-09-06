*       Version:  This file is part of pftools release 0.1 January 1995
*----------------------------------------------------------------------*     
* NORMALIZATION 

        Parameter        (KNOR=2)
        Parameter        (KNPM=5)
        Parameter        (MAXN=8)

        Character*16      CNOR(KNOR)
           Data              CNOR(1)/'LINEAR          '/
           Data              CNOR(2)/'GLE_ZSCORE      '/
        Integer           JNOP(KDIS)
           Data              JNOP(1)/2/
           Data              JNOP(2)/5/

        Integer           JNOR
        Integer           MNOR(MAXN)
        Integer           NNOR(MAXN)
        Integer           NNPR(MAXN)
        Character*32      CNTX(MAXN)
        Real              RNOP(KNPM,MAXN)
