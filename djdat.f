*       Version:  This file is part of pftools release 0.1 January 1995
*----------------------------------------------------------------------*     
* DISJOINT 

        Parameter        (KDIS=2)
        Parameter        (KDPM=2)

        Character*16      CDIS(KDIS)
           Data              CDIS(1)/'UNIQUE          '/
           Data              CDIS(2)/'PROTECT         '/
        Integer           JDIP(KDIS)
           Data              JDIP(1)/0/
           Data              JDIP(2)/2/

        Integer           MDIS 
        Integer           NDIP(KDPM)     
