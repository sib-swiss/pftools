*       Version:  This file is part of pftools release 1.2 April 1997
*----------------------------------------------------------------------*     

* Note: for use on machines with less than 32 MB RAM, replace the actual 
*       array Parameter instructions by commented Parameter instructions 
*       or modify the array size definitions manually. 

* max. profile length  

        Parameter        (IDMP=9999)
C       Parameter        (IDMP=2499)

* max. sequence length 

        Parameter        (IDMS=1000000)
C       Parameter        (IDMS= 250000)

* max. number of matches per sequence

        Parameter        (IDMN=2000) 
C       Parameter        (IDMN= 500) 

* max. alignment length 

        Parameter        (IDMA=20000)
C       Parameter        (IDMA= 5000)

* max. path matrix surface

        Parameter        (IDMM=4000000)
C       Parameter        (IDMM=1000000)
