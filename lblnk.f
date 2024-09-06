*       Version:  This file is part of pftools release 2.1 February 1998
*----------------------------------------------------------------------*     
        Function          Lblnk(string) 
        Character*(*)     string 

        L=Len(string)

        Do   9 I1=L,1,-1
           If(STRING(I1:I1).NE.' ') go to  10
    9   Continue
   10   Lblnk=I1

        Return
        End
