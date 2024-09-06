*       Version:  This file is part of pftools release 1.0 January 1996
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
