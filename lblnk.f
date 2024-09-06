*       Version:  This file is part of pftools release 2.2 June 1999
*----------------------------------------------------------------------*     
        Function          Lblnk(STRING) 
        Character*(*)     STRING

        L=Len(STRING)
        Lblnk=0

        Do I1=L,1,-1
           If(STRING(I1:I1).NE.' ') then
              Lblnk=I1
              Exit
           End if
        End do 

        Return
        End
