*----------------------------------------------------------------------*     
* $Id: lblnk.f,v 2.4 2003/04/10 11:58:37 vflegel Exp $
*----------------------------------------------------------------------*     
*       Version:  File under developpment for release 2.3
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
