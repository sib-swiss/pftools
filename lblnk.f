*----------------------------------------------------------------------*     
* $Id: lblnk.f,v 2.5 2003/11/18 10:19:38 vflegel Exp $
*----------------------------------------------------------------------*     
*       Version:  File under developpment for release 2.3
*----------------------------------------------------------------------*     
      Integer Function  Lblnk(STRING) 
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
