*----------------------------------------------------------------------*     
* $Id: Xblnk.f,v 1.1 2003/07/03 13:29:16 vflegel Exp $
*----------------------------------------------------------------------*     
*       Version:  File under developpment for release 2.3
*----------------------------------------------------------------------*     
      Function          Xblnk(STRING,LIMIT) 
      Character*(*)     STRING
      Integer           LIMIT

      L=Len(STRING)
      Xblnk=0
      J1=0

      Do I1=1,L
         If(STRING(I1:I1).EQ.' ') then
            J1=J1+1
         Else
            J1=0
         End if
         Xblnk=I1-J1
         If(J1.GE.LIMIT) Exit
      End do 
      Return
      End
