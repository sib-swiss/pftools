*       Version:  This file is part of pftools release 2.2 June 1998
*----------------------------------------------------------------------*     
        Subroutine RECMD(RCEX)

        Character*(*)     RCEX

        Character*64      CARG
        Character*256     CMDL

        LCMD=LEN(RCEX)

* Concatenate arguments of command-line

        N=Iargc()

        IC=1
        Do I1=0,N
           Call GetArg(I1,CARG)
           CMDL(IC:)=CARG
           IC=Lblnk(CMDL)+2
        End do

        RCEX=CMDL
        If(IC.GT.LCMD) RCEX(LCMD-1:LCMD)='..'

        Return
        End 
