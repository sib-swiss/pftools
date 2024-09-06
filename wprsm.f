*       Version:  This file is part of pftools release 1.0 January 1996
*----------------------------------------------------------------------*     
        Subroutine WPRSM(JSEQ,
     *     LUNI,LNOR,LREV,LPFA,
     *     CHID,CHAC,CHDE,
     *     IOPT,JALB,JALE,NALI,
     *     RNOP,KNPM,MAXN,INOR,IFUN,LSEQ,RAVE)

* profile paramters

        Real              RNOP(KNPM,MAXN)

* logical switches 

        Logical           LUNI
        Logical           LNOR
        Logical           LREV
        Logical           LPFA

* sequence header 
 
        Character*(*)     CHID
        Character*(*)     CHAC
        Character*(*)     CHDE

* Output fields 

        Character*08      CHNS
        Character*06      CHRS 
        Character*20      CHLO 
        Character*64      CHER 
        Character*64      CHMI

* work fields 

        Character*132     RCEX

* prepare output fields 

* - norm-score

        If(LNOR) then
           Call RtoN
     *        (IOPT,XOPT,RNOP,KNPM,MAXN,INOR,IFUN,LSEQ,RAVE)
           Write(CHNS,'(F8.4)') XOPT
        End if 

* - raw-score

        Write(CHRS,'(I6)') IOPT
        
* - location

        If(.NOT.LUNI) then 
           If(LREV) then 
              JALB=LSEQ-JALB+1
              JALE=LSEQ-JALE+1
           End if 
           Write(CHLO,'('' pos. '',I6,'' -'',I6)') JALB,JALE
           If(LREV) then 
              JALB=LSEQ-JALB+1
              JALE=LSEQ-JALE+1
           End if 
        End if

* - entry-ref 

        L1=Lblnk(CHAC)
        L2=Lblnk(CHID)
        CHER=CHAC(1:L1) // CHID(1:L2)
        LNER=MIN(61,L1+L2)

        If(LUNI.AND.LREV) then 
           CHER=CHER(1:LNER) // '(-)' 
           LNER=LNER+3
        End if 

* - match-id

        If(LPFA) then 
           CHMI=CHID(1:L2) // '_'
           Write(CHMI(L2+2:),'(I6)') JSEQ
              J1=L2+1
           Do I1=L2+2,L2+7 
              If(CHMI(I1:I1).NE.' ') then 
                 J1=J1+1
                 CHMI(J1:J1)=CHMI(I1:I1)
              End if
           End do
           LNMI=J1
        End if

* assemble ouput record

        LNEX=0 

        If(LPFA) then  
           RCEX(LNEX+1:)='>' // CHMI 
           LNEX=LNEX+LNMI+1
        End if 

        If(LNOR) then 
           RCEX(LNEX+1:)=CHNS
           LNEX=LNEX+8
        End if

        RCEX(LNEX+1:)=CHRS
        LNEX=LNEX+6

        If(.NOT.LUNI) then 
           RCEX(LNEX+1:)=CHLO
           LNEX=LNEX+20
        End if 

        RCEX(LNEX+2:)=CHER(1:LNER)
        LNEX=LNEX+1+LNER

        L3=MIN(132-LNEX-1,Lblnk(CHDE))
        RCEX(LNEX+2:)=CHDE(1:L3)
        LNEX=LNEX+1+L3

* Write output record

        Write(6,'(132A)')(RCEX(ii1:ii1),ii1=1,LNEX)

  100   Return 
        End 
