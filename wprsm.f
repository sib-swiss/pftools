*       Version:  This file is part of pftools release 2.2 June 1999
*----------------------------------------------------------------------*     
        Subroutine WPRSM(JSEQ,
     *     LUNI,LNOR,LREV,LPFA,OPTZ,OPTL,OPLU,NW,
     *     CHID,CHAC,CHDE,
     *     IOPT,JALB,JALE,NALI,IPMB,IPME,
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT,
     *     RNOP,KNPM,MAXN,INOR,IFUN,LSEQ,RAVE)

* profile parameters

        Include          'codat.f'

        Real              RNOP(KNPM,MAXN)

* logical switches 

        Logical           LUNI
        Logical           LNOR
        Logical           LREV
        Logical           LPFA
        Logical           OPTL
        Logical           OPLU
        Logical           OPTZ 

* sequence header 
 
        Character*(*)     CHID
        Character*(*)     CHAC
        Character*(*)     CHDE

* Output fields 

        Character*04      CHLE
        Character*08      CHNS
        Character*07      CHRS 
        Character*20      CHLO 
        Character*18      CHPP
        Character*64      CHER 
        Character*64      CHMI

        Character*1024    RCEX

* prepare output fields 

* - norm-score

        If(LNOR) then
           Call RtoN
     *        (IOPT,XOPT,RNOP,KNPM,MAXN,INOR,IFUN,LSEQ,RAVE)
           R=XOPT
           If(R.GT.9999.999) R=9999.999 
           If(R.LT.-999.999) R=-999.999
           Write(CHNS,'(F7.3)') R
        End if 

* - cut-off level

        If(OPTL.OR.OPLU) then
           If(LNOR) then
              K=MCLE(1)
              Do I1=2,JCUT
                 K=MIN(K,MCLE(I1))
              End do
              K=MAX(-9,K-1)
              Do I1=1,JCUT
                 Do I2=1,JCNM(I1)
                    If(MCUT(I2,I1).EQ.INOR) then 
                       If(XOPT.GE.RCUT(I2,I1)) K=MAX(K,MCLE(I1))
                    End if
                 End do
              End do
           Else
              Do I1=1,JCUT
                 If(IOPT.GE.ICUT(I1)) K=MAX(K,MCLE(I1))
              End do 
           End if 
           If     (OPLU) then
              Do I1=1,JCUT
                 J=I1 
                 If(K.EQ.MCLE(I1)) Exit
              End do 
              CHLE=CCUT(J)
           Else if(K.LT.0.OR.K.GT.9) then 
              Write(CHLE,'(''L='',I2)') K 
           Else
              Write(CHLE,'(''L='',I1,'' '')') K 
           End if
        End if  

* - raw-score

        I=IOPT
        If(I.GT.999999) I=999999
        If(I.LE.-99999) I=-99999
        Write(CHRS,'(I7)') I
        
* - location in sequence

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

* - location in profile 

           If(OPTZ) then 
              Write(CHPP,'('' ['',I5,'','',I6,'']'')') IPMB,IPME
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

        If(LPFA.AND.NALI.GT.1) then 
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
        Else
           CHMI=CHID(1:L2)
           LNMI=L2
        End if

* assemble ouput record

        LNEX=0 

        If(LPFA) then  
           RCEX(LNEX+1:)='>' // CHMI // ' ' 
           LNEX=LNEX+LNMI+2
        End if 

        If(OPTL.OR.OPLU) then
           RCEX(LNEX+1:)=CHLE
           LNEX=LNEX+4
           If(OPLU) LNEX=LNEX-2
        End if

        If(LNOR) then 
           RCEX(LNEX+1:)=CHNS
           LNEX=LNEX+7
        End if

        RCEX(LNEX+1:)=CHRS
        LNEX=LNEX+7

        If(.NOT.LUNI) then 
           RCEX(LNEX+1:)=CHLO
           LNEX=LNEX+20

           If(OPTZ) then
              RCEX(LNEX+1:)=CHPP
              LNEX=LNEX+15
           End if
        End if

        RCEX(LNEX+2:)=CHER(1:LNER)
        LNEX=LNEX+1+LNER

        L3=MAX(0,MIN(NW-LNEX-1,Lblnk(CHDE)))
        RCEX(LNEX+2:)=CHDE(1:L3)
        LNEX=LNEX+1+L3

* Write output record
        Write(6,'(1024A)')(RCEX(ii1:ii1),ii1=1,LNEX)

  100   Return 
        End 
