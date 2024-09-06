*       Program 6ft
*----------------------------------------------------------------------*
*       Function: 6-frame translation of DNA sequence into protein
*       Author:   Philipp Bucher
*       Version:  This file is part of pftools release 2.2 June 1999
*----------------------------------------------------------------------*

        Parameter        (NOUT=6)

        Character*256     RCIN
        Character*256     CHDE 
        Character*64      CHID
        Character*256     RCEX

        Integer*2         ISEQ(1000000)
        Character         CSEQ(1000000)
    
        Character         CABC(0:63) 
        Character         CNBC(0:15) 

        Character*04      CDNA
 
        Include          'sterr.f'
 
* options
 
        Logical           OPTS
        Logical           OPTR

* Initializations

        Data              CABC( 0)/'K'/
        Data              CABC( 1)/'N'/
        Data              CABC( 2)/'K'/
        Data              CABC( 3)/'N'/
        Data              CABC( 4)/'T'/
        Data              CABC( 5)/'T'/
        Data              CABC( 6)/'T'/
        Data              CABC( 7)/'T'/
        Data              CABC( 8)/'R'/
        Data              CABC( 9)/'S'/
        Data              CABC(10)/'R'/
        Data              CABC(11)/'S'/
        Data              CABC(12)/'I'/
        Data              CABC(13)/'I'/
        Data              CABC(14)/'M'/
        Data              CABC(15)/'I'/
        Data              CABC(16)/'Q'/
        Data              CABC(17)/'H'/
        Data              CABC(18)/'Q'/
        Data              CABC(19)/'H'/
        Data              CABC(20)/'P'/
        Data              CABC(21)/'P'/
        Data              CABC(22)/'P'/
        Data              CABC(23)/'P'/
        Data              CABC(24)/'R'/
        Data              CABC(25)/'R'/
        Data              CABC(26)/'R'/
        Data              CABC(27)/'R'/
        Data              CABC(28)/'L'/
        Data              CABC(29)/'L'/
        Data              CABC(30)/'L'/
        Data              CABC(31)/'L'/
        Data              CABC(32)/'E'/
        Data              CABC(33)/'D'/
        Data              CABC(34)/'E'/
        Data              CABC(35)/'D'/
        Data              CABC(36)/'A'/
        Data              CABC(37)/'A'/
        Data              CABC(38)/'A'/
        Data              CABC(39)/'A'/
        Data              CABC(40)/'G'/
        Data              CABC(41)/'G'/
        Data              CABC(42)/'G'/
        Data              CABC(43)/'G'/
        Data              CABC(44)/'V'/
        Data              CABC(45)/'V'/
        Data              CABC(46)/'V'/
        Data              CABC(47)/'V'/
        Data              CABC(48)/'O'/
        Data              CABC(49)/'Y'/
        Data              CABC(50)/'O'/
        Data              CABC(51)/'Y'/
        Data              CABC(52)/'S'/
        Data              CABC(53)/'S'/
        Data              CABC(54)/'S'/
        Data              CABC(55)/'S'/
        Data              CABC(56)/'O'/
        Data              CABC(57)/'C'/
        Data              CABC(58)/'W'/
        Data              CABC(59)/'C'/
        Data              CABC(60)/'L'/
        Data              CABC(61)/'F'/
        Data              CABC(62)/'L'/
        Data              CABC(63)/'F'/

        Data              CNBC( 0)/'X'/ 
        Data              CNBC( 1)/'T'/ 
        Data              CNBC( 2)/'X'/ 
        Data              CNBC( 3)/'X'/ 
        Data              CNBC( 4)/'X'/ 
        Data              CNBC( 5)/'P'/ 
        Data              CNBC( 6)/'R'/ 
        Data              CNBC( 7)/'L'/ 
        Data              CNBC( 8)/'X'/ 
        Data              CNBC( 9)/'A'/ 
        Data              CNBC(10)/'G'/ 
        Data              CNBC(11)/'V'/ 
        Data              CNBC(12)/'X'/ 
        Data              CNBC(13)/'S'/ 
        Data              CNBC(14)/'X'/ 
        Data              CNBC(15)/'X'/ 

        CDNA='ACGT'

        Call Repar(OPTS,OPTR,IRC)
        If(IRC.NE.0) then
           Write(NERR,'(
     *      ''Usage: 6ft [-[r|s]] < dna-sequence-file '',/
     *        )')
           Stop
        End if

    1   Read(5,'(A)',End=100) RCIN
        If(RCIN(1:1).NE.'>') go to   1

    2   If(RCIN(1:1).NE.'>') Go to 100 
        J1=Index(RCIN,' ')
        CHID='>x|' // RCIN(2:J1)
        LNID=Lblnk(CHID)
        LNDE=256-LNID+3
        CHDE=RCIN(J1+1:J1+LNDE)
        RCEX=CHID(1:LNID) // '   ' // CHDE(1:LNDE)  
        
        LSEQ=0
    3   Read(5,'(A)',End= 10) RCIN
        If(RCIN(1:1).EQ.'>') go to  10
        L1=Lblnk(RCIN) 
        
        Do   5 I1=1,L1
           K1=Ichar(RCIN(I1:I1))
           If(K1.GE.97) then 
              K1=K1-32
              RCIN(I1:I1)=char(K1)
           End if
           If(K1.GT.90.OR.K1.LT.65) go to   5
           LSEQ=LSEQ+1
           ISEQ(LSEQ)=Index(CDNA,(RCIN(I1:I1)))-1
           If(ISEQ(LSEQ).EQ.-1) ISEQ(LSEQ)=-64
    5   Continue
        Go to   3

   10   Continue 

* convert sequence into codons. 

        J1=LNID+1
        J2=LNID+2
        J3=Lblnk(RCEX)

* - plus strand
 
        If(.NOT.OPTR) then

        Do  12 I1=1,LSEQ-2
           N1=ISEQ(I1)*16+ISEQ(I1+1)*4+ISEQ(I1+2)
           If(N1.LT.0) then 
              N1=ISEQ(I1)*4+ISEQ(I1+1)
              If(N1.GE.0) then
                    CSEQ(I1)=CNBC(N1)
              Else 
                 CSEQ(I1)='X'
              End if
           Else
                 CSEQ(I1)=CABC(N1)
           End if 
   12   Continue

        RCEX(J1:J2)='_1'
        Write(6,'(256A)')(RCEX(ii1:ii1),ii1=1,J3)
        Write(6,'((60A))')(CSEQ(ii1),ii1=1,LSEQ-2,3)
        RCEX(J1:J2)='_2'
        Write(6,'(256A)')(RCEX(ii1:ii1),ii1=1,J3)
        Write(6,'((60A))')(CSEQ(ii1),ii1=2,LSEQ-2,3)
        RCEX(J1:J2)='_3'
        Write(6,'(256A)')(RCEX(ii1:ii1),ii1=1,J3)
        Write(6,'((60A))')(CSEQ(ii1),ii1=3,LSEQ-2,3)

        End if

* - minus strand
 
        If(.NOT.OPTS) then

        Do  13 I1=LSEQ,1,-1
           ISEQ(I1)=3-ISEQ(I1)
           If(ISEQ(I1).GT.4) ISEQ(I1)=-64
   13   Continue
           K1=1
        Do  14 I1=LSEQ,3,-1
           N1=ISEQ(I1)*16+ISEQ(I1-1)*4+ISEQ(I1-2)
           If(N1.LT.0) then 
              N1=ISEQ(I1)*4+ISEQ(I1-1)
              If(N1.GE.0) then 
                 CSEQ(K1)=CNBC(N1)
              Else 
                 CSEQ(K1)='X'
              End if
           Else
              CSEQ(K1)=CABC(N1)
           End if 
           K1=K1+1
   14   Continue

        RCEX(J1:J2)='_4'
        If(OPTR) RCEX(J1:J2)='_1'
        Write(6,'(256A)')(RCEX(ii1:ii1),ii1=1,J3)
        Write(6,'((60A))')(CSEQ(ii1),ii1=1,LSEQ-2,3)
        RCEX(J1:J2)='_5'
        If(OPTR) RCEX(J1:J2)='_2'
        Write(6,'(256A)')(RCEX(ii1:ii1),ii1=1,J3)
        Write(6,'((60A))')(CSEQ(ii1),ii1=2,LSEQ-2,3)
        RCEX(J1:J2)='_6'
        If(OPTR) RCEX(J1:J2)='_3'
        Write(6,'(256A)')(RCEX(ii1:ii1),ii1=1,J3)
        Write(6,'((60A))')(CSEQ(ii1),ii1=3,LSEQ-2,3)

        End if

        Go to   2

  100   Stop
        End
*----------------------------------------------------------------------*
        Subroutine Repar(OPTS,OPTR,IRC)

        Character*64      CARG

        Logical           OPTS
        Logical           OPTR
 
        IRC=0
 
        OPTS=.FALSE.
        OPTR=.FALSE.
 
        N1=Iargc()
 
           K1=0
        Do I1=1,N1
           Call GetArg(I1,CARG)
           If(CARG(1:1).EQ.'-') then
              If     (CARG(1:2).EQ.'-h') then
                 go to 900
              Else if(Index(CARG,'s').NE.0) then
                 OPTS=.TRUE.
              Else if(Index(CARG,'r').NE.0) then
                 OPTR=.TRUE.
              End if
           Else
              K1=K1+1
           End if
        End do
 
        If(K1.GT.0.OR.(OPTS.AND.OPTR)) go to 900
 
  100   Return
  900   IRC=-1
        Go to 100
        End
*----------------------------------------------------------------------*
        Include          'lblnk.f'
