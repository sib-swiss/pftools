*       Program psa2msa
*----------------------------------------------------------------------*     
*       Function: Converts a pfsearch -x output file into Pearson/Fasta
*                 multiple sequence asignment format  
*       Author:   Philipp Bucher
*       Version:  This file is part of pftools release 1.1 March 1996
*----------------------------------------------------------------------*     
* DATA
*----------------------------------------------------------------------*     

        Parameter        (NSEQ=  11)

        Character*64      FSEQ
        Character*01      CSEQ(262144) 
        Integer           IPRF(0:8191)

        Character*512     RCIO
        Character*01      B

        Logical           OPTL
        Logical           OPTU
        Logical           OPTP
        Logical           OPTD

        Integer           Getc
        Integer           Fputc

        Include          'sterr.f' 

*----------------------------------------------------------------------*     
* INPUT SECTION  
*----------------------------------------------------------------------*     

        RC=0
        LPRF=0
        RCIO=' '

* command line arguments
 
        Call Repar
     *    (FSEQ,OPTL,OPTU,OPTP,OPTD,IRC)
        If(IRC.NE.0) then
           Write(NERR,'(
     *      ''Usage: psa2msa [ -ulpd ] [ [ psa-file | - ] ]''
     *        )')
           Stop
        End if

* open input file

        If(FSEQ.EQ.'-') then
    1      Open(NSEQ,Status='SCRATCH')
           Do I1=1,1000000000 
              If(Getc(B).NE.0) go to 2 
              J1=Fputc(NSEQ,B)
           End do
    2      Rewind(NSEQ)
        Else 
           Open(NSEQ,File=FSEQ,Status='OLD',Err=900)
        End if
 
* first sequence:

        Call GetSeq(NSEQ,CSEQ,LSEQ,RCIO,LDES,IRC)
        If(IRC.NE.0) go to 100

        Call UpdIPRF(CSEQ,LSEQ,LPRF,IPRF,IRC)

* next sequence

   10   Call GetSeq(NSEQ,CSEQ,LSEQ,RCIO,LDES,IRC)
        If     (IRC.EQ.-1) then
           Go to  50
        Else if(IRC.NE. 0) then 
           Go to 100
        End if 

        Call UpdIPRF(CSEQ,LSEQ,LPRF,IPRF,IRC)

        Go to  10

   50   Continue     

        Rewind(NSEQ)

        LMSA=LPRF 
        Do I1=0,LPRF
           LMSA=LMSA+IPRF(I1)
        End do 
        
   60   Call GetSeq(NSEQ,CSEQ,LSEQ,RCIO,LDES,IRC)
        If(IRC.NE.0) go to 100

        Call UpdSeq(CSEQ,LSEQ,LMSA,IPRF,LPRF)
        Call EdtSeq(CSEQ,LMSA,OPTL,OPTU,OPTP,OPTD)

        Write(6,'((512A))')(RCIO(ii1:ii1),ii1=1,LDES)
        Write(6,'(( 60A))')(CSEQ(ii1),ii1=1,LMSA)

        Go to  60
 
  100   Stop 
  900   go to 100
        End
*----------------------------------------------------------------------*
        Subroutine Repar
     *    (FSEQ,OPTL,OPTU,OPTP,OPTD,IRC)

        Character*64      FSEQ
        Character*64      CARG 

        Logical           OPTL
        Logical           OPTU
        Logical           OPTP
        Logical           OPTD

        IRC=0

        OPTL=.FALSE.
        OPTU=.FALSE.
        OPTP=.FALSE.
        OPTD=.FALSE.

        N1=Iargc()
        K1=0
        Do  I1=1,N1
           Call GetArg(I1,CARG)
           If(CARG(1:1).EQ.'-') then 
              If(Index(CARG,'l').NE.0) OPTL=.TRUE.
              If(Index(CARG,'u').NE.0) OPTU=.TRUE.
              If(Index(CARG,'p').NE.0) OPTP=.TRUE.
              If(Index(CARG,'d').NE.0) OPTD=.TRUE.
           Else
              FSEQ=CARG 
              K1=K1+1
           End if
        End do 

        If(K1.LT.1) then
           FSEQ='-'
        Else if(K1.GT.1) then
           Go to 900
        End if 
 
  100   Return
  900   IRC=-1  
        End
*----------------------------------------------------------------------*
        Subroutine GetSeq(NSEQ,CSEQ,LSEQ,RCIO,LDES,IRC)

        Character*01      CSEQ(*)
        Character*(*)     RCIO

        Character*512     RCIN

        IRC=0
        LSEQ=0
        LDES=0

    1   Continue 
        If(RCIN(1:1).NE.'>') then 
           Read(NSEQ,'(A)',Err=900,End=900) RCIN
           L1=Lblnk(RCIN)
           Go to   1
        End if

        LDES=L1
        RCIO(1:LDES)=RCIN(1:LDES)

    2   Read(NSEQ,'(A)',Err=900,End=900) RCIN
        L1=Lblnk(RCIN)
        If(RCIN(1:1).EQ.'>') then 
           Go to 100
        Else
           Do I1=1,L1
              K1=Ichar(RCIN(I1:I1))
              If((K1.GE.65.AND.K1.LE. 90).OR.
     *           (K1.GE.97.AND.K1.LE.122).OR.
     *           (RCIN(I1:I1).EQ.'-').OR.
     *           (RCIN(I1:I1).EQ.'.')) then
                 LSEQ=LSEQ+1
                 CSEQ(LSEQ)=RCIN(I1:I1)
              End if
           End do
        End if 
        Go to   2

  100   Return
  900   IRC=-1
        Go to 100
        End
*----------------------------------------------------------------------*
        Subroutine UpdIPRF(CSEQ,LSEQ,LPRF,IPRF,IRC)

        Character*01      CSEQ(*)
        Integer           IPRF(0:8191)

        IRC=0

        If(LPRF.EQ.0) then
           Do I1=1,LSEQ
              K1=Ichar(CSEQ(I1))
              If((K1.GE.65.AND.K1.LE. 90).OR.
     *           (CSEQ(I1).EQ.'-')) then
                 LPRF=LPRF+1
              End if 
           End do 
           Do I1=0,LPRF
              IPRF(I1)=0
           End do
        End if 

        J1=0
        M1=0
        Do I1=1,LSEQ
           K1=Ichar(CSEQ(I1))
           If((K1.GE.65.AND.K1.LE. 90).OR.
     *        (CSEQ(I1).EQ.'-')) then
              IPRF(J1)=MAX(IPRF(J1),M1)
              J1=J1+1
              M1=0
           Else
              M1=M1+1  
           End if
        End do 
              IPRF(J1)=MAX(IPRF(J1),M1)
        
        If(J1.NE.LPRF) go to 900

  100   Return
  900   IRC=1
        Go to 100
        End        
*----------------------------------------------------------------------*
        Subroutine UpdSeq(CSEQ,LSEQ,LMSA,IPRF,LPRF)

        Character*01      CSEQ(*)
        Integer           IPRF(0:8191)

        J1=LPRF
        M1=LMSA
        N1=0
        Do I1=LSEQ,1,-1
           K1=Ichar(CSEQ(I1))
           If((K1.GE.65.AND.K1.LE. 90).OR.
     *        (CSEQ(I1).EQ.'-')) then
              Do I2=N1+1,IPRF(J1)
                 CSEQ(M1)='.'
                 M1=M1-1
              End do
                 CSEQ(M1)=CSEQ(I1)
                 M1=M1-1
                 N1=0
                 J1=J1-1
           Else 
                 CSEQ(M1)=CSEQ(I1)
                 M1=M1-1
                 N1=N1+1
           End if
        End do 
  100   Return
        End
*----------------------------------------------------------------------*
        Subroutine EdtSeq(CSEQ,LMSA,OPTL,OPTU,OPTP,OPTD)

        Character*01      CSEQ(*)

        Logical           OPTU
        Logical           OPTL
        Logical           OPTP
        Logical           OPTD

        If(OPTU) then
           Do  I1=1,LMSA
              K1=Ichar(CSEQ(I1))
              If(K1.LE.122.AND.K1.GE.97) CSEQ(I1)=Char(K1-32) 
            End do 
        End if 

        If(OPTL) then
           Do  I1=1,LMSA
              K1=Ichar(CSEQ(I1))
              If(K1.LE.90.AND.K1.GE.65) CSEQ(I1)=Char(K1+32) 
            End do 
        End if 

        If(OPTP) then
           Do  I1=1,LMSA
              If(CSEQ(I1).EQ.'-') CSEQ(I1)='.'
            End do 
        End if 

        If(OPTD) then
           Do  I1=1,LMSA
              If(CSEQ(I1).EQ.'.') CSEQ(I1)='-'
            End do 
        End if 

        Return
        End
*----------------------------------------------------------------------*
        Include          'lblnk.f'
