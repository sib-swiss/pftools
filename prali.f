*       Version:  This file is part of pftools release 1.2 April 1997
*----------------------------------------------------------------------*     
        Subroutine PRALI
     *    (LPRF,CHIP,CHMP,IDMP,LSEQ,LREV,
     *     CALI,LALI,JALB,JALE)

        Character*01      CHIP(0:IDMP)
        Character*01      CHMP(IDMP)
        Character*01      CALI(*)
        Logical           LREV

        Character*80      RCPR
        Character*80      RCSQ
        

* Find beginning of alignment, sequence. profile                

           JB=0
           KB=1 
        Do I1=1,LALI
           If(CALI(I1).EQ.'-') then 
              JB=JB+1
              KB=KB+1
           Else 
              Go to   2
           End if 
        End do 
    2   LB=JALB 
           

* Find beginning of alignment, sequence, profile                

           JE=LPRF
           KE=LALI 
        Do I1=LALI,KB+1,-1
           If(CALI(I1).EQ.'-') then 
              JE=JE-1
              KE=KE-1
           Else 
              Go to   3
           End if 
        End do 
    3   Continue
   
* Write alignment  

              IX=0

        Do I1=KB,KE

           If(IX.GE.60) then 
                 NPE=JB
              If(.NOT.LREV) then 
                 NSE=LB
                 If(RCSQ(1:1).NE.'-') NSE=NSE-1
              Else
                 NSE=LSEQ-LB+1 
                 If(RCSQ(1:1).NE.'-') NSE=NSE+1
              End if
                 NPE=NPE-LPRF-1
                 NSE=NSE-LSEQ-1
              Write(6,'(''#'')')    
              Write(6,'(''# P'',I8,'' '',60A,I8)')
     *           NPB,(RCPR(ii1:ii1),ii1=1,60),NPE
              Write(6,'(''# S'',I8,'' '',60A,I8)')
     *           NSB,(RCSQ(ii1:ii1),ii1=1,60),NSE
              IX=0
           End if 

              IX=IX+1
           K1=Ichar(CALI(I1))
           If     (K1.GE.65.AND.K1.LE. 90) then 
              JB=JB+1
              LB=LB+1
              RCPR(IX:IX)=CHMP(JB)
              RCSQ(IX:IX)=CALI(I1)
           Else if(K1.GE.97.AND.K1.LE.122) then 
              LB=LB+1
              RCPR(IX:IX)=CHIP(JB)
              RCSQ(IX:IX)=CALI(I1)
           Else if(CALI(I1).EQ.'-') then 
              JB=JB+1
              RCPR(IX:IX)=CHMP(JB)
              RCSQ(IX:IX)=CALI(I1)
           End if 
              
           If(IX.EQ.1) then
                 NPB=JB
              If(.NOT.LREV) then 
                 NSB=LB
                 If(RCSQ(1:1).NE.'-') NSB=NSB-1
              Else
                 NSB=LSEQ-LB+1 
                 If(RCSQ(1:1).NE.'-') NSB=NSB+1
              End if
           End if
        
        End do  

           If(IX.GT.0) then 
                 NPE=JB
              If(.NOT.LREV) then 
                 NSE=LB
                 If(RCSQ(1:1).NE.'-') NSE=NSE-1
              Else
                 NSE=LSEQ-LB+1 
                 If(RCSQ(1:1).NE.'-') NSE=NSE+1
              End if
                 NPE=NPE-LPRF-1
                 NSE=NSE-LSEQ-1
              Write(6,'(''#'')')    
              RCPR(IX+1:)=' '
              RCSQ(IX+1:)=' '
              Write(RCPR(IX+1:IX+9),'(I8)') NPE
              Write(RCSQ(IX+1:IX+9),'(I8)') NSE
              IX=IX+9
              Write(6,'(''# P'',I8,'' '',80A)')
     *           NPB,(RCPR(ii1:ii1),ii1=1,IX)
              Write(6,'(''# S'',I8,'' '',80A)')
     *           NSB,(RCSQ(ii1:ii1),ii1=1,IX)
           End if
              Write(6,'(''#'')')    

       Return 
       End
