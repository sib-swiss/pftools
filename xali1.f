*       Version:  This file is part of pftools release 1.0 January 1996
*----------------------------------------------------------------------*     
        Subroutine XALI1
     *    (NABC,CABC,LPRF,LPCI,
     *     KCUT,IDMP,IIPP,IMPP,CHIP,CHMP,IIPX,
     *     IDMS,LSEQ,ISEQ,
     *     IOPM,IOPI,IOPD,
     *     IOPT,LOPT,  
     *     IRC)

* profile and sequence fields :

        Include           'gsdat.f'
        Include           'pfdat.f'
        Include           'pxdat.f'

* sequence

        Integer           LSEQ
        Integer*2         ISEQ(IDMS)

* work fields

        Integer           IOPM(0:IDMP)
        Integer           IOPI(0:IDMP)
        Integer           IOPD(0:IDMP)

        Integer           KOPM

        Logical           LOPT

        IRC=0

        IOPT=NLOW

* beginning of sequence 

              IOPM(0)=IIPX(YM, 0)
              IOPI(0)=IIPX(YI, 0)
              IOPD(0)=IIPX(YD, 0)

           Do   9, I2=1,LPRF

              KD=IOPD(I2-1)+IMPP( D,I2)

              IOPM(I2)=MAX(KD+IIPP(DM,I2), 
     *                        IIPX(YM,I2))  
              IOPI(I2)=MAX(KD+IIPP(DI,I2), 
     *                        IIPX(YI,I2))  
              IOPD(I2)=MAX(KD+IIPP(DD,I2), 
     *                        IIPX(YD,I2))  
    9      Continue

* internal sequence positions

        Do  50 I1=1,LSEQ-1

              J1=ISEQ(I1)

              KI=IOPI( 0)+IIPP(J1, 0)

              KOPM=IOPM( 0)

              IOPM( 0)=MAX(KI+IIPP(IM, 0), 
     *                        IIPX(XM, 0))  
              IOPI( 0)=MAX(KI+IIPP(II, 0), 
     *                        IIPX(XI, 0))  
              IOPD( 0)=MAX(KI+IIPP(ID, 0), 
     *                        IIPX(XD, 0))  
              IOPT    =MAX(IOPT,
     *                     KI+IIPX(IX, 0)) 

           Do  49 I2=1,LPRF

              KM=KOPM      +IMPP(J1,I2)
              KI=IOPI(I2  )+IIPP(J1,I2)
              KD=IOPD(I2-1)+IMPP( D,I2)

              KOPM=IOPM(I2)

              IOPM(I2)=MAX(KM+IIPP(MM,I2), 
     *                     KI+IIPP(IM,I2), 
     *                     KD+IIPP(DM,I2), 
     *                        IIPX(XM,I2))  
              IOPI(I2)=MAX(KM+IIPP(MI,I2), 
     *                     KI+IIPP(II,I2), 
     *                     KD+IIPP(DI,I2), 
     *                        IIPX(XI,I2))  
              IOPD(I2)=MAX(KM+IIPP(MD,I2), 
     *                     KI+IIPP(ID,I2), 
     *                     KD+IIPP(DD,I2), 
     *                        IIPX(XD,I2))  

              IOPT    =MAX(IOPT,
     *                     KM+IIPX(MX,I2),
     *                     KI+IIPX(IX,I2),
     *                     KD+IIPX(DX,I2))

   49      Continue

           If(.NOT.LOPT.AND.IOPT.GE.KCUT) go to 100

   50   Continue

* end of sequence 

              J1=ISEQ(LSEQ)

              KI=IOPI( 0)+IIPP(J1, 0)

              KOPM=IOPM( 0)

              IOPD( 0)=MAX(KI+IIPP(ID, 0), 
     *                        IIPX(XD, 0))  
              IOPT    =MAX(IOPT,
     *                     KI+IIPX(IY, 0)) 

           Do  59 I2=1,LPRF

              KM=KOPM      +IMPP(J1,I2)
              KI=IOPI(I2  )+IIPP(J1,I2)
              KD=IOPD(I2-1)+IMPP( D,I2)

              KOPM=IOPM(I2)

              IOPD(I2)=MAX(KM+IIPP(MD,I2), 
     *                     KI+IIPP(ID,I2), 
     *                     KD+IIPP(DD,I2), 
     *                        IIPX(XD,I2))  

              IOPT    =MAX(IOPT,
     *                     KM+IIPX(MY,I2),
     *                     KI+IIPX(IY,I2),
     *                     KD+IIPX(DY,I2))

   59      Continue

  100   Return

        End 
