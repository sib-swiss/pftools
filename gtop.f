        Program gtop 
*----------------------------------------------------------------------*     
*       Function: Reformats profiles: in-fmt=GRIBSKOV / out-fmt=PROSITE    
*       Author:   Philipp Bucher
*       Version:  This file is part of pftools release 1.2 April 1997
*----------------------------------------------------------------------*     
*
* DATA
*----------------------------------------------------------------------*     

* array dimensions and I/O units

        Parameter        (IDMP=8191)

        Parameter        (NOUT=   6)    
        Parameter        (NGPR=  11)    

* profile and sequence fields

        Character*64      FGPR

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'pfdat.f'
        Include          'dfdat.f'
        Include          'sterr.f'

* command line options and parameters

        Logical           LSYM

* initialization of controlled vocabularies

        Include          'cvini.f' 

*----------------------------------------------------------------------*     
* INPUT SECTION 
*----------------------------------------------------------------------*     

        IRC=0
        FGPR='-'
* read command line

           RL=NLOW
        Call Repar
     *    (FGPR,LSYM,RG,RE,RF,RO,IRC)
        If(IRC.NE.0) then
           Write(NERR,'(
     *      ''Usage: gtop [ -as ] [ gcg-profile ] '' 
     *      ''[ parameters ]'',//,
     *      ''   valid parameters are:'',//,
     *      ''                 [G=gap-weigth]             '',/,
     *      ''                 [E=gap-extension-weight]   '',/,
     *      ''                 [F=output-score-multiplier]'',/
     *      ''                 [O=output-score-offset]    '',/
     *        )')
           Stop
        End if

* read profile

        Call REGPR
     *    (NGPR,FGPR,
     *     RG,RE,RF,RO,LSYM,
     *     CPID,CPAC,CPDE,NABC,CABC,LPRF,LPCI,
     *     CDIS,JDIP,MDIS,NDIP,
     *     CNOR,JNOP,JNOR,MNOR,NNOR,NNPR,CNTX,RNOP,
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT, 
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

        If(IRC.NE.0) go to 100

        Call WRPRF
     *    (NOUT,
     *     CPID,CPAC,CPDE,NABC,CABC,LPRF,LPCI,
     *     CDIS,JDIP,MDIS,NDIP,
     *     CNOR,JNOP,JNOR,MNOR,NNOR,NNPR,CNTX,RNOP,
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT, 
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

  100   Stop
        End
*----------------------------------------------------------------------*     
        Subroutine Repar         
     *    (FGPR,LSYM,RG,RE,RF,RO,IRC)

        Character*64      CPAR
        Character*64      FGPR 
 
        Logical           LSYM
 
        LSYM=.TRUE.
        RG=4.5
        RE=0.05
        RF=100
        RO=0
 
        FGPR=' '

        N1=Iargc()
 
           K1=0
        Do  50 I1=1,N1
 
           Call GetArg(I1,CPAR)
 
           If     (CPAR(1:1).EQ.'-'.AND.K1.EQ.0) then
              Do  I2=2,Lblnk(CPAR)
                 If     (CPAR(I2:I2).EQ.'a') then
                    LSYM=.FALSE.
                 End if
              End do
           Else if(K1.GE.1) then 
              If     (CPAR(1:2).EQ.'G=') then
                 Read(CPAR(3:64),*,Err=900) RG
              Else if(CPAR(1:2).EQ.'E=') then
                 Read(CPAR(3:64),*,Err=900) RE
              Else if(CPAR(1:2).EQ.'F=') then
                 Read(CPAR(3:64),*,Err=900) RF
              Else if(CPAR(1:2).EQ.'O=') then
                 Read(CPAR(3:64),*,Err=900) RO
              End if 
           Else
              FGPR=CPAR
              K1=K1+1
           End if 

   50   Continue

        If(K1.NE.1) Go to 900 
  100   Return
  900   IRC=1
        Go to 100
        End 
*----------------------------------------------------------------------*     
        Include          'lblnk.f'
        Include          'regpr.f'
        Include          'wrprf.f'
