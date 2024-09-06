*       Program GtoP 
*----------------------------------------------------------------------*     
*       Function: Reformats profiles: in-fmt=GRIBSKOV / out-fmt=PROSITE    
*       Author:   Philipp Bucher
*       Syntax:   GtoP > outfile (input via via interactive dialog) 
*----------------------------------------------------------------------*     
*
* DATA
*----------------------------------------------------------------------*     

* array dimensions and I/O units

        Parameter        (IDMP=1023)

        Parameter        (NOUT=   6)    
        Parameter        (NINP=  11)    

* profile and sequence fields :

        Character*64      FPRF

        Include          'psdat.f'
        Include          'gsdat.f'
        Include          'djdat.f'
        Include          'nodat.f'
        Include          'codat.f'
        Include          'pfdat.f'
        Include          'dfdat.f'

*----------------------------------------------------------------------*     
* INPUT SECTION 
*----------------------------------------------------------------------*     

        IRC=0
        FPRF='stdin'

* read profile

        Call RGPRF
     *    (NINP,FPRF,
     *     CPID,CPAC,CPDE,NABC,CABC,LPRF,LPCI,
     *     MDIS,NDIP,
     *     JNOR,MNOR,NNOR,NNPR,CNTX,RNOP, 
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT, 
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

        If(IRC.NE.0) go to 100

        Call WRPRF
     *    (NOUT,FPRF,
     *     CPID,CPAC,CPDE,NABC,CABC,LPRF,LPCI,
     *     MDIS,NDIP,
     *     JNOR,MNOR,NNOR,NNPR,CNTX,RNOP, 
     *     JCUT,MCLE,CCUT,ICUT,JCNM,RCUT,MCUT, 
     *     IDMP,CHIP,IIPP,CHMP,IMPP,
     *     CHID,IIPD,CHMD,IMPD,
     *     IRC)

  100   Stop
        End
*----------------------------------------------------------------------*     
        Include          'rgprf.f'
        Include          'wrprf.f'
