# M A K E F I L E   F O R   P F T O O L S  R E L E A S E  2.3
#----------------------------------------------------------------------#
# $Id: Makefile,v 2.16 2003/11/28 11:54:37 vflegel Exp $
#----------------------------------------------------------------------#

PROGS = gtop pfmake pfscan pfw ptoh htop \
        pfscale pfsearch psa2msa 2ft 6ft ptof
MANS =  gtop.1 pfmake.1 pfscan.1 pfw.1 ptoh.1 htop.1 pfscale.1 \
        pfsearch.1 psa2msa.1 2ft.1 6ft.1 ptof.1 psa.5 xpsa.5

F77   = g77
CC    = gcc
FFLAGS= -O2 -init-local-zero -fno-automatic io.o
#PRFLAG= -g -pg
#----------------------------------------------------------------------#
# Location of package installation directory
#
PKGDIR = /usr/share/pftools23
#----------------------------------------------------------------------#
#
# Compilation with native fortran compilers (f77)
#
#F77  = f77
#CC   = cc
#
# sunos / solaris
#FFLAGS= -cg89 -O4
#
# HP-UX 
#FFLAGS= +O4 +E4 +E6 +U77 -K
#
# IRIX 
#FFLAGS= -O2 -static
#
# AIX 3.2.5
#FFLAGS= -qcharlen=1024  -O3
#
# Tru64 UNIX V5.1A
#FFLAGS= -noautomatic -fast -g0
#----------------------------------------------------------------------#

all  :  io.o $(PROGS)

install: $(PROGS)
	./install_pftools.sh $(PKGDIR)

clean:
	rm $(PROGS) io.o

io.o :  io.c 
	$(CC) -c io.c -o io.o $(PRFLAG)

gtop :  gtop.f io.o psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f \
        sterr.f cvini.f lblnk.f regpr.f wrprf.f sterr.f ardim.f
	$(F77) $(FFLAGS) gtop.f -o gtop $(PRFLAG)

htop :  htop.f codat.f cvini.f dfdat.f djdat.f gsdat.f lblnk.f nodat.f \
        pfdat.f pfind.f psdat.f rhmmer.f rhmmer2.f rhnul.f sterr.f wrprf.f recmd.f ardim.f
	$(F77) $(FFLAGS) htop.f -o htop $(PRFLAG)

pfsearch : pfsearch.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f pxdat.f \
        avdat.f sterr.f cvini.f reprf.f reseq.f rfseq.f xali1.f xalip.f \
        RtoN.f NtoR.f CFAve.f CPAve.f wprsm.f xprsm.f xalit.f lblnk.f prali.f sterr.f \
        pmali.f ardim.f prsp.f prxp.f Xblnk.f
	$(F77) $(FFLAGS) pfsearch.f -o pfsearch $(PRFLAG)

pfscan : pfscan.f psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f \
        pxdat.f avdat.f sterr.f cvini.f abini.f reprf.f reseq.f rfseq.f \
        xali1.f xalip.f RtoN.f NtoR.f CFAve.f CPAve.f wprsm.f  xprsm.f xalit.f lblnk.f \
        prali.f ardim.f prsp.f prxp.f Xblnk.f
	$(F77) $(FFLAGS) pfscan.f -o pfscan $(PRFLAG)

psa2msa : psa2msa.f ardim.f io.o sterr.f lblnk.f   
	$(F77) $(FFLAGS) psa2msa.f -o psa2msa $(PRFLAG)

pfmake : pfmake.f psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f \
        sterr.f cvini.f remsf.f wrprf.f lblnk.f remsf.f wrprf.f lblnk.f recmd.f \
        remsa.f Xblnk.f
	$(F77) $(FFLAGS) pfmake.f -o pfmake  $(PRFLAG)

ptoh :  ptoh.f psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f sterr.f \
        hmdat.f cvini.f reprf.f rhnul.f wrsam.f wrhmr.f schmm.f dfnul.f lblnk.f \
        ardim.f Xblnk.f
	$(F77) $(FFLAGS) ptoh.f -o ptoh $(PRFLAG)

pfw :   pfw.f sterr.f remsf.f lblnk.f remsa.f
	$(F77) $(FFLAGS) pfw.f -o pfw  $(PRFLAG)

pfscale : pfscale.f sterr.f recmd.f ardim.f psdat.f gsdat.f djdat.f nodat.f codat.f \
          pfdat.f dfdat.f sterr.f cvini.f reprf.f wrprf.f recmd.f NtoR.f lblnk.f ardim.f \
          Xblnk.f
	$(F77) $(FFLAGS) pfscale.f -o pfscale 

2ft :   2ft.f lblnk.f ardim.f sterr.f
	$(F77) $(FFLAGS) 2ft.f -o 2ft  $(PRFLAG)

6ft :   6ft.f lblnk.f ardim.f sterr.f
	$(F77) $(FFLAGS) 6ft.f -o 6ft $(PRFLAG)

ptof :  ptof.f codat.f cvini.f dfdat.f djdat.f gsdat.f lblnk.f nodat.f pfdat.f \
        psdat.f reprf.f sterr.f wrprf.f recmd.f ardim.f Xblnk.f
	$(F77) $(FFLAGS) ptof.f -o ptof $(PRFLAG)
