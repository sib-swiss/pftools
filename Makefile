# M A K E F I L E   F O R   P F T O O L S  R E L E A S E  2.2
#----------------------------------------------------------------------#

PROGS = gtop pfmake pfscan pfw ptoh htop pfscale pfsearch psa2msa \
        2ft 6ft ptof
MANS =  gtop.1 pfmake.1 pfscan.1 pfw.1 ptoh.1 htop.1 pfscale.1 \
        pfsearch.1 psa2msa.1 2ft.1 6ft.1 ptof.1

F77   = g77
CC    = gcc
FFLAGS= -O -init-local-zero -fno-automatic io.o 
#----------------------------------------------------------------------#
# Location of binaries and manual pages. You may want to modify
# these definitions.
#
BINDIR = /usr/local/bin
MANDIR = /usr/local/man
#----------------------------------------------------------------------#
#
# Compilation with native fortran compilers (f77)
#
#F77  = f77
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
#----------------------------------------------------------------------#

all  :  $(PROGS)

install: $(PROGS)
	test -d $(BINDIR) || mkdir -p $(BINDIR)
	test -d $(MANDIR)/man1 || mkdir -p $(MANDIR)/man1
	cp -i $(PROGS) $(BINDIR)/
	cp -i $(MANS)  $(MANDIR)/man1

clean:  
	rm $(PROGS) io.o

io.o :  io.c 
	$(CC) -c io.c -o io.o

gtop :  gtop.f io.o psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f \
        sterr.f cvini.f lblnk.f regpr.f wrprf.f sterr.f  
	$(F77) $(FFLAGS) gtop.f -o gtop

htop :  htop.f codat.f cvini.f dfdat.f djdat.f gsdat.f lblnk.f nodat.f \
        pfdat.f pfind.f psdat.f rhmmer.f rhmmer2.f rhnul.f sterr.f wrprf.f recmd.f 
	$(F77) $(FFLAGS) htop.f -o htop

pfsearch : pfsearch.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f pxdat.f \
        avdat.f sterr.f cvini.f reprf.f reseq.f rfseq.f xali1.f xalip.f \
        RtoN.f NtoR.f CFAve.f CPAve.f wprsm.f xalit.f lblnk.f prali.f sterr.f  
	$(F77) $(FFLAGS) pfsearch.f -o pfsearch

pfscan : pfscan.f psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f \
        pxdat.f avdat.f sterr.f cvini.f abini.f reprf.f reseq.f rfseq.f \
        xali1.f xalip.f RtoN.f NtoR.f CFAve.f CPAve.f wprsm.f xalit.f lblnk.f \
        prali.f
	$(F77) $(FFLAGS) pfscan.f -o pfscan

psa2msa : psa2msa.f io.o sterr.f lblnk.f   
	$(F77) $(FFLAGS) psa2msa.f -o psa2msa

pfmake : pfmake.f psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f \
        sterr.f cvini.f remsf.f wrprf.f lblnk.f remsf.f wrprf.f lblnk.f recmd.f
	$(F77) $(FFLAGS) pfmake.f -o pfmake 

ptoh :  ptoh.f psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f sterr.f \
        hmdat.f cvini.f reprf.f rhnul.f wrsam.f wrhmr.f schmm.f dfnul.f lblnk.f 
	$(F77) $(FFLAGS) ptoh.f -o ptoh

pfw :   pfw.f sterr.f remsf.f lblnk.f
	$(F77) $(FFLAGS) pfw.f -o pfw 

pfscale : pfscale.f sterr.f recmd.f
	$(F77) $(FFLAGS) pfscale.f -o pfscale 

2ft :   2ft.f lblnk.f 
	$(F77) $(FFLAGS) 2ft.f -o 2ft 

6ft :   6ft.f lblnk.f 
	$(F77) $(FFLAGS) 6ft.f -o 6ft 

ptof :  ptof.f codat.f cvini.f dfdat.f djdat.f gsdat.f lblnk.f nodat.f pfdat.f \
        psdat.f reprf.f sterr.f wrprf.f recmd.f
	$(F77) $(FFLAGS) ptof.f -o ptof
