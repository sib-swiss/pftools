FFLAGS= -O4

#For compilation under HP-UX uncomment following line 
#FFLAGS= +O4 +E6 +U77 -K

#For compilation under IRIX uncomment following line 
#FFLAGS= -O3 -static

all  :  gtop pfsearch pfscan psa2msa

gtop : gtop.f psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f sterr.f cvini.f lblnk.f regpr.f wrprf.f sterr.f
	f77 $(FFLAGS) gtop.f -o gtop

pfsearch: pfsearch.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f pxdat.f avdat.f sterr.f cvini.f reprf.f reseq.f rfseq.f xali1.f xalip.f RtoN.f NtoR.f CFAve.f CPAve.f wprsm.f xalit.f lblnk.f prali.f sterr.f
	f77 $(FFLAGS) pfsearch.f -o pfsearch

pfscan : pfscan.f psdat.f gsdat.f djdat.f nodat.f codat.f pfdat.f dfdat.f pxdat.f avdat.f sterr.f cvini.f abini.f reprf.f reseq.f rfseq.f xali1.f xalip.f RtoN.f NtoR.f CFAve.f CPAve.f wprsm.f xalit.f lblnk.f prali.f 
	f77 $(FFLAGS) pfscan.f -o pfscan

psa2msa : psa2msa.f sterr.f lblnk.f 
	f77 $(FFLAGS) psa2msa.f -o psa2msa
