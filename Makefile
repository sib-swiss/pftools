all : GtoP pfsearch

GtoP : GtoP.f rgprf.f wrprf.f codat.f dfdat.f djdat.f gsdat.f nodat.f pfdat.f psdat.f 
	f77 GtoP.f -o GtoP

pfsearch: pfsearch.f reprf.f reseq.f rfseq.f xali1.f xalip.f RtoN.f NtoR.f codat.f dfdat.f djdat.f gsdat.f nodat.f pfdat.f psdat.f pxdat.f
	f77 -O4 pfsearch.f -o pfsearch
