./pfsearch -f  sh3.prf    sh3.seq       C=6.0
./pfsearch -bx ecp.prf    CVPBR322              | ./psa2msa -du
./pfscan   -s  GTPA_HUMAN prosite13.prf
./pfscan   -by CVPBR322     ecp.prf      L=2
./gtop sh3.gpr F=50 | ./pfsearch -far - sh3.seq | sort -nr
