#!/bin/sh
echo "#----------------------------------------------------------------------#"
echo "# pftools test 1: ./pfsearch -f sh3.prf sh3.seq C=6.0"
echo "#----------------------------------------------------------------------#"
./pfsearch -f sh3.prf sh3.seq C=6.0
echo "#----------------------------------------------------------------------#"
echo "# pftools test 2: ./pfsearch -bx ecp.prf CVPBR322 | ./psa2msa -du"
echo "#----------------------------------------------------------------------#"
./pfsearch -bx ecp.prf CVPBR322 | ./psa2msa -du
echo "#----------------------------------------------------------------------#"
echo "# pftools test 3: ./pfscan -s GTPA_HUMAN prosite13.prf"
echo "#----------------------------------------------------------------------#"
./pfscan -s GTPA_HUMAN prosite13.prf
echo "#----------------------------------------------------------------------#"
echo "# pftools test 4: ./pfscan -by CVPBR322 ecp.prf L=2"
echo "#----------------------------------------------------------------------#"
./pfscan -by CVPBR322 ecp.prf L=2
echo "#----------------------------------------------------------------------#"
echo "# pftools test 5: ./gtop sh3.gpr F=50 | ./pfsearch -far - sh3.seq"
echo "#                    | sort -nr"
echo "#----------------------------------------------------------------------#"
./gtop sh3.gpr F=50 | ./pfsearch -far - sh3.seq | sort -nr
echo "#----------------------------------------------------------------------#"
echo "# pftools test 6: ./htop pfam_sh3.hmm  | ./pfsearch -f - sh3.seq"
echo "                     | sort -nr"
echo "#----------------------------------------------------------------------#"
./htop pfam_sh3.hmm | ./pfsearch -f - sh3.seq | sort -nr
echo "#----------------------------------------------------------------------#"
echo "# pftools test 7: ./pfw sh3.msf N=1000 |"
echo "#                    ./pfmake -b - blosum45.cmp H=0.6"
echo "#----------------------------------------------------------------------#"
./pfw sh3.msf N=1000 | ./pfmake -b - blosum45.cmp H=0.6
echo "#----------------------------------------------------------------------#"
echo "# pftools test 8:./ptoh ecp.prf L=1.15"
echo "#----------------------------------------------------------------------#"
./ptoh ecp.prf L=1.15
echo "#----------------------------------------------------------------------#"
echo "# pftools test 9: ./pfscale score.lis N=14147368 P=0.0001"
echo "#                    Q=0.000001 | sed -n 1,25p"
echo "#----------------------------------------------------------------------#"
./pfscale score.lis N=14147368 P=0.0001 Q=0.000001 | sed -n 1,25p
echo "#----------------------------------------------------------------------#"
echo "# pftools test 10: ./pfsearch -y coils.prf MYSA_HUMAN"
echo "#----------------------------------------------------------------------#"
./pfsearch -y coils.prf MYSA_HUMAN
echo "#----------------------------------------------------------------------#"
echo "# pftools test 11: /bin/rm sh3.fsp"
echo "#                 ./ptof -r sh3.prf F=-1.2 I=0.6 X=-1.5 B=-0.5 >sh3.fsp"
echo "#                 ./2ft < R76849.seq | ./pfsearch -fy  sh3.fsp - C=5.0"
echo "#                  /bin/rm sh3.fsp"
echo "#----------------------------------------------------------------------#"
/bin/rm sh3.fsp; ./ptof -r sh3.prf F=-1.2 I=0.6 X=-1.5 B=-0.5 > sh3.fsp ; ./2ft < R76849.seq | ./pfsearch -fy  sh3.fsp - C=5.0; /bin/rm sh3.fsp


