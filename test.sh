#!/bin/sh
echo "#----------------------------------------------------------------------#"
echo "# pftools test 1: ./pfsearch -f -C 6.0 sh3.prf sh3.seq"
echo "#----------------------------------------------------------------------#"
./pfsearch -f -C 6.0 sh3.prf sh3.seq
echo "#----------------------------------------------------------------------#"
echo "# pftools test 2: ./pfsearch -bx ecp.prf CVPBR322 | ./psa2msa -du"
echo "#----------------------------------------------------------------------#"
./pfsearch -bx ecp.prf CVPBR322 | ./psa2msa -du
echo "#----------------------------------------------------------------------#"
echo "# pftools test 3: ./pfscan -s GTPA_HUMAN prosite13.prf"
echo "#----------------------------------------------------------------------#"
./pfscan -s GTPA_HUMAN prosite13.prf
echo "#----------------------------------------------------------------------#"
echo "# pftools test 4: ./pfscan -by -C 2 CVPBR322 ecp.prf"
echo "#----------------------------------------------------------------------#"
./pfscan -by -C 2 CVPBR322 ecp.prf
echo "#----------------------------------------------------------------------#"
echo "# pftools test 5: ./gtop -F 50 sh3.gpr | ./pfsearch -far - sh3.seq"
echo "#                    | sort -nr"
echo "#----------------------------------------------------------------------#"
./gtop -F 50 sh3.gpr | ./pfsearch -far - sh3.seq | sort -nr
echo "#----------------------------------------------------------------------#"
echo "# pftools test 6: ./htop pfam_sh3.hmm  | ./pfsearch -f - sh3.seq"
echo "                     | sort -nr"
echo "#----------------------------------------------------------------------#"
./htop pfam_sh3.hmm | ./pfsearch -f - sh3.seq | sort -nr
echo "#----------------------------------------------------------------------#"
echo "# pftools test 7: ./pfw -N 1000 sh3.msf |"
echo "#                    ./pfmake -b -H 0.6 - blosum45.cmp"
echo "#----------------------------------------------------------------------#"
./pfw -N 1000 sh3.msf | ./pfmake -b -H 0.6 - blosum45.cmp
echo "#----------------------------------------------------------------------#"
echo "# pftools test 8:./ptoh -L 1.15 ecp.prf"
echo "#----------------------------------------------------------------------#"
./ptoh -L 1.15 ecp.prf
echo "#----------------------------------------------------------------------#"
echo "# pftools test 9: ./pfscale -N 14147368 -P 0.0001 -Q 0.000001 score.lis"
echo "#                    | sed -n 1,25p"
echo "#----------------------------------------------------------------------#"
./pfscale -N 14147368 -P 0.0001 -Q 0.000001 score.lis | sed -n 1,25p
echo "#----------------------------------------------------------------------#"
echo "# pftools test 10: ./pfsearch -y coils.prf MYSA_HUMAN"
echo "#----------------------------------------------------------------------#"
./pfsearch -y coils.prf MYSA_HUMAN
echo "#----------------------------------------------------------------------#"
echo "# pftools test 11: /bin/rm sh3.fsp"
echo "#                 ./ptof -r -F -1.2 -I 0.6 -X -1.5 -B -0.5 sh3.prf >sh3.fsp"
echo "#                 ./2ft < R76849.seq | ./pfsearch -fy -C 5.0 sh3.fsp -"
echo "#                  /bin/rm sh3.fsp"
echo "#----------------------------------------------------------------------#"
/bin/rm sh3.fsp; ./ptof -r -F -1.2 -I 0.6 -X -1.5 -B -0.5 sh3.prf > sh3.fsp ; ./2ft < R76849.seq | ./pfsearch -fy -C 5.0 sh3.fsp -; /bin/rm sh3.fsp


