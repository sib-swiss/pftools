#! /usr/bin/env bash
#
# $Id: install_pftools.sh,v 1.4 2003/09/22 15:17:03 vflegel Exp $
#
# This shell script installs all components of the pftools package into
# the specified directories.
#
# Usage: install_pftools.sh <baseDirectory>
#
# Description: The script will copy the package components into a given
#              base directory. This directory is given as a command line
#              parameter of the shell script, but may be redefined through
#              user input. It then makes links to the executables in
#              a user defined bin directory as well as links to the man pages
#              in a user specified man directory.
#
# Author:      Volker Flegel
# Email:       pftools@isb-sib.ch
#
# Date:        May 2003
#

# Get program name
prgName=`basename "$0"`

# Helper functions
Usage () {
  echo "$prgName - install the pftools package, man pages and executables.

Usage: $prgName <baseDirectory>
    baseDirectory   directory where the pftools orginal files should be \
installed.

" >&2
  exit 1
}

Msg () {
  echo
  for MsgLine
    do echo "$prgName: $MsgLine" >&2
  done
}

Fatal () { Msg "$@"; exit 1; }

# Check command line arguments
if [ $# != 1 ]; then
  Usage
fi

# Set command variables
rmCmd=`which rm`
cpCmd=`which cp`

# Set package directories
pkgDir="$1"
binDir="/usr/bin/"
manDir="/usr/man/"

# Inform user about script action
echo
echo "This will install the pftools package and components into their"
echo "respective directories."
echo
echo "Make sure you have write permissions on the different installation directories."
echo

# Get package directory
echo -n "Where should the pftools package be installed ? [ $pkgDir ] "
read userInput
pkgDir=${userInput:-$pkgDir}

# Get binary directory
echo -n "Where should the pftools executables be installed ? [ $binDir ] "
read userInput
binDir=${userInput:-$binDir}

# Get man page directory
echo -n "Where should the pftools man pages be installed ? [ $manDir ] "
read userInput
manDir=${userInput:-$manDir}

# Test and remove package directory
if [ -d $pkgDir ]; then
  echo
  echo "Package directory $pkgDir already exists."
  echo "Please choose another destination."
#  echo "Overwrite directory $pkgDir ? [ y/n ] "
#  read -s -n 1 userInput
#  if [ "$userInput" == 'y' ]; then
#    echo -n " Removing directory $pkgDir ... "
#    $rmCmd -rf $pkgDir 2>/dev/null || Fatal "Failed to remove $pkgDir";
#    echo "OK"
#  else
    Fatal "... aborting."
#  fi
fi

# Create package directory
echo
echo -n " Creating directory $pkgDir ... "
mkdir -p $pkgDir 2>/dev/null || Fatal "Failed to create $pkgDir";
echo "OK"

# Create package subdirectory
for subDir in bin man; do
  echo -n " Creating directory ${pkgDir}/${subDir} ... "
  mkdir -p ${pkgDir}/${subDir} 2>/dev/null || Fatal "Failed to create ${pkgDir}/${subDir}";
  echo "OK"
done


# Test and create binary directory
if [ ! -d $binDir ]; then
  echo
  echo "Binary directory $binDir does not exist."
  echo "Create directory $binDir ? [ y/n ] "
  read -s -n 1 userInput
  if [ "$userInput" == 'y' ]; then
    echo -n " Creating directory $binDir ... "
    mkdir -p $binDir 2>/dev/null || Fatal "Failed to create $binDir";
    echo "OK"
  else
    Fatal "... aborting."
  fi
fi  

# Test and create man directory
if [ ! -d $manDir ]; then
  echo
  echo "Man page directory $manDir does not exist."
  echo "Create directory $manDir ? [ y/n ] "
  read -s -n 1 userInput
  if [ "$userInput" == 'y' ]; then
    echo -n " Creating directory $manDir ... "
    mkdir -p $manDir 2>/dev/null || Fatal "Failed to create $manDir";
    echo "OK"
  else
    Fatal "... aborting."
  fi
fi

# Test and create man1 and man5 directories
for subDir in ${manDir}/man1 ${manDir}/man5; do
  if [ ! -d $subDir ]; then
    echo -n " Creating directory $subDir ... "
    mkdir -p $subDir 2>/dev/null || Fatal "Failed to create $subDir";
    echo "OK"
  fi
done

# Copy package files to package directory
pkgFiles="CVPBR322 GTPA_HUMAN MYSA_HUMAN coils.prf pam220.cmp score.lis ecp.prf pam250.cmp sh3.gpr \
R76849.seq gonnet.cmp pam30.cmp sh3.msf blosum100.cmp pam40.cmp sh3.prf \
blosum30.cmp pam400.cmp sh3.seq blosum45.cmp pam80.cmp standard.random blosum50.cmp pfam_sh3.hmm test.out \
blosum62.cmp pam120.cmp test.sh blosum65.cmp pam160.cmp profile.txt blosum80.cmp pam200.cmp \
README prosite13.prf"

for myFile in $pkgFiles; do
  $cpCmd $myFile ${pkgDir}/${myFile} 2>/dev/null ||  Fatal "Failed to copy $myFile";
done

# Copy and link binary files to bin directory
binFiles="gtop pfmake pfscan pfw ptoh htop pfscale pfsearch psa2msa 2ft 6ft ptof"
unset allFiles

for myFile in $binFiles; do
  if ([ -e ${binDir}/${myFile} ] || [ -h ${binDir}/${myFile} ]) && [ "$allFiles" != 'a' ]; then
    echo "Destination file '${binDir}/${myFile}' exists."
    echo "Overwrite file '${binDir}/${myFile}' ? [ yes/no/all ] "
    read -s -n 1 allFiles
    if [ "$allFiles" != 'y' ] && [ "$allFiles" != 'a' ]; then
      Fatal "Another version of $myFile exists. Aborting..."
    fi
  fi
  if [ -e ${binDir}/${myFile} ] || [ -h ${binDir}/${myFile} ]; then
    $rmCmd -f ${binDir}/${myFile} 2>/dev/null ||  Fatal "Failed to remove $myFile";
  fi
  $cpCmd $myFile ${pkgDir}/bin/${myFile} ||  Fatal "Failed to copy $myFile";
  ln -s ${pkgDir}/bin/$myFile ${binDir}/${myFile} ||  Fatal "Failed to link $myFile";
done

# Copy and link man1 files to man1 directory
manFiles="gtop.1 pfmake.1 pfscan.1 pfw.1 ptoh.1 htop.1 pfscale.1 pfsearch.1 psa2msa.1 2ft.1 6ft.1 ptof.1"
unset allFiles

for myFile in $manFiles; do
  if ([ -e ${manDir}/man1/${myFile} ] || [ -h ${manDir}/man1/${myFile} ]) && [ "$allFiles" != 'a' ]; then
    echo "Destination file '${manDir}/man1/${myFile}' exists."
    echo "Overwrite file '${manDir}/man1/${myFile}' ? [ yes/no/all ] "
    read -s -n 1 allFiles
    if [ "$allFiles" != 'y' ] && [ "$allFiles" != 'a' ]; then
      Fatal "Another version of $myFile exists. Aborting..."
    fi
  fi
  if [ -e ${manDir}/man1/${myFile} ] || [ -h ${manDir}/man1/${myFile} ]; then
    $rmCmd -f ${manDir}/man1/${myFile} 2>/dev/null ||  Fatal "Failed to remove $myFile";
  fi
  $cpCmd man/$myFile ${pkgDir}/man/${myFile} 2>/dev/null ||  Fatal "Failed to copy $myFile";
  ln -s ${pkgDir}/man/$myFile ${manDir}/man1/${myFile} 2>/dev/null ||  Fatal "Failed to link $myFile";
done

# Copy and link man5 files to man5 directory
manFiles=" psa.5 xpsa.5"
unset allFiles

for myFile in $manFiles; do
  if ([ -e ${manDir}/man5/${myFile} ] || [ -h ${manDir}/man5/${myFile} ]) && [ "$allFiles" != 'a' ]; then
    echo "Destination file '${manDir}/man5/${myFile}' exists."
    echo "Overwrite file '${manDir}/man5/${myFile}' ? [ yes/no/all ] "
    read -s -n 1 allFiles
    if [ "$allFiles" != 'y' ] && [ "$allFiles" != 'a' ]; then
      Fatal "Another version of $myFile exists. Aborting..."
    fi
  fi
  if [ -e ${manDir}/man5/${myFile} ] || [ -h ${manDir}/man5/${myFile} ]; then
    $rmCmd -f ${manDir}/man5/${myFile} 2>/dev/null ||  Fatal "Failed to remove $myFile";
  fi
  $cpCmd man/$myFile ${pkgDir}/man/${myFile} 2>/dev/null ||  Fatal "Failed to copy $myFile";
  ln -s ${pkgDir}/man/$myFile ${manDir}/man5/${myFile} 2>/dev/null ||  Fatal "Failed to link $myFile";
done

echo
echo "Installation seems successful."
echo
echo "Install directories: $pkgDir $binDir $manDir"
exit 0
