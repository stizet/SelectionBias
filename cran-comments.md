## R CMD check results
There were no ERRORs or WARNINGs. 

There were 3 NOTEs:

* checking CRAN incoming feasibility ... [6s/17s] NOTE
  Maintainer: ‘Stina Zetterstrom <stina.zetterstrom@statistics.uu.se>’
  New submission
  Possibly misspelled words in DESCRIPTION:
    SV (11:59, 12:9)
    VanderWeeles (13:40)
    Waernbaum (15:22)
    Zetterstrom (15:6)

  This is a new submission. The email address is correct. The words are not
  misspelled.


Only on Windows Server 2022, R-devel, 64 bit:
* checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
  'lastMiKTeXException'
  
  Apparently, this is a known issue with Rhub and does not
  suggest a problem with the package.

Only on Fedora Linux, R-devel, clang, gfortran:
* checking HTML version of manual ... NOTE
  Skipping checking HTML validation: no command 'tidy' found
  
  I cannot change that Tidy is not on the path, or update Tidy on
  the external Fedora Linux server.



## Downstream dependencies
There are no downstream dependencies.
