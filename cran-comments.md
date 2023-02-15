## Resubmission
This is a resubmission. In this version I have:

* Explained acronyms in the DESCRIPTION file.

* Written the references in the proper form in the DESCRIPTION file.

## R CMD check results
There were no ERRORs or WARNINGs. 

There were 4 NOTEs:

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
  

* checking examples ... NOTE
  Examples with CPU (user + system) or elapsed time > 5s
                      user system elapsed
  SVboundparametersM 2.039  0.075    5.33
  
  I cannot make the examples smaller, they run in under 10 seconds,
  and they run in under 5 seconds on the other platforms.




## Downstream dependencies
There are no downstream dependencies.
