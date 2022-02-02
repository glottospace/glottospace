## Resubmission

Dear CRAN-team,

Thanks again for your time. This is a resubmission. 

There were no ERRORS or WARNINGS.

On Windows there were 3 NOTES, on Debian there was one additional NOTE:

* Possibly misspelled words in DESCRIPTION: I have checked the words and they are spelled correctly (related with package name, data sources, etc.) 

* Reading CITATION file fails: I have gone at great lenghts to understand where this NOTE comes from, but I haven't been able to understand what it means, or how to fix it. If I type citation("glottospace"), the citation is printed. There is a file with the name CITATION in the folder 'inst/'

* Non-standard files at top level: 'cran-comments.md' (this file) and 'news.md' (currently empty because this will be the first release). I have kept these files because they are required for a submission to CRAN. 

* The Debian pre-test indicated there were four files in the R directory. In the previous submission I had added those files to .Rbuildignore. This worked on the Windows pre-test, but not on Debian. I have now deleted all these files and added \dontrun{} to the examples that generated those files. 

Thanks and kind regards,
Sietze


## First submission: R CMD check results

Dear CRAN-team,

Thank you very much in advance for your time.

This is my first package. 

# ERRORS and WARNINGS

When running check_rhub(), there were no ERRORS or WARNINGS on Windows Server (64 bit), but a PREPERROR on Linux.

# NOTES

There were five NOTES:

* Possibly misspelled words in DESCRIPTION: I have checked the words and they are spelled correctly (related with package name, data sources, etc.) 

* Reading CITATION file fails: I have gone at great lenghts to understand where this NOTE comes from, but I haven't been able to understand what it means, or how to fix it. If I type citation("glottospace") , the citation is printed. 

* Package dependencies: Imports includes 32 packages. I am sorry, I don't see how the number of dependencies could be further reduced, glottospace covers a broad functionality (from data wrangling to spatial analysis and visualisation).

* Non-standard files at top level: 'cran-comments.md' (this file) and 'news.md' (currently empty because this will be the first release)

* check_rhub() generates a note about 'rnaturalearthdata' being in the Imports while it is supposedly unused. However, rnaturalearthdata is used within the rnaturalearthdata package on which my package depends. Removing 'rnaturalearthdata' from the Imports generates an ERROR. I have therefore kept the package listed as Imports.

* The following file/directory was found in the temp directory: 'lastMiKTeXException' is a know issue on rhub: https://github.com/r-hub/rhub/issues/503


# R CMD CHECK locally with check()

The package has been manually tested on Windows, Mac, and Ubuntu.The package builds and can be installed on all three platforms.

When running R CMD CHECK on windows, there is one ERROR stating that loading failed for 'i386'. When specifying 'install(force = TRUE, args=c("--no-multiarch")) ' the package can be installed.

When installing the package on Ubuntu there are some issues with spatial packages RGEOS, RGDAL and sf on which glottospace depends. These issues were already known and are hard to fix within the scope of the current package: https://philmikejones.me/tutorials/2018-08-29-install-sf-ubuntu.html
https://philmikejones.me/tutorials/2014-07-14-installing-rgdal-in-r-on-linux.html
https://github.com/r-spatial/sf/issues/1419

## Downstream dependencies
There are currently no downstream dependencies for this package
