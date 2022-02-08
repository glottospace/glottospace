
## Second resubmission

Dear CRAN-team,

Thanks again for your time. This is a resubmission. 

There were no ERRORS or WARNINGS.

There was 1 NOTE:

* Possibly misspelled words in DESCRIPTION: I have checked the words and they are spelled correctly (the words are related with the package name, data sources, etc.) 

Kind regards,
Sietze

## Resubmission

Dear CRAN-team,

Thanks again for your time. This is a resubmission. 

There were no ERRORS or WARNINGS.

There were 6 NOTES. Here I address them one by one:

* Possibly misspelled words in DESCRIPTION: I have checked the words and they are spelled correctly (the words are related with the package name, data sources, etc.) 

* Reading CITATION file fails: I had formatted the CITATION file incorrectly. While citation("glottospace") gave the correct output, it was mentioned in the CRAN pretest. I have fixed this by changing the bibtype to 'Unpublished' instead of 'Article' (since the manuscript hasn't been published yet). In addition I have now wrapped as.person() within personList(). 

* Non-standard files at top level: 'cran-comments.md' (this file) and 'news.md' (currently empty because this will be the first release). I have kept these files because they are required for a submission to CRAN. 

* Examples with elapsed time > 10s or elapsed time >5s. While CRAN pretest highlight these as two separate notes, I here address them together. The glottospace package works with several large online databases. When the user calls a function that requires any of these databases to be loaded, it first checks which version is available locally and what is the most recent online version. If the local version is outdated, the newest version will be downloaded, unpacked, reformatted etc. Because of these processing steps, it takes more time during the first run. I assume that CRAN pretests run examples only once, and I assume this is why they take more time. The functions in the package that download these databases provide messages to keep the user updated about what's happening behind the scenes. 

* The Debian pre-test indicated there were four files in the R directory. In the previous submission I had added those files to .Rbuildignore. This worked on the Windows pre-test, but not on Debian. I have now deleted all these files and also added \dontrun{} to the examples that generated those files. 

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
