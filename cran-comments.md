## Fifth resubmission

Dear Uwe Ligges/Gregor Seyer, 

There was one non-standard file in the check directory. I've now replaced dontrun with donttest in the specific exmaple that created the file.

Thanks again for your time.

All the best,
Sietze

## Fourth resubmission

Dear Uwe Ligges/Gregor Seyer, 

The package was archived because data was stored in ~/.local/share

I have implemented the following changes:
- A lightweight version of the global databases is stored internally (within the package). These data are used by default (download = FALSE).
- If the user specifies the argument download = TRUE, the user has to confirm that she/he agrees with downloading the data. If the user does not specify a directory, the data will be stored in a temporary directory.

Thanks again for your time.

All the best,
Sietze



## Third resubmission

Dear Uwe Ligges/Gregor Seyer, 

Thanks again for your time and patience. This is a resubmission. 

Below I respond to the issues you highlighted in your mail: 

* ISSUE: Please do not start the description with "This package", package name, title or similar.
* FIX: Implemented all suggested changes. 

* ISSUE:Please always write package names, software names and API (application programming interface) names in single quotes in title and description. e.g: --> 'glottospace'
* FIX: Implemented all suggested changes.

* ISSUE: Please add \value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. 
* FIX: I have drastically reduced the number of exported functions. Previously I used @keywords internal while still exporting the functions. I now use @noRd for those functions. For the remaining functions I now specify the class and what the output means. 

* ISSUE: You write information messages to the console that cannot be easily suppressed.
It is more R like to generate objects that can be used to extract the information a user is interested in, and then print() that object. Instead of print()/cat() rather use message()/warning()  or if(verbose)cat(..) (or maybe stop()) if you really have to write text to the console.
(except for print, summary, interactive functions)
* FIX: I have now replaced all occurrences of print()/cat() with message(). The only exceptions are in the glottomap() and glottoplot() functions, because their output should be printed. 

* ISSUE: Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies.
In your examples/vignettes/tests you can write to tempdir(). 
* FIX: The functions in the package only save a file if the filename argument is specified. I had already wrapped those examples that illustrate how to save a file in donttest{}, I have now also added tempdir() to those examples that write files, following your suggestion. 

Kind regards,
Sietze

## Second resubmission

Dear CRAN-team,

Thanks again for your time. This is a resubmission. 

There were no ERRORS or WARNINGS.

There was one NOTE:
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
