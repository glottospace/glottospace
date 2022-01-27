## Resubmission
This is a resubmission. In this version I have:

# Fixed ERRORS:
* Commented text in documentation of the function glottocreate()

# Fixed WARNINGS:
* Removed invalid license file pointer LICENSE

* Added packages to dependencies
* Removed Namespaces in Imports field not imported from

* Rd cross-references: removed missing links.

* Missing documentation for code objects are now documented (resulted from using @export and @noRd at the same time).
* Undocumented data sets are now documented

* Fixed documented/undocumented arguments. 
* Argument items added description.

# WARNINGS ignored:

* non-ASCII characters are in raw data should be kept. 

# NOTES

* Converted the DESCRIPTION title to title case.

## First submission: R CMD check results
There was 1 ERROR, no WARNINGs or NOTEs. 

The only ERROR is that loading failed for 'i386'. When installing from github, this can be solved by specifying: INSTALL_opts=c("--no-multiarch") to force building the package only for the current R version. I expect this won't cause problems to users since they will have only one R version (64 bit) installed. 

This is the first submission of the package. 

The package has been tested on Windows, Mac, and Ubuntu.The package builds and can be installed on all three platforms. 

However, on Ubuntu there are some issues with spatial packages RGEOS, RGDAL and sf on which glottospace depends. These issues were already known and are hard to fix within the scope of the current package: https://philmikejones.me/tutorials/2018-08-29-install-sf-ubuntu.html
https://philmikejones.me/tutorials/2014-07-14-installing-rgdal-in-r-on-linux.html
https://github.com/r-spatial/sf/issues/1419

## Downstream dependencies
There are currently no downstream dependencies for this package
