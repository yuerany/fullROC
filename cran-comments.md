## Test environments
* local R installation, R 4.2.3
* ubuntu 16.04 (on travis-ci), R 4.2.3
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 0 notes


## Resubmission

This is a resubmission (3/23/2023). In this version I have:

* Added additional functions to add ROC lines to an existing ROC plot and to create z-ROC curves.

* Updated the response_simu() function to enable users to simulate showup data.

* Added additional arguments to the roc_plot() function to enable users to output cumulative data that used to create ROC curves.

* Updated doi for the Yang & Smith (2022) reference.

This is a resubmission (1/8/2021). In this version I have:

* Reduce the length of the title to 49 characters (with spaces).

* Added references to the description of the DESCRIPTION file.


This is a resubmission (1/6/2021). In this version I have:

* Added explanations of all acronyms to the DESCRIPTION.

* Dropped references from the DESCRIPTION file and added them to the function documents.

* Changed print()/cat() to message()/warning().


