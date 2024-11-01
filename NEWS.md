## version 1.0.1

- Trim leading and trailing whitespace from descriptions that are read in using the internal function `get_descriptions()`, addressing an issue Elisabeth raised in srppphist
- Remove html documentation built with pkgdown from the git repository, the online documentation is built using the github workflow
- Address issues found by the package review on CRAN as follows:
- Rename the package to make it clear that its purpose is to read in data from the Swiss Register of Plant Protection Products
- Wrap example code in \donttest{} to avoid CRAN notes for checks >5s. The code is still tested in the testthat tests.
- Document return values of all exported functions

## version 1.0.0

- Risk mitigation measures `sw_drift_dist`, `sw_runoff_dist`, `sw_runoff_points` and `biotope_drift_dist` are now stored as integers in the `obligations` table.
- In the table `cultures`, the primary keys of up to two parent cultures are now included.
- Fix the use rate calculations for liquid products for the case that only a dosage in percent is given.

## version 0.99.2

- R/srppp-xml.R: Revise the structure of the `srppp_dm` object, after verifying that the product sections of different products with the same P-Number are identical, with the exception of the permission holder. Therefore, all tables describing the products, including the use definitions, are now tied to the `pNbrs` table with the `pNbr` as the primary key, instead of the `products` table which has the `wNbr` as a primary key. Functions, example code, vignettes and tests were adapted accordingly.

## version 0.99.1

- vignettes/srppp.rmd: Add an overview vignette which is displayed with the link 'Get started' in the online documentation

## version 0.3.4

- Rename the package from 'psmv' to 'srppp'

## version 0.3.3

### Format changes

- Remove redundant information from the ingredients table by removing
  all W-Numbers containing a dash. P-Numbers were added to the ingredient
  table as well, so product compositions can more easily be obtained
  using the ingredients table. Finally remove W-Numbers from the ingredients
  table. As a consequence, products and ingredients must now be joined
  by 'pNbr', and the relationship is 'many-to-many', as a 'pNbr' can
  occur more than once in the products table.
- The grouping of the products table by P-Numbers was removed, as it
  seemed not to be used anywhere and created spurious messages during 
  constraint checking.
