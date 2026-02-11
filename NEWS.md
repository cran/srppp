## version 2.0.2

- Include the latest XML file published the registration authorities instead of the file from December 2025, because in the current version the information to correctly read the culture tree is included again.
- `resolve_cultures()`: The warning that was issued when the function was used with the new format was removed, as everything seems to be working fine again.

## version 2.0.1 (only published on github)

- Get the substance type (i.e. ACTIVE_INGREDIENT, ADDITIVE_TO_DECLARE, SAFENER, SYNERGIST) of ingredients also from XML files in the new format. Thanks to Alexandre Gurba (FOEN) for reporting the problem.

## version 2.0.0

- Adapt to the new format of the XML file. The changes are explained at https://www.blv.admin.ch/blv/de/home/zulassung-pflanzenschutzmittel/zugelassene-pflanzenschutzmittel/pflanzenschutzmittelverzeichnis.html
- Due to UUIDs being used for cross-referencing instead of integer identifiers, the corresponding identifier columns in several tables in the `srppp_dm` objects have changed their type from integer to character.
- The culture tree constructed from the new version of the XML has changed, among other changes the cultures `allg. Obstbau`, `allg. Feldbau`, `allg. Gemüsebau` and similar are not defined any more as parent cultures of any of the cultures listed. The functionality of `resolve_cultures()` and `alternative_products()`
is not yet adapted to this new situation and will give a warning when working with a register read in from the new format.

## version 1.1.0 (CRAN release: 2025-07-07)

- Example code and vignettes: To satisfy CRAN policy, fall back to use the register version distributed as test data with the package (see below) in case downloading  or reading from the standard URL fails, instead of throwing an error. This makes the check process on CRAN robust against server outage or a changed URL.
- Include information on the completeness of the effect against each target organism in the column `type` of the `pests` table of the `srppp_dm` object. Possible types are `PEST_FULL_EFFECT`, `PEST_PARTIAL_EFFECT` and `PEST_SIDE_EFFECT`. In the `obligations` table, include a column `varying_effect`, that contains `TRUE` in case the obligation text indicates that a full effect has not been demonstrated in all cases.
- `R/resolve_cultures.R`: Function to expand a dataframe with culture names at the lowest level (leafs) of a culture tree constructed by `srppp_dm`.
- Add a culture tree as a `Node` object from the `data.tree` package as attribute to `srppp_dm` objects and show it in the main vignette.
- Remove the vignette intended for JOSS, as it was rejected there
- Remove duplicate ingredient entries for the same substance in the same product (issue #6)
- `inst/testdata/`: Add the zipped XML from 16 December 2024 for testing purposes and test the resulting `srppp_dm` object for referential integrity
- `srppp_dm()`: Add argument `verbose` with default `TRUE`.

## version 1.0.1 (CRAN release: 2024-11-01)

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
