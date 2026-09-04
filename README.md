# Description

This is a collection of helper functions developed to standardize MSI analysis across multiple datasets.

To add functions follow this workflow:

1. In RStudio, open the `cardinalHelpers` project.
2. Open a new R Script using  `usethis::use_r("<name of your function>")`.
3. Enter the function in the script.
4. Regenerate documentation and reinstall the package using `devtools::document()` and `devetools::install()`.
5. Use `devtools::check()` to check for issues (i.e., missing dependency declarations, etc.).
6. Commit and push to GitHub using the RStudio Git pane.
7. Confirm the push is live by installing the package from GitHub:
  ```
  pak::pak("eolder95/cardinalHelpers")
  library(cardinalHelpers)
  <test new function>
  ```

To add a new dependency to the whole package:

1. In RStudio, open the `cardinalHelpers` project.
2. Add the dependency using `usethis::use_package("<name of package>")`.
3. Regenerate documentation and reinstall the package using `devtools::document()` and `devetools::install()`.
4. Commit and push to GitHub using the RStudio Git pane.
 
