#=============================================================================
# Put together the package
#=============================================================================

# WORKFLOW: UPDATE EXISTING PACKAGE
# 1) Modify package content and documentation.
# 2) Increase package number in "use_description" below.
# 3) Go through this script and carefully answer "no" if a "use_*" function
#    asks to overwrite the existing files. Don't skip that function call.
# devtools::load_all()

library(usethis)

# Sketch of description file
use_description(
  fields = list(
    Title = "Permutation SHAP",
    Version = "0.1.0",
    Description = "Multivariate implementation of permutation SHAP for models with
    up to 16 features. The results are exact regarding the selected background data.
    The package plays well together with meta-learning packages
    like 'tidymodels', 'caret' or 'mlr3'.
    Visualizations can be done using the R package 'shapviz'.",
    `Authors@R` =
    "c(person('Michael', family='Mayer', role=c('aut', 'cre'), email='mayermichael79@gmail.com')
     )",
    Depends = "R (>= 3.2.0)",
    LazyData = NULL
  ),
  roxygen = TRUE
)

use_package("foreach", "Imports")
use_package("stats", "Imports")
use_package("utils", "Imports")

use_package("doFuture", "Suggests")

use_gpl_license(2)

# Your files that do not belong to the package itself (others are added by "use_* function")
use_build_ignore(c("^packaging.R$", "[.]Rproj$",
                   "^cran-comments.md$", "^logo.png$"), escape = FALSE)

# If your code uses the pipe operator %>%
# use_pipe()

# If your package contains data. Google how to document
# use_data()

# Add short docu in Markdown (without running R code)
use_readme_md()

# Longer docu in RMarkdown (with running R code). Often quite similar to readme.
# use_vignette("permshap")

# If you want to add unit tests
use_testthat()
# use_test("permshap.R")

# On top of NEWS.md, describe changes made to the package
use_news_md()

# Add logo
use_logo("logo.png")

# If package goes to CRAN: infos (check results etc.) for CRAN
use_cran_comments()

use_github_links() # use this if this project is on github

# Github actions
use_github_action("check-standard")
use_github_action("pkgdown")

# Revdep
use_revdep()


#=============================================================================
# Finish package building (can use fresh session)
#=============================================================================

library(devtools)

document()
test()
check(manual = TRUE, cran = TRUE)
build()
# build(binary = TRUE)
install(upgrade = FALSE)

# Run only if package is public(!) and should go to CRAN
if (FALSE) {
  check_win_devel()
  check_rhub()

  # Wait until above checks are passed without relevant notes/warnings
  # then submit to CRAN
  release()
}
