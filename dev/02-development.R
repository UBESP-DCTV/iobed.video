
# Requirements ----------------------------------------------------
usethis::use_tidy_eval()

## development/suggested
dev_pkg <- c("")
renv::install(dev_pkg)
purrr::walk(dev_pkg, usethis::use_package, type = "Suggests")


## production/imports
pkg <- c("serial", "stringr", "readr", "dplyr", "lubridate", "usethis")
renv::install(pkg)
purrr::walk(pkg, usethis::use_package)

# Tools -----------------------------------------------------------
autotestthat::auto_test_package_job()


# Functions -------------------------------------------------------
usethis::use_r(basename(usethis::use_test("")))



#'
#' Check cycles
#' ====================================================================
#'
#' Before pushes
#' --------------------------------------------------------------------
#'
usethis::use_tidy_description()
spelling::spell_check_package()
spelling::update_wordlist()



#'
#' Before pull requests
#' --------------------------------------------------------------------
#'
lintr::lint_package()

usethis::use_version()
