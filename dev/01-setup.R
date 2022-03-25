
usethis::create_package("~/GitHub-UBEP/iobed.video")
usethis::use_directory("dev", ignore = TRUE)
fs::file_create("dev/01-setup.R")
rstudioapi::navigateToFile("dev/01-setup.R")

usethis::use_roxygen_md()
usethis::use_news_md()

usethis::use_mit_license(c("Corrado Lanera", "Dario Gregori"))
usethis::use_package_doc()


usethis::use_readme_rmd()
usethis::use_logo(  # not in the pkg folder!
  "~/../OneDrive/LAIMS/LAIMS.png"
)
usethis::use_cran_badge()
usethis::use_lifecycle_badge("experimental")

usethis::use_code_of_conduct("corrado.lanera@ubep.unipd.it")

usethis::use_spell_check()
spelling::spell_check_package()
spelling::update_wordlist()

usethis::use_testthat()
writeLines("library(checkmate)", here::here("tests/testthat/setup.R"))
usethis::use_package("checkmate", type = "Suggests")
usethis::use_r(basename(usethis::use_test("test")))
renv::install("CorradoLanera/autotestthat")
usethis::use_dev_package("autotestthat", type = "Suggests")

usethis::use_tidy_description()

lintr::lint_package()

usethis::use_git()
usethis::git_vaccinate()
usethis::use_github("UBESP-DCTV", private = TRUE)
usethis::use_tidy_github()

renv::init()

usethis::use_github_action_check_standard()
usethis::use_github_action_check_release("R-CMD-check-develop.yaml")
usethis::use_github_actions_badge(name = "R-CMD-check-develop")
usethis::use_github_action("lint")
usethis::use_github_action("pkgdown")

usethis::use_tibble()
# required if used w/ Windows machines (even for CI/CD)
renv::install("tibble")
usethis::use_tibble()

renv::install("purrr")
usethis::use_package("purrr", type = "Suggests")
renv::install("withr")
usethis::use_package("withr", type = "Suggests")
renv::status()

usethis::use_tidy_description()
devtools::check_man()
spelling::spell_check_package()
spelling::update_wordlist()
lintr::lint_package()
renv::status()
devtools::check()

usethis::use_version("dev")

fs::file_create("dev/02-development.R")
rstudioapi::navigateToFile("dev/02-development.R")
