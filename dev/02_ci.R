# GitHub + CI helpers (run interactively)
usethis::use_git()
# set your GitHub PAT first: usethis::create_github_token(); gitcreds::gitcreds_set()
usethis::use_github(open = FALSE)
usethis::use_github_action_check_standard()
usethis::use_github_action("test-coverage")
# optional: pkgdown site
# usethis::use_pkgdown()
# usethis::use_github_pages()
