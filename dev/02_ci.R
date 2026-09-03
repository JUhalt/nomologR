# GitHub / CI maintenance --------------------------------------------------
#
# The public GitHub repository and workflow files already exist. Do not call
# usethis::use_github() during routine development.

usethis::git_sitrep()

# If a workflow ever needs to be recreated, use the generic current helper:
# usethis::use_github_action("check-standard")
# usethis::use_github_action("test-coverage")
#
# Normal pre-push gate:
#   devtools::document()
#   devtools::test()
#   devtools::check()
#
# Then commit/push and require GitHub Actions to agree with the local result.
