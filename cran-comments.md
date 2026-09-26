## Submission

This is the first CRAN submission of nomologR.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new submission.
* win-builder lists possibly misspelled words in DESCRIPTION. Cronbach, Meehl,
  Hehman, and Pek are surnames of the authors cited there (Cronbach & Meehl,
  1955; Flake, Pek, & Hehman, 2017). "Nomological" is the technical term, as in
  a nomological network, that the package is named for. All are spelled
  correctly.

## Test environments

* Local: Windows 11, R 4.6.1, `R CMD check --as-cran` on the built tarball:
  1 NOTE (new submission).
* macOS builder: R 4.6.1 Patched (2026-07-27 r90311), aarch64-apple-darwin23:
  OK.
* win-builder: R 4.6.1 (2026-06-24 ucrt) and R-devel (2026-09-25 r90590
  ucrt): 1 NOTE each, the one described above.
* R-hub, R-devel (4.7.0): Ubuntu 24.04 (x86_64-pc-linux-gnu), Windows Server
  2022 (x86_64-w64-mingw32), and macOS 15 (x86_64-apple-darwin20): all OK.
* GitHub Actions: macOS (release), Windows (release), and Ubuntu (devel,
  release, oldrel-1), plus Ubuntu with the minimum versions declared in
  `Imports`. All OK.

## Notes for the reviewer

* Check time. Tests that fit many models (simulations, full guided workflows,
  and rendered reports) are skipped on CRAN with `testthat::skip_on_cran()`.
  They run in continuous integration, where executable-line coverage is 100%.
  Tests that check the package against lavaan's own estimates run on CRAN. On
  the macOS builder the whole check took 2.5 minutes.
* Examples wrapped in `\donttest{}` refit models repeatedly (bootstrap
  intervals, several missing-data strategies). Each is fully runnable, and all
  pass under `--run-donttest`.
* The package writes files only where the user asks: `nomo_report()` writes to
  its `file` argument, and examples, tests, and vignettes use `tempfile()`.
* Where a function sets a seed, it restores the user's random-number state on
  exit.

## Downstream dependencies

None: this is a new package.
