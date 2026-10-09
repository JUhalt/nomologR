## Resubmission

TODO (maintainer, before submission): this file holds four TODO paragraphs,
this one included. Each says what to refresh; delete all four before
submitting. If a patched 0.3.0 is resubmitted in place of the current version,
also drop the sentence about the version number below and the mention of
`nomo_power_simulate()`, which 0.3.0 does not have; the functions that take a
seed are then three, not four.

This is a resubmission. nomologR is not on CRAN yet. Version 0.3.0 was
reviewed on 2026-10-08 and returned with two requests, which are answered
below. The version number is higher than 0.3.0 because development went on
while 0.3.0 was under review; `NEWS.md` lists the changes.

> Please ensure that your functions do not write by default or in your
> examples/vignettes/tests in the user's home filespace (including the
> package directory and getwd()). [...] Please omit any default path in
> writing functions.

* `nomo_report()` is the only function in the package that writes a file. In
  0.3.0 its `file` argument had the default `"nomologR-report.html"`, a path in
  the working directory. The default is removed: `file` is required, and
  `nomo_report()` stops with an error that names the argument when it is not
  given. An existing file is replaced only with `overwrite = TRUE`.
* Examples, tests, and vignettes write only under `tempdir()`. The
  `nomo_report()` example writes to `tempfile()`. Tests create their files
  with `tempfile()`, `withr::local_tempfile()`, or `withr::local_tempdir()`,
  and no test gives `nomo_report()` a relative path. The vignette that renders
  reports writes them to a directory it creates under `tempdir()` and removes
  at its end.
* The test suite keeps a null graphics device open (`grDevices::pdf(NULL)`),
  so a plot drawn by a test cannot create `Rplots.pdf`.

> Please do not modify the .GlobalEnv.

* In 0.3.0, `nomo_factors()` and `nomo_split()` saved the user's
  `.Random.seed` before setting a seed and put it back on exit with `assign()`
  and `rm()` on `.GlobalEnv`. That code is removed. The four functions that
  take a seed now set it with `withr::with_seed()`, and withr is in `Imports`:
  `nomo_factors()`, `nomo_split()`, `nomo_power_simulate()` (new since 0.3.0),
  and `nomo_reliability()`.
* `nomo_reliability()` had no such code of its own. It passes its `ci_seed`
  to `lavaan::bootstrapLavaan()` as `iseed`, and that call now runs inside
  `withr::with_seed()` as well, so the user's random-number state is put back
  whatever the installed version of lavaan does, also when the bootstrap
  fails.
* The package's code no longer names `.GlobalEnv` and has no call to
  `set.seed()`, `assign()`, or `rm()`. `globalenv()` appears once, in
  `nomo_report()`, as the parent of the new environment in which the report
  template is evaluated (`new.env(parent = globalenv())`); nothing is assigned
  into it. Every `<<-` in the package sets a variable that an enclosing
  function created.
* No test calls `assign()` or `rm()` on the global environment any more. The
  tests of the restore run inside `withr::with_seed()` and compare the
  random-number state read before a seeded call with the state read after it.
  A session that has no random-number state is tested in a fresh R process.
  Test fixtures that simulate data are still seeded with `set.seed()`.
* A test now fails if a file in `R/` names `.GlobalEnv`, calls `set.seed()`,
  uses `globalenv()` for anything but the parent of a new environment, or uses
  `<<-` on a variable that no enclosing function holds.

## R CMD check results

TODO (maintainer, before submission): replace this paragraph with the result
line of `R CMD check --as-cran` on the tarball that is submitted. No such
result exists yet. The development version (0.9.0) was last checked on
2026-10-09 on a Windows 11 laptop that was running other jobs (`R CMD check
--as-cran --no-manual`, R 4.6.1): 0 errors | 0 warnings | 3 notes. The notes
were the new-submission note; "Problems with news in 'NEWS.md'", which goes
when the first heading of `NEWS.md` carries the version number; and "Examples
with CPU (user + system) or elapsed time > 5s", for `nomo_run()` alone and by
elapsed time only (8.0 s elapsed, 2.0 s of CPU). No example took 5 s of CPU.
Neither of the last two notes may appear for the submitted tarball: read the
example timings that win-builder reports.

* This is a new submission.
* win-builder lists possibly misspelled words in DESCRIPTION. Cronbach, Meehl,
  Hehman, and Pek are surnames of the authors cited there (Cronbach & Meehl,
  1955; Flake, Pek, & Hehman, 2017). "Nomological" is the technical term, as in
  a nomological network, that the package is named for. All are spelled
  correctly.

## Test environments

TODO (maintainer, before submission): list the environments the submitted
tarball was checked on, with their results. The results reported with 0.3.0
(local, macOS builder, win-builder, R-hub, GitHub Actions) are removed because
they describe the version that was reviewed. None of those services has been
run on this version yet.

## Notes for the reviewer

* Check time. Tests that fit many models (simulations, full guided workflows,
  and rendered reports) are skipped on CRAN with `testthat::skip_on_cran()`,
  and so are tests that replace a function of another package for the length
  of the test. They run in continuous integration, where executable-line
  coverage is 100%. Tests that check the package against lavaan's own
  estimates run on CRAN.
  TODO (maintainer, before submission): confirm that the coverage job still
  reports 100% for every file in `R/` on the submitted commit (a local covr
  run of the whole suite on 2026-10-09 gave 100% for each of the 61 files
  that hold executable lines),
  and add the whole check time that win-builder or the macOS builder reports.
  The 2.5 minutes reported with 0.3.0 no longer holds. In the local check
  described above the tests took 12 minutes, the vignettes 37 minutes, and the
  whole check 61 minutes, beside two other long R jobs; an earlier check with
  less running beside it took 13, 11, and 45 minutes. That laptop takes about
  four times as long as GitHub's Windows runner to rebuild the vignettes, so
  these are not the figures to report.
* Examples. Every exported function has examples that run in the ordinary
  check, except `nomo_report()` and `nomo_revise()`, whose examples are
  wrapped whole in `\donttest{}`. The first renders a report, which needs
  pandoc. The second runs a guided workflow twice, once for the parent and
  once for the revision, and no smaller call shows it. The other examples in
  `\donttest{}` run a Monte Carlo simulation or refit models several times
  (bootstrap intervals, missing-data strategies, partial invariance, model
  comparison). Each is fully runnable, and all pass under `--run-donttest`.
* Files. `nomo_report()` is the only function that writes a file, and only
  when given a path: its `file` argument is required and has no default.
* Random numbers. Each of the four functions that take a seed sets it with
  `withr::with_seed()`, which leaves the user's random-number state as it was.
  The package's own code does not write to the global environment.

## Downstream dependencies

None: this is a new package.
