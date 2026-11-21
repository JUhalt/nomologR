## Resubmission

TODO (maintainer, before submission): this file prepares the CRAN submission
of 1.0.0, which follows its release on GitHub and R-universe. It holds four
TODO paragraphs, this one included; the other three wait for figures from the
submitted tarball: the cran-conditions workflow on the release commit (Linux
and Windows, `NOT_CRAN` unset), win-builder (release and devel), the macOS
builder, and R-hub (the rhub.yaml workflow). Delete all four before
submitting. If a later version than 1.0.0 is submitted, change the version in
the next paragraph.

This is a resubmission. nomologR is not on CRAN yet. Version 0.3.0 was
reviewed on 2026-10-08 and returned with two requests, which are answered
below. This submission is version 1.0.0, the package's first stable release,
which was published on GitHub and R-universe first. Development went on while
0.3.0 was under review; `NEWS.md` lists the changes.

> Please ensure that your functions do not write by default or in your
> examples/vignettes/tests in the user's home filespace (including the
> package directory and getwd()). [...] Please omit any default path in
> writing functions.

* `nomo_report()` is the only function in the package that writes a file
  outside `tempdir()`. In 0.3.0 its `file` argument had the default
  `"nomologR-report.html"`, a path in the working directory. The default is
  removed: `file` is required, and `nomo_report()` stops with an error that
  names the argument when it is not given. An existing file is replaced only
  with `overwrite = TRUE`.
* Examples, tests, and vignettes write only under `tempdir()`. The
  `nomo_report()` example writes to `tempfile()`. Tests create their files
  and directories with `tempfile()`, `withr::local_tempfile()`, or
  `withr::local_tempdir()`. The vignette that renders reports writes them to
  a directory it creates under `tempdir()` and removes at its end.
* The test suite sets its default graphics device to one with no file
  (`grDevices::pdf(file = NULL)`), so a plot drawn by a test cannot create
  `Rplots.pdf`.

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
* The package's functions name neither `.GlobalEnv` nor `globalenv()`, and
  none calls `set.seed()`, `assign()`, or `rm()`. The report template is
  evaluated in a new environment whose parent is the base environment
  (`new.env(parent = baseenv())`). Every `<<-` in the package sets a variable
  of an enclosing function.
* No test calls `assign()` or `rm()` on the global environment any more. The
  tests of the restore run inside `withr::with_seed()` and compare the
  random-number state read before a seeded call with the state read after it.
  A session that has no random-number state is tested in a fresh R process.
  Test fixtures that simulate data are still seeded with `set.seed()`, as are
  the simulated data in two examples (`nomo_hierarchical()`, `nomo_retest()`).
* A test now fails if a file in `R/` names `.GlobalEnv`, calls `set.seed()`,
  uses `globalenv()` for anything but the parent of a new environment, or uses
  `<<-` on a variable that no enclosing function holds.

## R CMD check results

TODO (maintainer, before submission): replace this paragraph with the result
line of `R CMD check --as-cran` on the submitted tarball, from win-builder and
from the cran-conditions workflow on the release commit. Keep the second
bullet below only for the words win-builder lists, and add any other NOTE. The
cran-conditions workflow also lists the examples that took over 5 s in the run
without `\donttest{}`, which CRAN notes.

* This is a new submission.
* win-builder lists possibly misspelled words in DESCRIPTION. Cronbach, Meehl,
  Hehman, and Pek are surnames of the authors cited there (Cronbach & Meehl,
  1955; Flake, Pek, & Hehman, 2017). "Nomological" is the technical term, as in
  a nomological network, that the package is named for. All are spelled
  correctly.

## Test environments

TODO (maintainer, before submission): list the environments the submitted
tarball was checked on, with their results: the cran-conditions workflow
(ubuntu-latest and windows-latest, R release, `NOT_CRAN` unset); win-builder
(R release and R-devel); the macOS builder; R-hub (linux, windows, and macos,
through the rhub.yaml workflow); and the R-CMD-check workflow (macOS, Windows,
and Ubuntu devel, release, and oldrel-1, plus Ubuntu with the minimum versions
declared in `Imports`). The results reported with 0.3.0 describe the version
that was reviewed and are not repeated.

## Notes for the reviewer

* Check time. Tests that fit many models (simulations, full guided workflows,
  and rendered reports) are skipped on CRAN with `testthat::skip_on_cran()`,
  and so are tests that replace a function of another package for the length
  of the test. They run in continuous integration, whose coverage job fails
  when a line in `R/` is not covered. Tests that check the package against
  lavaan's own estimates run on CRAN.
  TODO (maintainer, before submission): confirm that the coverage job passed
  on the release commit, and add the time of the whole check, the tests, and
  the vignettes from the cran-conditions workflow on that commit (its job
  summary gives them for Linux and Windows) and from win-builder. The 2.5
  minutes reported with 0.3.0 no longer holds, and no time measured on the
  maintainer's laptop is a figure to report.
* Examples. Every exported function has examples that run in the ordinary
  check, except `nomo_report()` and `nomo_revise()`, whose examples are
  wrapped whole in `\donttest{}`. The first renders a report, which needs
  pandoc. The second runs a guided workflow twice, once for the parent and
  once for the revision, and no smaller call shows it. The other examples in
  `\donttest{}` follow a quick example that runs, and show slower uses: a
  Monte Carlo simulation, bootstrap intervals, ordered indicators with
  polychoric correlations, partial and longitudinal invariance, a model
  comparison, a replication in split samples, a composite corrected for its
  unreliability, and the later steps of a guided workflow. Each is fully
  runnable, and all pass under `--run-donttest`.
* Files. `nomo_report()` is the only function that writes a file outside
  `tempdir()`, and only when given a path: its `file` argument is required
  and has no default.
* Random numbers. Each of the four functions that take a seed sets it with
  `withr::with_seed()`, which leaves the user's random-number state as it was.
  The package's own code does not write to the global environment.

## Downstream dependencies

None: this is a new package.
