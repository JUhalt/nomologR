# Simulated teaching datasets -------------------------------------------------

#' Simulated two-factor item data with known teaching features
#'
#' A simulated scale-development dataset with two correlated latent factors and
#' five candidate indicators per factor. Because the population model is known,
#' learners can compare `nomologR` evidence with the structure that actually
#' generated the data.
#'
#' The data are deliberately imperfect in ways that scale developers routinely
#' encounter:
#'
#' * `a5` cross-loads on both factors (population loadings .45 and .35);
#' * `b5` is a weak indicator (population loading .30);
#' * `a2` (15 cases) and `b3` (12 cases) contain values missing completely at
#'   random.
#'
#' These features are included so that item-audit, factor-retention, and EFA
#' output have something substantive to explain. They are not instructions to
#' delete `a5` or `b5`; whether such items remain depends on theory, content
#' coverage, and later evidence.
#'
#' @format A data frame with 500 rows and 10 numeric columns:
#' \describe{
#'   \item{a1, a2, a3, a4, a5}{Indicators written for factor A. Population
#'     standardized loadings are .80, .75, .70, .72, and .45 (with a .35
#'     cross-loading on factor B for `a5`).}
#'   \item{b1, b2, b3, b4, b5}{Indicators written for factor B. Population
#'     standardized loadings are .78, .74, .80, .70, and .30.}
#' }
#'
#' @details
#' Population model: factors A and B are standard normal with correlation .40.
#' Each indicator is a linear function of its factor(s) plus normal unique
#' variance chosen so that the indicator has unit population variance.
#' Scores are reported on a continuous rating metric (population mean 4, SD 1)
#' and rounded to two decimals; the linear transformation does not affect
#' correlations or standardized estimates.
#'
#' @source Simulated with a fixed seed by `data-raw/nomo_demo.R` in the package
#'   source repository.
#'
#' @seealso [nomo_demo_ordinal] for five-category ordered versions of the same
#'   latent responses and [nomo_demo_network] for a multi-construct validation
#'   study.
#'
#' @examples
#' str(nomo_demo_continuous)
#' colSums(is.na(nomo_demo_continuous))
#'
#' scr <- nomo_screen(nomo_demo_continuous)
#' scr
"nomo_demo_continuous"


#' Simulated five-category ordered item data
#'
#' An ordered-categorical version of [nomo_demo_continuous]. The same latent
#' item responses (before missing values were introduced) were cut into five
#' ordered response categories, as is typical of Likert-type rating scales.
#' This dataset supports polychoric factor-retention, ordinal EFA, WLSMV CFA,
#' and ordinal reliability examples.
#'
#' @format A data frame with 500 rows and 10 ordered factors with levels
#'   `"1"` < `"2"` < `"3"` < `"4"` < `"5"`:
#' \describe{
#'   \item{a1, a2, a3, a4, a5}{Ordered indicators written for factor A
#'     (`a5` cross-loads on factor B).}
#'   \item{b1, b2, b3, b4, b5}{Ordered indicators written for factor B
#'     (`b5` is weak).}
#' }
#'
#' @details
#' Latent responses were thresholded at -1.80, -0.80, 0.20, and 1.20, so the
#' observed distributions lean toward the upper categories. The dataset has no
#' missing values, which keeps the first ordinal examples focused on estimator
#' and correlation choices rather than on missing-data handling for categorical
#' models.
#'
#' @source Simulated with a fixed seed by `data-raw/nomo_demo.R` in the package
#'   source repository.
#'
#' @seealso [nomo_demo_continuous], [nomo_demo_network]
#'
#' @examples
#' str(nomo_demo_ordinal)
#' table(nomo_demo_ordinal$a1)
"nomo_demo_ordinal"


#' Simulated multi-construct validation study
#'
#' A simulated construct-validation dataset for confirmatory measurement,
#' measurement-invariance, and theory-specified nomological-network examples.
#' Three latent constructs are measured by multiple indicators, an observed
#' outcome is available, and responses were collected in two administration
#' groups.
#'
#' @format A data frame with 800 rows and 13 columns:
#' \describe{
#'   \item{ag1, ag2, ag3, ag4}{Agency indicators. Population standardized
#'     loadings .80, .75, .70, .78.}
#'   \item{pe1, pe2, pe3, pe4}{Persistence indicators. Population standardized
#'     loadings .78, .72, .76, .70.}
#'   \item{sd1, sd2, sd3}{Social desirability indicators. Population
#'     standardized loadings .70, .75, .65.}
#'   \item{Performance}{Observed outcome score (population mean 70, SD 10).}
#'   \item{group}{Administration group: a factor with levels `"online"` and
#'     `"paper"` (400 cases each).}
#' }
#'
#' @details
#' Within each group, the population structural model is:
#'
#' * Persistence is regressed on Agency with a standardized coefficient of .45;
#' * Performance is regressed on Agency (.40) and **not** on Persistence (0);
#' * Agency and Social desirability are uncorrelated (0).
#'
#' Because Persistence is correlated with Agency, Persistence and Performance
#' are still correlated marginally (about .18 in the population) even though the
#' direct Persistence-to-Performance path is zero. This makes the dataset useful
#' for teaching the difference between a marginal association and a
#' theory-specified structural path.
#'
#' Measurement-invariance teaching features: all loadings are equal across
#' groups, the latent Agency mean is .25 SD higher in the `paper` group, and the
#' intercept of `ag3` is .50 higher in the `paper` group. The `ag3` intercept is
#' therefore a known source of scalar non-invariance. Indicators are reported
#' on a continuous rating metric (population mean 4, SD 1 in the `online`
#' group) and rounded to two decimals.
#'
#' @source Simulated with a fixed seed by `data-raw/nomo_demo.R` in the package
#'   source repository.
#'
#' @seealso [nomo_network()], [nomo_invariance()], [nomo_run()]
#'
#' @examples
#' str(nomo_demo_network)
#' table(nomo_demo_network$group)
"nomo_demo_network"
