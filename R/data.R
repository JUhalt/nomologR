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

#' Shared walkthrough data from content review to empirical screening
#'
#' Simulated responses of 400 students to twelve candidate items for a two-facet
#' Study Persistence scale. These are the shared teaching data of the joint
#' walkthrough with `contentvalidR`: the same twelve items went through an
#' expert item sort there, and the ten it carried forward are screened here.
#' The data are simulated. No participant was involved, and the construct, its
#' facets, and every item stem were written for the example.
#'
#' Each item was built to behave in a particular way at one stage or both, so
#' the two stages can be seen to disagree. `nomo_demo_walkthrough_items` states
#' what each item was built to do, in its `role` column:
#'
#' * `EF1`, `TF1`, `TF3`: ordinary items, which both stages accept.
#' * `EF2`, `TF2`: reverse-worded. A high answer means less persistence, so
#'   until they are recoded (6 minus the response) each correlates negatively
#'   with its own facet. That is a coding matter, not evidence against the item.
#' * `EF3`: endorsed by almost everyone, so its answers pile up at the top of
#'   the scale and every correlation it has is attenuated. An empirical screen
#'   flags it; it is also the only item about finishing required work, so
#'   dropping it narrows the domain the panel defined.
#' * `EF4`: passes content review (18 of 20 judges) and carries almost no common
#'   variance. Content review cannot see this.
#' * `EF5`, `TF5`: fail content review, so a handoff holds them back. They stay
#'   in the data so a reader can see what keeping them would have done.
#' * `EF6`: meets the content criterion by one judge, then behaves well.
#' * `TF4`: loads on both facets.
#' * `TF6`: shifted upward in cohort B (a classroom rule about phones), a known
#'   source of non-invariance between cohorts.
#'
#' @format `nomo_demo_walkthrough` is a data frame with 400 rows and 14 columns:
#' \describe{
#'   \item{respondent}{Respondent number, 1 to 400.}
#'   \item{cohort}{A factor with levels `"A"` and `"B"`, 200 respondents each.}
#'   \item{EF1, EF2, EF3, EF4, EF5, EF6}{Effort Regulation items, integer
#'     responses 1 to 5, as answered (`EF2` is not recoded).}
#'   \item{TF1, TF2, TF3, TF4, TF5, TF6}{Task Focus items, integer responses 1
#'     to 5, as answered (`TF2` is not recoded).}
#' }
#'
#' `nomo_demo_walkthrough_items` is a data frame with 12 rows and 5 columns:
#' `item`; `facet` (`"EF"` or `"TF"`); `stem`, the item's wording;
#' `reverse_worded`, a logical; and `role`, what the item was built to do.
#'
#' @details
#' Population model, per respondent:
#'
#' * Two facets, Effort Regulation and Task Focus, standard normal with
#'   correlation .45.
#' * Each item's latent response is its loadings times the facets plus normal
#'   error, scaled to unit variance, and cut into five categories at the normal
#'   quantiles .10, .30, .60, and .85.
#' * Standardized loadings on EF: `EF1` .72, `EF2` .68, `EF3` .60, `EF4` .15,
#'   `EF5` .55, `EF6` .60, and `TF4` .45.
#' * Loadings on TF: `TF1` .70, `TF2` .66, `TF3` .62, `TF4` .40, `TF5` .58,
#'   `TF6` .64.
#' * `EF3`'s latent response is shifted up by 1.75, and `TF6`'s by .35 in
#'   cohort B. `EF2` and `TF2` are answered in the opposite direction
#'   (6 minus the category).
#'
#' The content-review half of the example, the expert sort of these items, and
#' the handoff it produced are in `contentvalidR`. The handoff for these items is
#' stored with this package as
#' `system.file("extdata", "content-handoff-walkthrough.rds", package = "nomologR")`.
#'
#' @source Generated by `contentvalidR`'s `data-raw/build-walkthrough-data.R`
#'   (seed 20260921), copied unchanged from its v0.9.0 release into this
#'   package's `data-raw/walkthrough/`, and converted by
#'   `data-raw/nomo_demo_walkthrough.R`.
#'
#' @seealso [nomo_screen()], [nomo_run()], and `vignette("content-review",
#'   package = "nomologR")`.
#'
#' @examples
#' str(nomo_demo_walkthrough)
#' nomo_demo_walkthrough_items[, c("item", "role")]
#'
#' # Answered as written, a reverse-worded item runs against its facet.
#' cor(nomo_demo_walkthrough$EF1, nomo_demo_walkthrough$EF2)
#' cor(nomo_demo_walkthrough$EF1, 6 - nomo_demo_walkthrough$EF2)
"nomo_demo_walkthrough"


#' @rdname nomo_demo_walkthrough
"nomo_demo_walkthrough_items"
