# Research basis: from historical to contemporary practice

## Purpose

Scale development has a long methodological history. Many techniques
that graduate students meet in published articles were reasonable
responses to the computing and theory of their time, and several have
since been refined or replaced. This article maps each `nomologR`
workflow stage from **historical practice** to **contemporary
practice**, states **what `nomologR` does**, and notes what is
**planned**.

How to read the labels:

- **Historical practice** — widely used earlier and still common in
  published work. `nomologR` shows these methods as context when
  learners need to recognize them, labels them, and never silently
  substitutes them for contemporary evidence. “Historical” is not the
  same as “always wrong.”
- **Contemporary practice** — what the current methodological literature
  recommends. These methods provide `nomologR`’s primary evidence.
- **Planned** — work accepted for a future release, linked to its issue.

Full references appear at the end of this article and in each function’s
help page.

## The methods registry

The same mapping is available as data.
[`nomo_methods()`](https://juhalt.github.io/nomologR/reference/nomo_methods.md)
returns one row per method the package implements, with its stage,
lineage, role, estimand, key assumptions, implementing function,
computational engine, and references.

``` r

methods <- nomo_methods()
nrow(methods)
#> [1] 65
table(methods$stage, methods$lineage)
#>              
#>               contemporary emerging historical
#>   cfa                    8        0          3
#>   compare                6        0          0
#>   efa                    3        0          2
#>   factors                7        1          4
#>   invariance             6        0          1
#>   network                4        0          1
#>   reliability            3        0          1
#>   screen                 4        0          1
#>   validity               3        0          2
#>   workflow               5        0          0
```

The `role` column records how the evidence is used. A **context** method
is shown so that a reader can recognize it in published work, and it
takes no part in any synthesis or decision:

``` r

nomo_methods(lineage = "historical")[, c("stage", "method", "role")]
#> # A tibble: 15 × 3
#>    stage       method                                               role      
#>    <chr>       <chr>                                                <chr>     
#>  1 screen      Fixed item-total correlation reference (about .30)   context   
#>  2 factors     Eigenvalue-greater-than-one rule                     context   
#>  3 factors     Scree test                                           context   
#>  4 factors     Kaiser-Meyer-Olkin sampling adequacy                 supporting
#>  5 factors     Bartlett's test of sphericity                        supporting
#>  6 efa         Orthogonal (varimax) rotation                        context   
#>  7 efa         Fixed loading cutoff                                 context   
#>  8 cfa         Chi-square exact-fit test                            supporting
#>  9 cfa         Fixed fit-index cutoffs                              context   
#> 10 cfa         Modification indices                                 context   
#> 11 reliability Coefficient alpha                                    supporting
#> 12 validity    Standardized loadings and average variance extracted supporting
#> 13 validity    Fornell-Larcker comparison                           context   
#> 14 invariance  Fixed change-in-CFI rule                             context   
#> 15 network     Nomological network of construct relations           primary
```

The registry lists only what the package actually computes. Methods that
are planned but not yet implemented appear in this article and in the
issue tracker, not in the registry, so that
[`nomo_methods()`](https://juhalt.github.io/nomologR/reference/nomo_methods.md)
never describes a capability the package lacks.

Given a workflow object, `nomo_methods(run)` returns only the methods
that workflow used. For any selection, `references = TRUE` expands the
result into a reference list:

``` r

nomo_methods(stage = "reliability", references = TRUE)[, c("method", "citation")]
#> # A tibble: 8 × 2
#>   method                                         citation                       
#>   <chr>                                          <chr>                          
#> 1 Model-based coefficient omega                  Dunn, T. J., Baguley, T., & Br…
#> 2 Model-based coefficient omega                  McNeish, D. (2018). Thanks coe…
#> 3 Model-based coefficient omega                  Flora, D. B. (2020). Your coef…
#> 4 Model-based coefficient omega                  Bell, S. M., Chalmers, R. P., …
#> 5 Reliability on the ordered-score scale         Green, S. B., & Yang, Y. (2009…
#> 6 Coefficient alpha                              Cronbach, L. J. (1951). Coeffi…
#> 7 Coefficient alpha                              Sijtsma, K. (2009). On the use…
#> 8 Bootstrap confidence intervals for reliability Kelley, K., & Pornprasertmanit…
```

This is what the **Methods and citations** section of
[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
is built from, so an archived report cites the methods a run used rather
than every method the package knows about. Every DOI in the registry is
checked against the DOI registries before each release, including that
the registered author, year, and title match the citation.

## 1. Item and data audit — `nomo_screen()`

**Historical practice.** Item analysis often meant screening corrected
item–total correlations against a fixed value (a reference near .30 is
widely taught; see Nunnally & Bernstein, 1994) and deleting items that
fell below it, with little attention to response distributions, missing
data, or response quality.

**Contemporary practice.** Item statistics are one strand of evidence,
interpreted alongside content, construct definition, and later
structural evidence (Clark & Watson, 2019). Audits describe missingness,
category use, and response concentration before modeling. Case-level
screening for careless or insufficient-effort responding uses several
complementary indices rather than a single exclusion rule (Meade &
Craig, 2012; Huang et al., 2012; Curran, 2016).

**What nomologR does.** Reports item and case missingness, response
distributions, unused categories, response concentration, near-zero
variance (using heuristics described by Kuhn & Johnson, 2013), corrected
item–rest and inter-item correlations, and descriptive shape statistics.
Flags are review prompts; no item or case is removed or recoded.

**Planned.** Insufficient-effort responding indices
([\#34](https://github.com/JUhalt/nomologR/issues/34)).

## 2. Dimensionality — `nomo_factors()`

**Historical practice.** Retaining factors with eigenvalues greater than
one (Guttman, 1954; Kaiser, 1960) and visually inspecting the scree plot
(Cattell, 1966). The “Little Jiffy” routine — principal components,
eigenvalues greater than one, and varimax rotation — became a default in
software (discussed by Kaiser, 1970).

**Contemporary practice.** Simulation research favors parallel analysis
(Horn, 1965; Crawford et al., 2010) and Velicer’s minimum average
partial criterion (Velicer, 1976; Velicer et al., 2000), with newer
criteria such as the empirical Kaiser criterion (Braeken & van Assen,
2017), comparison data (Ruscio & Roche, 2012), NEST (Achim, 2017), and
the Hull method (Lorenzo-Seva et al., 2011). Criteria are triangulated,
and the correlation model matches the measurement level of the items.

**What nomologR does.** Common-factor parallel analysis is the primary
evidence, complemented by original and revised MAP and EKC; NEST, Hull,
and comparison data are optional. The eigenvalue-greater-than-one rule
is displayed only as labeled historical context and is excluded from the
synthesis. KMO (Kaiser, 1974) and Bartlett’s test (Bartlett, 1950) are
supporting diagnostics, not decision rules. Pearson, polychoric,
tetrachoric, or mixed correlations are chosen explicitly.

## 3. Exploratory structure — `nomo_efa()`

**Historical practice.** Principal components analysis reported as
“factor analysis,” orthogonal varimax rotation by default (Kaiser,
1958), and deleting items below loading cutoffs. Reviews documented how
common these choices were (Fabrigar et al., 1999; Conway & Huffcutt,
2003).

**Contemporary practice.** Common-factor extraction, oblique rotation
whenever factors may plausibly correlate (Browne, 2001; Fabrigar et al.,
1999), attention to cross-loadings, communalities, and residual
correlations, and item decisions that combine numerical evidence with
content (Costello & Osborne, 2005; Watkins, 2018).

**What nomologR does.** MINRES common-factor extraction with oblimin
rotation by default; pattern and structure matrices; communalities;
cross-loading and residual diagnostics; `KEEP`, `REVIEW`, or
`STRONG REVIEW` item guidance with explanations. Orthogonal rotation is
allowed but recorded as a choice that needs justification.

## 4. Confirmatory measurement model — `nomo_cfa()`

**Historical practice.** Treating the chi-square exact-fit test as the
sole criterion (Jöreskog, 1969); judging incremental fit indices against
.90 (Bentler & Bonett, 1980); later, treating the cutoffs of Hu and
Bentler (1999) — CFI and TLI near .95, RMSEA near .06, SRMR near .08 —
as universal golden rules; and freeing parameters suggested by
modification indices until a model “fits.”

**Contemporary practice.** Fit indices are interpreted in the context of
model size, estimator, sample size, and indicator type rather than as
universal rules (Marsh et al., 2004). RMSEA is reported with its
confidence interval (Browne & Cudeck, 1992). Local strain in residuals
is inspected. Estimators match the indicators — for example WLSMV for
ordered categories (Flora & Curran, 2004; Rhemtulla et al., 2012).
Data-driven respecification is recognized as capitalization on chance
(MacCallum et al., 1992). Improper solutions such as Heywood cases are
treated as possible signs of misspecification (Kolenikov & Bollen,
2012). Model-specific (“dynamic”) fit-index cutoffs are an emerging
alternative to fixed thresholds (McNeish & Wolf, 2023).

**What nomologR does.** Retains the unchanged `lavaan` fit (Rosseel,
2012) and reports chi-square, CFI (Bentler, 1990), TLI, RMSEA with its
interval, and SRMR against teaching references; localized residual
correlations; Heywood and convergence diagnostics; and modification
indices quarantined as post-hoc diagnostics that never change the model.
Declared ordered indicators request WLSMV.
[`nomo_split()`](https://juhalt.github.io/nomologR/reference/nomo_split.md)
supports calibration and validation samples because evaluating an
exploratory solution on the same data it came from overstates
confirmation (Fokkema & Greiff, 2017).

**Comparing models.** Historically, competing models were compared by
subtracting chi-square values and degrees of freedom, even for robust
estimators, or by applying one fixed change-in-fit cutoff. Contemporary
practice uses the difference test that matches the estimator — scaled
differences for robust ML (Satorra & Bentler, 2001, 2010) and
scaled-and-shifted tests for categorical estimators (Satorra, 2000) —
verifies nesting rather than assuming it (Bentler & Satorra, 2010),
reports several changes in fit (Cheung & Rensvold, 2002; Chen, 2007),
and compares non-nested models of the same data with information
criteria (Akaike, 1974; Schwarz, 1978; Raftery, 1995; Burnham &
Anderson, 2004).
[`nomo_compare()`](https://juhalt.github.io/nomologR/reference/nomo_compare.md)
implements this: it requires a recorded rationale, labels post-hoc
comparisons, refuses comparisons across different estimators, cases, or
data, and never selects a model automatically.

**Revising a model.** Historically, a model that fit poorly was
respecified in place, and the published model was reported as though it
had been the plan.
[`nomo_revise()`](https://juhalt.github.io/nomologR/reference/nomo_revise.md)
instead creates a child workflow from the parent run, records what
changed, the researcher’s rationale, and whether the change was
prespecified or post hoc, compares the parent and revised models, and
recommends confirming a post-hoc revision in independent data.

**Planned.** Model-specific fit cutoffs remain a research proposal
([\#23](https://github.com/JUhalt/nomologR/issues/23)).

## 5. Reliability — `nomo_reliability()`

**Historical practice.** Reporting coefficient alpha (Cronbach, 1951) as
“the” reliability of a scale regardless of whether its assumptions hold.

**Contemporary practice.** Model-based omega for congeneric measurement
(Dunn et al., 2014; McNeish, 2018; Flora, 2020), recognition of alpha’s
limits (Sijtsma, 2009), reliability defined on the score scale actually
used for ordered items (Green & Yang, 2009), interval estimates (Kelley
& Pornprasertmanit, 2016), and awareness that measurement
misspecification biases omega (Bell et al., 2024).

**What nomologR does.** Model-based omega from `semTools` is primary;
alpha is a qualified secondary statistic and is reported as unavailable
rather than silently redefined for ordered-score estimands; bootstrap
intervals are optional.

**Planned.** Omega hierarchical, explained common variance, and related
indices for bifactor and higher-order models
([\#29](https://github.com/JUhalt/nomologR/issues/29)).

## 6. Convergent and discriminant evidence — `nomo_validity()`

**Historical practice.** Multitrait–multimethod correlation matrices
(Campbell & Fiske, 1959); later, average variance extracted above .50
and the Fornell–Larcker comparison of AVE with squared correlations
(Fornell & Larcker, 1981).

**Contemporary practice.** Simulation work shows that the
Fornell–Larcker comparison can miss discriminant-validity problems
(Henseler et al., 2015; Voorhees et al., 2016). The
heterotrait–monotrait ratio (Henseler et al., 2015) and its
congeneric-appropriate successor HTMT2 (Roemer et al., 2021) are
recommended, together with latent factor correlations and their
confidence intervals (Rönkkö & Cho, 2022).

**What nomologR does.** Standardized loadings and AVE as convergent
evidence (never as reliability); HTMT2 as primary construct-separation
evidence with original HTMT for comparison; latent correlations with
uncertainty; the Fornell–Larcker table only on explicit request, labeled
legacy/supporting.

## 7. Measurement invariance — `nomo_invariance()`, `nomo_partial()`

**Historical practice.** Multiple-group factor analysis (Jöreskog, 1971)
and the configural–metric–scalar–strict hierarchy (Meredith, 1993;
Vandenberg & Lance, 2000), evaluated with chi-square difference tests
alone or a single fixed rule such as a CFI decrease of .01 (Cheung &
Rensvold, 2002); ordinal items analyzed as if continuous.

**Contemporary practice.** Several change-in-fit indices interpreted
with sample size and model context (Chen, 2007; Putnick & Bornstein,
2016); identification-aware sequences for ordered-categorical indicators
(Wu & Estabrook, 2016; Svetina et al., 2020); partial invariance based
on substantively justified, transparently reported releases (Byrne et
al., 1989).

**What nomologR does.** Category-aware sequences for continuous, binary,
three-category, and four-or-more-category indicators; fit and change
evidence without a universal pass/fail rule; score-test diagnostics that
locate strain but never free parameters; researcher-specified releases
with required rationales carried forward to more restrictive levels.

**Planned.** Longitudinal invariance is a candidate for v0.3 scope
selection ([\#38](https://github.com/JUhalt/nomologR/issues/38)).

## 8. Nomological network — `nomo_hypotheses()`, `nomo_network()`

**Historical practice.** Construct validity articulated as a nomological
network of lawful relations (Cronbach & Meehl, 1955), in practice often
examined through tables of observed correlations and significance tests,
with a non-significant result taken as evidence of “no relation.”

**Contemporary practice.** Establish the measurement model before
interpreting structural relations (Anderson & Gerbing, 1988); treat
validity as an integrated argument about the meaning of scores (Messick,
1995); test negligible predictions with equivalence procedures and a
smallest effect size of interest rather than `p > .05` (Schuirmann,
1987; Lakens et al., 2018); and distinguish prespecified from post-hoc
predictions (Nosek et al., 2018).

**What nomologR does.** Machine-readable directional, magnitude, and
negligible predictions; researcher-specified equivalence regions (the
package never invents one); latent SEM with observed outcomes;
measurement context kept alongside theory concordance; a-priori versus
post-hoc provenance; and replication of the exact same model in
validation data. A change in sign between samples is called a reversal
only when both confidence intervals exclude zero on opposite sides;
otherwise it is reported as a direction that did not replicate, or as a
sign change within sampling uncertainty, because estimates scattered
around a null relation differ in sign about half the time.

## 9. Workflow, scores, and reporting — `nomo_run()`, `nomo_report()`

**Historical practice.** A sequence of undisclosed decisions — dropping
items, adding residual covariances, trying estimators — reported as if
it had been the plan all along; unit-weighted sum scores assumed to
represent the construct.

**Contemporary practice.** Transparent reporting of measurement
decisions and their justification (Flake et al., 2017; Flake & Fried,
2020), awareness of researcher degrees of freedom (Simmons et al., 2011;
Wicherts et al., 2016), staged scale-development guidance (Clark &
Watson, 1995, 2019; Hinkin, 1998; Boateng et al., 2018), missing-data
methods such as full-information maximum likelihood (Enders & Bandalos,
2001; Schafer & Graham, 2002), and checking whether sum scores are
consistent with the measurement model before using them (Grice, 2001;
McNeish & Wolf, 2020).

**What nomologR does.**
[`nomo_run()`](https://juhalt.github.io/nomologR/reference/nomo_run.md)
pauses at consequential decisions and records the decision and
rationale;
[`nomo_report()`](https://juhalt.github.io/nomologR/reference/nomo_report.md)
archives methods, evidence, decisions, deviations, and session
information.
[`nomo_methods()`](https://juhalt.github.io/nomologR/reference/nomo_methods.md)
identifies which methods a workflow actually used, so the report cites
those methods and their literature rather than only the software that
computed them.

**Planned.** Missing-data sensitivity
([\#32](https://github.com/JUhalt/nomologR/issues/32)), score guidance
([\#33](https://github.com/JUhalt/nomologR/issues/33)), and
manuscript-ready tables
([\#35](https://github.com/JUhalt/nomologR/issues/35)).

## References

Every DOI below was verified against Crossref or the DOI registry.

Achim, A. (2017). Testing the number of required dimensions in
exploratory factor analysis. *The Quantitative Methods for Psychology,
13*(1), 64–74. <https://doi.org/10.20982/tqmp.13.1.p064>

Akaike, H. (1974). A new look at the statistical model identification.
*IEEE Transactions on Automatic Control, 19*(6), 716–723.
<https://doi.org/10.1109/TAC.1974.1100705>

Anderson, J. C., & Gerbing, D. W. (1988). Structural equation modeling
in practice: A review and recommended two-step approach. *Psychological
Bulletin, 103*(3), 411–423.
<https://doi.org/10.1037/0033-2909.103.3.411>

Bartlett, M. S. (1950). Tests of significance in factor analysis.
*British Journal of Statistical Psychology, 3*(2), 77–85.
<https://doi.org/10.1111/j.2044-8317.1950.tb00285.x>

Bell, S. M., Chalmers, R. P., & Flora, D. B. (2024). The impact of
measurement model misspecification on coefficient omega estimates of
composite reliability. *Educational and Psychological Measurement,
84*(1), 5–39. <https://doi.org/10.1177/00131644231155804>

Bentler, P. M. (1990). Comparative fit indexes in structural models.
*Psychological Bulletin, 107*(2), 238–246.
<https://doi.org/10.1037/0033-2909.107.2.238>

Bentler, P. M., & Bonett, D. G. (1980). Significance tests and goodness
of fit in the analysis of covariance structures. *Psychological
Bulletin, 88*(3), 588–606. <https://doi.org/10.1037/0033-2909.88.3.588>

Bentler, P. M., & Satorra, A. (2010). Testing model nesting and
equivalence. *Psychological Methods, 15*(2), 111–123.
<https://doi.org/10.1037/a0019625>

Boateng, G. O., Neilands, T. B., Frongillo, E. A., Melgar-Quiñonez, H.
R., & Young, S. L. (2018). Best practices for developing and validating
scales for health, social, and behavioral research: A primer. *Frontiers
in Public Health, 6*, 149. <https://doi.org/10.3389/fpubh.2018.00149>

Braeken, J., & van Assen, M. A. L. M. (2017). An empirical Kaiser
criterion. *Psychological Methods, 22*(3), 450–466.
<https://doi.org/10.1037/met0000074>

Browne, M. W. (2001). An overview of analytic rotation in exploratory
factor analysis. *Multivariate Behavioral Research, 36*(1), 111–150.
<https://doi.org/10.1207/S15327906MBR3601_05>

Browne, M. W., & Cudeck, R. (1992). Alternative ways of assessing model
fit. *Sociological Methods & Research, 21*(2), 230–258.
<https://doi.org/10.1177/0049124192021002005>

Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference:
Understanding AIC and BIC in model selection. *Sociological Methods &
Research, 33*(2), 261–304. <https://doi.org/10.1177/0049124104268644>

Byrne, B. M., Shavelson, R. J., & Muthén, B. (1989). Testing for the
equivalence of factor covariance and mean structures: The issue of
partial measurement invariance. *Psychological Bulletin, 105*(3),
456–466. <https://doi.org/10.1037/0033-2909.105.3.456>

Campbell, D. T., & Fiske, D. W. (1959). Convergent and discriminant
validation by the multitrait-multimethod matrix. *Psychological
Bulletin, 56*(2), 81–105. <https://doi.org/10.1037/h0046016>

Cattell, R. B. (1966). The scree test for the number of factors.
*Multivariate Behavioral Research, 1*(2), 245–276.
<https://doi.org/10.1207/s15327906mbr0102_10>

Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of
measurement invariance. *Structural Equation Modeling, 14*(3), 464–504.
<https://doi.org/10.1080/10705510701301834>

Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit
indexes for testing measurement invariance. *Structural Equation
Modeling, 9*(2), 233–255. <https://doi.org/10.1207/S15328007SEM0902_5>

Clark, L. A., & Watson, D. (1995). Constructing validity: Basic issues
in objective scale development. *Psychological Assessment, 7*(3),
309–319. <https://doi.org/10.1037/1040-3590.7.3.309>

Clark, L. A., & Watson, D. (2019). Constructing validity: New
developments in creating objective measuring instruments. *Psychological
Assessment, 31*(12), 1412–1427. <https://doi.org/10.1037/pas0000626>

Conway, J. M., & Huffcutt, A. I. (2003). A review and evaluation of
exploratory factor analysis practices in organizational research.
*Organizational Research Methods, 6*(2), 147–168.
<https://doi.org/10.1177/1094428103251541>

Costello, A. B., & Osborne, J. W. (2005). Best practices in exploratory
factor analysis: Four recommendations for getting the most from your
analysis. *Practical Assessment, Research, and Evaluation, 10*, Article
7. <https://doi.org/10.7275/jyj1-4868>

Crawford, A. V., Green, S. B., Levy, R., Lo, W.-J., Scott, L., Svetina,
D., & Thompson, M. S. (2010). Evaluation of parallel analysis methods
for determining the number of factors. *Educational and Psychological
Measurement, 70*(6), 885–901. <https://doi.org/10.1177/0013164410379332>

Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
tests. *Psychometrika, 16*(3), 297–334.
<https://doi.org/10.1007/BF02310555>

Cronbach, L. J., & Meehl, P. E. (1955). Construct validity in
psychological tests. *Psychological Bulletin, 52*(4), 281–302.
<https://doi.org/10.1037/h0040957>

Curran, P. G. (2016). Methods for the detection of carelessly invalid
responses in survey data. *Journal of Experimental Social Psychology,
66*, 4–19. <https://doi.org/10.1016/j.jesp.2015.07.006>

Dunn, T. J., Baguley, T., & Brunsden, V. (2014). From alpha to omega: A
practical solution to the pervasive problem of internal consistency
estimation. *British Journal of Psychology, 105*(3), 399–412.
<https://doi.org/10.1111/bjop.12046>

Enders, C. K., & Bandalos, D. L. (2001). The relative performance of
full information maximum likelihood estimation for missing data in
structural equation models. *Structural Equation Modeling, 8*(3),
430–457. <https://doi.org/10.1207/S15328007SEM0803_5>

Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J.
(1999). Evaluating the use of exploratory factor analysis in
psychological research. *Psychological Methods, 4*(3), 272–299.
<https://doi.org/10.1037/1082-989X.4.3.272>

Flake, J. K., & Fried, E. I. (2020). Measurement schmeasurement:
Questionable measurement practices and how to avoid them. *Advances in
Methods and Practices in Psychological Science, 3*(4), 456–465.
<https://doi.org/10.1177/2515245920952393>

Flake, J. K., Pek, J., & Hehman, E. (2017). Construct validation in
social and personality research: Current practice and recommendations.
*Social Psychological and Personality Science, 8*(4), 370–378.
<https://doi.org/10.1177/1948550617693063>

Flora, D. B. (2020). Your coefficient alpha is probably wrong, but which
coefficient omega is right? A tutorial on using R to obtain better
reliability estimates. *Advances in Methods and Practices in
Psychological Science, 3*(4), 484–501.
<https://doi.org/10.1177/2515245920951747>

Flora, D. B., & Curran, P. J. (2004). An empirical evaluation of
alternative methods of estimation for confirmatory factor analysis with
ordinal data. *Psychological Methods, 9*(4), 466–491.
<https://doi.org/10.1037/1082-989X.9.4.466>

Fokkema, M., & Greiff, S. (2017). How performing PCA and CFA on the same
data equals trouble. *European Journal of Psychological Assessment,
33*(6), 399–402. <https://doi.org/10.1027/1015-5759/a000460>

Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation
models with unobservable variables and measurement error. *Journal of
Marketing Research, 18*(1), 39–50. <https://doi.org/10.2307/3151312>

Green, S. B., & Yang, Y. (2009). Reliability of summed item scores using
structural equation modeling: An alternative to coefficient alpha.
*Psychometrika, 74*(1), 155–167.
<https://doi.org/10.1007/s11336-008-9099-3>

Grice, J. W. (2001). Computing and evaluating factor scores.
*Psychological Methods, 6*(4), 430–450.
<https://doi.org/10.1037/1082-989X.6.4.430>

Guttman, L. (1954). Some necessary conditions for common-factor
analysis. *Psychometrika, 19*(2), 149–161.
<https://doi.org/10.1007/BF02289162>

Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for
assessing discriminant validity in variance-based structural equation
modeling. *Journal of the Academy of Marketing Science, 43*(1), 115–135.
<https://doi.org/10.1007/s11747-014-0403-8>

Hinkin, T. R. (1998). A brief tutorial on the development of measures
for use in survey questionnaires. *Organizational Research Methods,
1*(1), 104–121. <https://doi.org/10.1177/109442819800100106>

Horn, J. L. (1965). A rationale and test for the number of factors in
factor analysis. *Psychometrika, 30*(2), 179–185.
<https://doi.org/10.1007/BF02289447>

Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in
covariance structure analysis: Conventional criteria versus new
alternatives. *Structural Equation Modeling, 6*(1), 1–55.
<https://doi.org/10.1080/10705519909540118>

Huang, J. L., Curran, P. G., Keeney, J., Poposki, E. M., & DeShon, R. P.
(2012). Detecting and deterring insufficient effort responding to
surveys. *Journal of Business and Psychology, 27*(1), 99–114.
<https://doi.org/10.1007/s10869-011-9231-8>

Jöreskog, K. G. (1969). A general approach to confirmatory maximum
likelihood factor analysis. *Psychometrika, 34*(2), 183–202.
<https://doi.org/10.1007/BF02289343>

Jöreskog, K. G. (1971). Simultaneous factor analysis in several
populations. *Psychometrika, 36*(4), 409–426.
<https://doi.org/10.1007/BF02291366>

Kaiser, H. F. (1958). The varimax criterion for analytic rotation in
factor analysis. *Psychometrika, 23*(3), 187–200.
<https://doi.org/10.1007/BF02289233>

Kaiser, H. F. (1960). The application of electronic computers to factor
analysis. *Educational and Psychological Measurement, 20*(1), 141–151.
<https://doi.org/10.1177/001316446002000116>

Kaiser, H. F. (1970). A second generation little jiffy. *Psychometrika,
35*(4), 401–415. <https://doi.org/10.1007/BF02291817>

Kaiser, H. F. (1974). An index of factorial simplicity. *Psychometrika,
39*(1), 31–36. <https://doi.org/10.1007/BF02291575>

Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for
population reliability coefficients: Evaluation of methods,
recommendations, and software for composite measures. *Psychological
Methods, 21*(1), 69–92. <https://doi.org/10.1037/a0040086>

Kolenikov, S., & Bollen, K. A. (2012). Testing negative error variances:
Is a Heywood case a symptom of misspecification? *Sociological Methods &
Research, 41*(1), 124–167. <https://doi.org/10.1177/0049124112442138>

Kuhn, M., & Johnson, K. (2013). *Applied predictive modeling*. Springer.
<https://doi.org/10.1007/978-1-4614-6849-3>

Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing
for psychological research: A tutorial. *Advances in Methods and
Practices in Psychological Science, 1*(2), 259–269.
<https://doi.org/10.1177/2515245918770963>

Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. L. (2011). The Hull
method for selecting the number of common factors. *Multivariate
Behavioral Research, 46*(2), 340–364.
<https://doi.org/10.1080/00273171.2011.564527>

MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model
modifications in covariance structure analysis: The problem of
capitalization on chance. *Psychological Bulletin, 111*(3), 490–504.
<https://doi.org/10.1037/0033-2909.111.3.490>

Marsh, H. W., Hau, K.-T., & Wen, Z. (2004). In search of golden rules:
Comment on hypothesis-testing approaches to setting cutoff values for
fit indexes and dangers in overgeneralizing Hu and Bentler’s (1999)
findings. *Structural Equation Modeling, 11*(3), 320–341.
<https://doi.org/10.1207/s15328007sem1103_2>

McNeish, D. (2018). Thanks coefficient alpha, we’ll take it from here.
*Psychological Methods, 23*(3), 412–433.
<https://doi.org/10.1037/met0000144>

McNeish, D., & Wolf, M. G. (2020). Thinking twice about sum scores.
*Behavior Research Methods, 52*(6), 2287–2305.
<https://doi.org/10.3758/s13428-020-01398-0>

McNeish, D., & Wolf, M. G. (2023). Dynamic fit index cutoffs for
confirmatory factor analysis models. *Psychological Methods, 28*(1),
61–88. <https://doi.org/10.1037/met0000425>

Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in
survey data. *Psychological Methods, 17*(3), 437–455.
<https://doi.org/10.1037/a0028085>

Meredith, W. (1993). Measurement invariance, factor analysis and
factorial invariance. *Psychometrika, 58*(4), 525–543.
<https://doi.org/10.1007/BF02294825>

Messick, S. (1995). Validity of psychological assessment: Validation of
inferences from persons’ responses and performances as scientific
inquiry into score meaning. *American Psychologist, 50*(9), 741–749.
<https://doi.org/10.1037/0003-066X.50.9.741>

Nosek, B. A., Ebersole, C. R., DeHaven, A. C., & Mellor, D. T. (2018).
The preregistration revolution. *Proceedings of the National Academy of
Sciences, 115*(11), 2600–2606. <https://doi.org/10.1073/pnas.1708274114>

Nunnally, J. C., & Bernstein, I. H. (1994). *Psychometric theory* (3rd
ed.). McGraw-Hill.

Putnick, D. L., & Bornstein, M. H. (2016). Measurement invariance
conventions and reporting: The state of the art and future directions
for psychological research. *Developmental Review, 41*, 71–90.
<https://doi.org/10.1016/j.dr.2016.06.004>

Raftery, A. E. (1995). Bayesian model selection in social research.
*Sociological Methodology, 25*, 111–163.
<https://doi.org/10.2307/271063>

Rhemtulla, M., Brosseau-Liard, P. É., & Savalei, V. (2012). When can
categorical variables be treated as continuous? A comparison of robust
continuous and categorical SEM estimation methods under suboptimal
conditions. *Psychological Methods, 17*(3), 354–373.
<https://doi.org/10.1037/a0029315>

Roemer, E., Schuberth, F., & Henseler, J. (2021). HTMT2—An improved
criterion for assessing discriminant validity in structural equation
modeling. *Industrial Management & Data Systems, 121*(12), 2637–2650.
<https://doi.org/10.1108/IMDS-02-2021-0082>

Rönkkö, M., & Cho, E. (2022). An updated guideline for assessing
discriminant validity. *Organizational Research Methods, 25*(1).
<https://doi.org/10.1177/1094428120968614>

Rosseel, Y. (2012). lavaan: An R package for structural equation
modeling. *Journal of Statistical Software, 48*(2), 1–36.
<https://doi.org/10.18637/jss.v048.i02>

Ruscio, J., & Roche, B. (2012). Determining the number of factors to
retain in an exploratory factor analysis using comparison data of known
factorial structure. *Psychological Assessment, 24*(2), 282–292.
<https://doi.org/10.1037/a0025697>

Satorra, A. (2000). Scaled and adjusted restricted tests in multi-sample
analysis of moment structures. In *Innovations in multivariate
statistical analysis* (pp. 233–247). Springer.
<https://doi.org/10.1007/978-1-4615-4603-0_17>

Satorra, A., & Bentler, P. M. (2001). A scaled difference chi-square
test statistic for moment structure analysis. *Psychometrika, 66*(4),
507–514. <https://doi.org/10.1007/BF02296192>

Satorra, A., & Bentler, P. M. (2010). Ensuring positiveness of the
scaled difference chi-square test statistic. *Psychometrika, 75*(2),
243–248. <https://doi.org/10.1007/s11336-009-9135-y>

Schafer, J. L., & Graham, J. W. (2002). Missing data: Our view of the
state of the art. *Psychological Methods, 7*(2), 147–177.
<https://doi.org/10.1037/1082-989X.7.2.147>

Schuirmann, D. J. (1987). A comparison of the two one-sided tests
procedure and the power approach for assessing the equivalence of
average bioavailability. *Journal of Pharmacokinetics and
Biopharmaceutics, 15*(6), 657–680. <https://doi.org/10.1007/BF01068419>

Schwarz, G. (1978). Estimating the dimension of a model. *The Annals of
Statistics, 6*(2), 461–464. <https://doi.org/10.1214/aos/1176344136>

Sijtsma, K. (2009). On the use, the misuse, and the very limited
usefulness of Cronbach’s alpha. *Psychometrika, 74*(1), 107–120.
<https://doi.org/10.1007/s11336-008-9101-0>

Simmons, J. P., Nelson, L. D., & Simonsohn, U. (2011). False-positive
psychology: Undisclosed flexibility in data collection and analysis
allows presenting anything as significant. *Psychological Science,
22*(11), 1359–1366. <https://doi.org/10.1177/0956797611417632>

Svetina, D., Rutkowski, L., & Rutkowski, D. (2020). Multiple-group
invariance with categorical outcomes using updated guidelines: An
illustration using Mplus and the lavaan/semTools packages. *Structural
Equation Modeling, 27*(1), 111–130.
<https://doi.org/10.1080/10705511.2019.1602776>

Vandenberg, R. J., & Lance, C. E. (2000). A review and synthesis of the
measurement invariance literature: Suggestions, practices, and
recommendations for organizational research. *Organizational Research
Methods, 3*(1), 4–70. <https://doi.org/10.1177/109442810031002>

Velicer, W. F. (1976). Determining the number of components from the
matrix of partial correlations. *Psychometrika, 41*(3), 321–327.
<https://doi.org/10.1007/BF02293557>

Velicer, W. F., Eaton, C. A., & Fava, J. L. (2000). Construct
explication through factor or component analysis: A review and
evaluation of alternative procedures for determining the number of
factors or components. In R. D. Goffin & E. Helmes (Eds.), *Problems and
solutions in human assessment* (pp. 41–71). Springer.
<https://doi.org/10.1007/978-1-4615-4397-8_3>

Voorhees, C. M., Brady, M. K., Calantone, R., & Ramirez, E. (2016).
Discriminant validity testing in marketing: An analysis, causes for
concern, and proposed remedies. *Journal of the Academy of Marketing
Science, 44*(1), 119–134. <https://doi.org/10.1007/s11747-015-0455-4>

Watkins, M. W. (2018). Exploratory factor analysis: A guide to best
practice. *Journal of Black Psychology, 44*(3), 219–246.
<https://doi.org/10.1177/0095798418771807>

Wicherts, J. M., Veldkamp, C. L. S., Augusteijn, H. E. M., Bakker, M.,
van Aert, R. C. M., & van Assen, M. A. L. M. (2016). Degrees of freedom
in planning, running, analyzing, and reporting psychological studies: A
checklist to avoid p-hacking. *Frontiers in Psychology, 7*, 1832.
<https://doi.org/10.3389/fpsyg.2016.01832>

Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor
analysis models of different levels of invariance for ordered
categorical outcomes. *Psychometrika, 81*(4), 1014–1045.
<https://doi.org/10.1007/s11336-016-9506-0>
