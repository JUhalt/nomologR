# Methods registry -------------------------------------------------------------

# Bibliography ----------------------------------------------------------------
#
# One entry per work cited by the registry. `short` is the in-text form used in
# compact output; `citation` is the full reference. Author names that contain
# non-ASCII characters use \u escapes so the source file stays ASCII while the
# parsed string carries the correct spelling.
#
# Integrity is enforced in both directions by tests: every `citations` key in
# the registry must resolve here, and every entry here must be cited by at
# least one method. That keeps the two from drifting apart.

nomo_bib_entry <- function(key, short, citation, doi = NA_character_) {
  list(key = key, short = short, citation = citation, doi = doi)
}


nomo_bibliography <- function() {
  entries <- list(
    nomo_bib_entry(
      "achim_2017", "Achim (2017)",
      paste(
        "Achim, A. (2017). Testing the number of required dimensions in",
        "exploratory factor analysis. The Quantitative Methods for Psychology,",
        "13(1), 64-74."
      ),
      "10.20982/tqmp.13.1.p064"
    ),
    nomo_bib_entry(
      "akaike_1974", "Akaike (1974)",
      paste(
        "Akaike, H. (1974). A new look at the statistical model identification.",
        "IEEE Transactions on Automatic Control, 19(6), 716-723."
      ),
      "10.1109/TAC.1974.1100705"
    ),
    nomo_bib_entry(
      "anderson_gerbing_1988", "Anderson & Gerbing (1988)",
      paste(
        "Anderson, J. C., & Gerbing, D. W. (1988). Structural equation modeling",
        "in practice: A review and recommended two-step approach.",
        "Psychological Bulletin, 103(3), 411-423."
      ),
      "10.1037/0033-2909.103.3.411"
    ),
    nomo_bib_entry(
      "bartlett_1950", "Bartlett (1950)",
      paste(
        "Bartlett, M. S. (1950). Tests of significance in factor analysis.",
        "British Journal of Statistical Psychology, 3(2), 77-85."
      ),
      "10.1111/j.2044-8317.1950.tb00285.x"
    ),
    nomo_bib_entry(
      "beauducel_2011", "Beauducel (2011)",
      paste(
        "Beauducel, A. (2011). Indeterminacy of factor score estimates in",
        "slightly misspecified confirmatory factor models. Journal of Modern",
        "Applied Statistical Methods, 10(2), 583-598."
      ),
      "10.22237/jmasm/1320120900"
    ),
    nomo_bib_entry(
      "bell_2024", "Bell, Chalmers, & Flora (2024)",
      paste(
        "Bell, S. M., Chalmers, R. P., & Flora, D. B. (2024). The impact of",
        "measurement model misspecification on coefficient omega estimates of",
        "composite reliability. Educational and Psychological Measurement,",
        "84(1), 5-39."
      ),
      "10.1177/00131644231155804"
    ),
    nomo_bib_entry(
      "bentler_1990", "Bentler (1990)",
      paste(
        "Bentler, P. M. (1990). Comparative fit indexes in structural models.",
        "Psychological Bulletin, 107(2), 238-246."
      ),
      "10.1037/0033-2909.107.2.238"
    ),
    nomo_bib_entry(
      "bentler_bonett_1980", "Bentler & Bonett (1980)",
      paste(
        "Bentler, P. M., & Bonett, D. G. (1980). Significance tests and goodness",
        "of fit in the analysis of covariance structures. Psychological",
        "Bulletin, 88(3), 588-606."
      ),
      "10.1037/0033-2909.88.3.588"
    ),
    nomo_bib_entry(
      "bentler_satorra_2010", "Bentler & Satorra (2010)",
      paste(
        "Bentler, P. M., & Satorra, A. (2010). Testing model nesting and",
        "equivalence. Psychological Methods, 15(2), 111-123."
      ),
      "10.1037/a0019625"
    ),
    nomo_bib_entry(
      "boateng_2018", "Boateng et al. (2018)",
      paste(
        "Boateng, G. O., Neilands, T. B., Frongillo, E. A.,",
        "Melgar-Qui\u00f1onez, H. R., & Young, S. L. (2018). Best practices for",
        "developing and validating scales for health, social, and behavioral",
        "research: A primer. Frontiers in Public Health, 6, 149."
      ),
      "10.3389/fpubh.2018.00149"
    ),
    nomo_bib_entry(
      "bonifay_2017", "Bonifay, Lane, & Reise (2017)",
      paste(
        "Bonifay, W., Lane, S. P., & Reise, S. P. (2017). Three concerns with",
        "applying a bifactor model as a structure of psychopathology. Clinical",
        "Psychological Science, 5(1), 184-186."
      ),
      "10.1177/2167702616657069"
    ),
    nomo_bib_entry(
      "braeken_vanassen_2017", "Braeken & van Assen (2017)",
      paste(
        "Braeken, J., & van Assen, M. A. L. M. (2017). An empirical Kaiser",
        "criterion. Psychological Methods, 22(3), 450-466."
      ),
      "10.1037/met0000074"
    ),
    nomo_bib_entry(
      "browne_2001", "Browne (2001)",
      paste(
        "Browne, M. W. (2001). An overview of analytic rotation in exploratory",
        "factor analysis. Multivariate Behavioral Research, 36(1), 111-150."
      ),
      "10.1207/S15327906MBR3601_05"
    ),
    nomo_bib_entry(
      "browne_cudeck_1992", "Browne & Cudeck (1992)",
      paste(
        "Browne, M. W., & Cudeck, R. (1992). Alternative ways of assessing model",
        "fit. Sociological Methods & Research, 21(2), 230-258."
      ),
      "10.1177/0049124192021002005"
    ),
    nomo_bib_entry(
      "burnham_anderson_2004", "Burnham & Anderson (2004)",
      paste(
        "Burnham, K. P., & Anderson, D. R. (2004). Multimodel inference:",
        "Understanding AIC and BIC in model selection. Sociological Methods &",
        "Research, 33(2), 261-304."
      ),
      "10.1177/0049124104268644"
    ),
    nomo_bib_entry(
      "byrne_1989", "Byrne, Shavelson, & Muth\u00e9n (1989)",
      paste(
        "Byrne, B. M., Shavelson, R. J., & Muth\u00e9n, B. (1989). Testing for the",
        "equivalence of factor covariance and mean structures: The issue of",
        "partial measurement invariance. Psychological Bulletin, 105(3),",
        "456-466."
      ),
      "10.1037/0033-2909.105.3.456"
    ),
    nomo_bib_entry(
      "cattell_1966", "Cattell (1966)",
      paste(
        "Cattell, R. B. (1966). The scree test for the number of factors.",
        "Multivariate Behavioral Research, 1(2), 245-276."
      ),
      "10.1207/s15327906mbr0102_10"
    ),
    nomo_bib_entry(
      "chen_2007", "Chen (2007)",
      paste(
        "Chen, F. F. (2007). Sensitivity of goodness of fit indexes to lack of",
        "measurement invariance. Structural Equation Modeling, 14(3), 464-504."
      ),
      "10.1080/10705510701301834"
    ),
    nomo_bib_entry(
      "cheung_rensvold_2002", "Cheung & Rensvold (2002)",
      paste(
        "Cheung, G. W., & Rensvold, R. B. (2002). Evaluating goodness-of-fit",
        "indexes for testing measurement invariance. Structural Equation",
        "Modeling, 9(2), 233-255."
      ),
      "10.1207/S15328007SEM0902_5"
    ),
    nomo_bib_entry(
      "clark_watson_1995", "Clark & Watson (1995)",
      paste(
        "Clark, L. A., & Watson, D. (1995). Constructing validity: Basic issues",
        "in objective scale development. Psychological Assessment, 7(3),",
        "309-319."
      ),
      "10.1037/1040-3590.7.3.309"
    ),
    nomo_bib_entry(
      "clark_watson_2019", "Clark & Watson (2019)",
      paste(
        "Clark, L. A., & Watson, D. (2019). Constructing validity: New",
        "developments in creating objective measuring instruments.",
        "Psychological Assessment, 31(12), 1412-1427."
      ),
      "10.1037/pas0000626"
    ),
    nomo_bib_entry(
      "conway_huffcutt_2003", "Conway & Huffcutt (2003)",
      paste(
        "Conway, J. M., & Huffcutt, A. I. (2003). A review and evaluation of",
        "exploratory factor analysis practices in organizational research.",
        "Organizational Research Methods, 6(2), 147-168."
      ),
      "10.1177/1094428103251541"
    ),
    nomo_bib_entry(
      "costello_osborne_2005", "Costello & Osborne (2005)",
      paste(
        "Costello, A. B., & Osborne, J. W. (2005). Best practices in exploratory",
        "factor analysis: Four recommendations for getting the most from your",
        "analysis. Practical Assessment, Research, and Evaluation, 10,",
        "Article 7."
      ),
      "10.7275/jyj1-4868"
    ),
    nomo_bib_entry(
      "crawford_2010", "Crawford et al. (2010)",
      paste(
        "Crawford, A. V., Green, S. B., Levy, R., Lo, W.-J., Scott, L.,",
        "Svetina, D., & Thompson, M. S. (2010). Evaluation of parallel analysis",
        "methods for determining the number of factors. Educational and",
        "Psychological Measurement, 70(6), 885-901."
      ),
      "10.1177/0013164410379332"
    ),
    nomo_bib_entry(
      "cronbach_1951", "Cronbach (1951)",
      paste(
        "Cronbach, L. J. (1951). Coefficient alpha and the internal structure of",
        "tests. Psychometrika, 16(3), 297-334."
      ),
      "10.1007/BF02310555"
    ),
    nomo_bib_entry(
      "cronbach_meehl_1955", "Cronbach & Meehl (1955)",
      paste(
        "Cronbach, L. J., & Meehl, P. E. (1955). Construct validity in",
        "psychological tests. Psychological Bulletin, 52(4), 281-302."
      ),
      "10.1037/h0040957"
    ),
    nomo_bib_entry(
      "dunn_2014", "Dunn, Baguley, & Brunsden (2014)",
      paste(
        "Dunn, T. J., Baguley, T., & Brunsden, V. (2014). From alpha to omega: A",
        "practical solution to the pervasive problem of internal consistency",
        "estimation. British Journal of Psychology, 105(3), 399-412."
      ),
      "10.1111/bjop.12046"
    ),
    nomo_bib_entry(
      "enders_bandalos_2001", "Enders & Bandalos (2001)",
      paste(
        "Enders, C. K., & Bandalos, D. L. (2001). The relative performance of",
        "full information maximum likelihood estimation for missing data in",
        "structural equation models. Structural Equation Modeling, 8(3),",
        "430-457."
      ),
      "10.1207/S15328007SEM0803_5"
    ),
    nomo_bib_entry(
      "fabrigar_1999", "Fabrigar et al. (1999)",
      paste(
        "Fabrigar, L. R., Wegener, D. T., MacCallum, R. C., & Strahan, E. J.",
        "(1999). Evaluating the use of exploratory factor analysis in",
        "psychological research. Psychological Methods, 4(3), 272-299."
      ),
      "10.1037/1082-989X.4.3.272"
    ),
    nomo_bib_entry(
      "flake_fried_2020", "Flake & Fried (2020)",
      paste(
        "Flake, J. K., & Fried, E. I. (2020). Measurement schmeasurement:",
        "Questionable measurement practices and how to avoid them. Advances in",
        "Methods and Practices in Psychological Science, 3(4), 456-465."
      ),
      "10.1177/2515245920952393"
    ),
    nomo_bib_entry(
      "flake_2017", "Flake, Pek, & Hehman (2017)",
      paste(
        "Flake, J. K., Pek, J., & Hehman, E. (2017). Construct validation in",
        "social and personality research: Current practice and recommendations.",
        "Social Psychological and Personality Science, 8(4), 370-378."
      ),
      "10.1177/1948550617693063"
    ),
    nomo_bib_entry(
      "flora_2020", "Flora (2020)",
      paste(
        "Flora, D. B. (2020). Your coefficient alpha is probably wrong, but",
        "which coefficient omega is right? A tutorial on using R to obtain",
        "better reliability estimates. Advances in Methods and Practices in",
        "Psychological Science, 3(4), 484-501."
      ),
      "10.1177/2515245920951747"
    ),
    nomo_bib_entry(
      "flora_curran_2004", "Flora & Curran (2004)",
      paste(
        "Flora, D. B., & Curran, P. J. (2004). An empirical evaluation of",
        "alternative methods of estimation for confirmatory factor analysis with",
        "ordinal data. Psychological Methods, 9(4), 466-491."
      ),
      "10.1037/1082-989X.9.4.466"
    ),
    nomo_bib_entry(
      "fokkema_greiff_2017", "Fokkema & Greiff (2017)",
      paste(
        "Fokkema, M., & Greiff, S. (2017). How performing PCA and CFA on the",
        "same data equals trouble. European Journal of Psychological Assessment,",
        "33(6), 399-402."
      ),
      "10.1027/1015-5759/a000460"
    ),
    nomo_bib_entry(
      "fornell_larcker_1981", "Fornell & Larcker (1981)",
      paste(
        "Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation",
        "models with unobservable variables and measurement error. Journal of",
        "Marketing Research, 18(1), 39-50."
      ),
      "10.2307/3151312"
    ),
    nomo_bib_entry(
      "gorsuch_1983", "Gorsuch (1983)",
      paste(
        "Gorsuch, R. L. (1983). Factor analysis (2nd ed.). Lawrence Erlbaum."
      ),
      NA_character_
    ),
    nomo_bib_entry(
      "green_yang_2009", "Green & Yang (2009)",
      paste(
        "Green, S. B., & Yang, Y. (2009). Reliability of summed item scores",
        "using structural equation modeling: An alternative to coefficient",
        "alpha. Psychometrika, 74(1), 155-167."
      ),
      "10.1007/s11336-008-9099-3"
    ),
    nomo_bib_entry(
      "guttman_1954", "Guttman (1954)",
      paste(
        "Guttman, L. (1954). Some necessary conditions for common-factor",
        "analysis. Psychometrika, 19(2), 149-161."
      ),
      "10.1007/BF02289162"
    ),
    nomo_bib_entry(
      "hancock_mueller_2001", "Hancock & Mueller (2001)",
      paste(
        "Hancock, G. R., & Mueller, R. O. (2001). Rethinking construct",
        "reliability within latent variable systems. In R. Cudeck, S. du Toit, &",
        "D. Sorbom (Eds.), Structural equation modeling: Present and future",
        "(pp. 195-216). Scientific Software International."
      ),
      NA_character_
    ),
    nomo_bib_entry(
      "henseler_2015", "Henseler, Ringle, & Sarstedt (2015)",
      paste(
        "Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for",
        "assessing discriminant validity in variance-based structural equation",
        "modeling. Journal of the Academy of Marketing Science, 43(1), 115-135."
      ),
      "10.1007/s11747-014-0403-8"
    ),
    nomo_bib_entry(
      "hinkin_1998", "Hinkin (1998)",
      paste(
        "Hinkin, T. R. (1998). A brief tutorial on the development of measures",
        "for use in survey questionnaires. Organizational Research Methods,",
        "1(1), 104-121."
      ),
      "10.1177/109442819800100106"
    ),
    nomo_bib_entry(
      "holzinger_swineford_1937", "Holzinger & Swineford (1937)",
      paste(
        "Holzinger, K. J., & Swineford, F. (1937). The bi-factor method.",
        "Psychometrika, 2(1), 41-54."
      ),
      "10.1007/BF02287965"
    ),
    nomo_bib_entry(
      "horn_1965", "Horn (1965)",
      paste(
        "Horn, J. L. (1965). A rationale and test for the number of factors in",
        "factor analysis. Psychometrika, 30(2), 179-185."
      ),
      "10.1007/BF02289447"
    ),
    nomo_bib_entry(
      "hu_bentler_1999", "Hu & Bentler (1999)",
      paste(
        "Hu, L., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in",
        "covariance structure analysis: Conventional criteria versus new",
        "alternatives. Structural Equation Modeling, 6(1), 1-55."
      ),
      "10.1080/10705519909540118"
    ),
    nomo_bib_entry(
      "joreskog_1969", "J\u00f6reskog (1969)",
      paste(
        "J\u00f6reskog, K. G. (1969). A general approach to confirmatory maximum",
        "likelihood factor analysis. Psychometrika, 34(2), 183-202."
      ),
      "10.1007/BF02289343"
    ),
    nomo_bib_entry(
      "joreskog_1971", "J\u00f6reskog (1971)",
      paste(
        "J\u00f6reskog, K. G. (1971). Simultaneous factor analysis in several",
        "populations. Psychometrika, 36(4), 409-426."
      ),
      "10.1007/BF02291366"
    ),
    nomo_bib_entry(
      "kaiser_1958", "Kaiser (1958)",
      paste(
        "Kaiser, H. F. (1958). The varimax criterion for analytic rotation in",
        "factor analysis. Psychometrika, 23(3), 187-200."
      ),
      "10.1007/BF02289233"
    ),
    nomo_bib_entry(
      "kaiser_1960", "Kaiser (1960)",
      paste(
        "Kaiser, H. F. (1960). The application of electronic computers to factor",
        "analysis. Educational and Psychological Measurement, 20(1), 141-151."
      ),
      "10.1177/001316446002000116"
    ),
    nomo_bib_entry(
      "kaiser_1970", "Kaiser (1970)",
      paste(
        "Kaiser, H. F. (1970). A second generation little jiffy. Psychometrika,",
        "35(4), 401-415."
      ),
      "10.1007/BF02291817"
    ),
    nomo_bib_entry(
      "kaiser_1974", "Kaiser (1974)",
      paste(
        "Kaiser, H. F. (1974). An index of factorial simplicity. Psychometrika,",
        "39(1), 31-36."
      ),
      "10.1007/BF02291575"
    ),
    nomo_bib_entry(
      "kelley_pornprasertmanit_2016", "Kelley & Pornprasertmanit (2016)",
      paste(
        "Kelley, K., & Pornprasertmanit, S. (2016). Confidence intervals for",
        "population reliability coefficients: Evaluation of methods,",
        "recommendations, and software for composite measures. Psychological",
        "Methods, 21(1), 69-92."
      ),
      "10.1037/a0040086"
    ),
    nomo_bib_entry(
      "kolenikov_bollen_2012", "Kolenikov & Bollen (2012)",
      paste(
        "Kolenikov, S., & Bollen, K. A. (2012). Testing negative error variances:",
        "Is a Heywood case a symptom of misspecification? Sociological Methods &",
        "Research, 41(1), 124-167."
      ),
      "10.1177/0049124112442138"
    ),
    nomo_bib_entry(
      "kuhn_johnson_2013", "Kuhn & Johnson (2013)",
      "Kuhn, M., & Johnson, K. (2013). Applied predictive modeling. Springer.",
      "10.1007/978-1-4614-6849-3"
    ),
    nomo_bib_entry(
      "lakens_2018", "Lakens, Scheel, & Isager (2018)",
      paste(
        "Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing",
        "for psychological research: A tutorial. Advances in Methods and",
        "Practices in Psychological Science, 1(2), 259-269."
      ),
      "10.1177/2515245918770963"
    ),
    nomo_bib_entry(
      "lorenzoseva_2011", "Lorenzo-Seva, Timmerman, & Kiers (2011)",
      paste(
        "Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. L. (2011). The Hull",
        "method for selecting the number of common factors. Multivariate",
        "Behavioral Research, 46(2), 340-364."
      ),
      "10.1080/00273171.2011.564527"
    ),
    nomo_bib_entry(
      "maccallum_1992", "MacCallum, Roznowski, & Necowitz (1992)",
      paste(
        "MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model",
        "modifications in covariance structure analysis: The problem of",
        "capitalization on chance. Psychological Bulletin, 111(3), 490-504."
      ),
      "10.1037/0033-2909.111.3.490"
    ),
    nomo_bib_entry(
      "marsh_2004", "Marsh, Hau, & Wen (2004)",
      paste(
        "Marsh, H. W., Hau, K.-T., & Wen, Z. (2004). In search of golden rules:",
        "Comment on hypothesis-testing approaches to setting cutoff values for",
        "fit indexes and dangers in overgeneralizing Hu and Bentler's (1999)",
        "findings. Structural Equation Modeling, 11(3), 320-341."
      ),
      "10.1207/s15328007sem1103_2"
    ),
    nomo_bib_entry(
      "mcneish_2018", "McNeish (2018)",
      paste(
        "McNeish, D. (2018). Thanks coefficient alpha, we'll take it from here.",
        "Psychological Methods, 23(3), 412-433."
      ),
      "10.1037/met0000144"
    ),
    nomo_bib_entry(
      "meredith_1993", "Meredith (1993)",
      paste(
        "Meredith, W. (1993). Measurement invariance, factor analysis and",
        "factorial invariance. Psychometrika, 58(4), 525-543."
      ),
      "10.1007/BF02294825"
    ),
    nomo_bib_entry(
      "messick_1995", "Messick (1995)",
      paste(
        "Messick, S. (1995). Validity of psychological assessment: Validation of",
        "inferences from persons' responses and performances as scientific",
        "inquiry into score meaning. American Psychologist, 50(9), 741-749."
      ),
      "10.1037/0003-066X.50.9.741"
    ),
    nomo_bib_entry(
      "murray_johnson_2013", "Murray & Johnson (2013)",
      paste(
        "Murray, A. L., & Johnson, W. (2013). The limitations of model fit in",
        "comparing the bi-factor versus higher-order models of human cognitive",
        "ability structure. Intelligence, 41(5), 407-422."
      ),
      "10.1016/j.intell.2013.06.004"
    ),
    nomo_bib_entry(
      "nosek_2018", "Nosek et al. (2018)",
      paste(
        "Nosek, B. A., Ebersole, C. R., DeHaven, A. C., & Mellor, D. T. (2018).",
        "The preregistration revolution. Proceedings of the National Academy of",
        "Sciences, 115(11), 2600-2606."
      ),
      "10.1073/pnas.1708274114"
    ),
    nomo_bib_entry(
      "nunnally_bernstein_1994", "Nunnally & Bernstein (1994)",
      "Nunnally, J. C., & Bernstein, I. H. (1994). Psychometric theory (3rd ed.). McGraw-Hill."
    ),
    nomo_bib_entry(
      "putnick_bornstein_2016", "Putnick & Bornstein (2016)",
      paste(
        "Putnick, D. L., & Bornstein, M. H. (2016). Measurement invariance",
        "conventions and reporting: The state of the art and future directions",
        "for psychological research. Developmental Review, 41, 71-90."
      ),
      "10.1016/j.dr.2016.06.004"
    ),
    nomo_bib_entry(
      "raftery_1995", "Raftery (1995)",
      paste(
        "Raftery, A. E. (1995). Bayesian model selection in social research.",
        "Sociological Methodology, 25, 111-163."
      ),
      "10.2307/271063"
    ),
    nomo_bib_entry(
      "reise_2012", "Reise (2012)",
      paste(
        "Reise, S. P. (2012). The rediscovery of bifactor measurement models.",
        "Multivariate Behavioral Research, 47(5), 667-696."
      ),
      "10.1080/00273171.2012.715555"
    ),
    nomo_bib_entry(
      "reise_bonifay_2013", "Reise, Bonifay, & Haviland (2013)",
      paste(
        "Reise, S. P., Bonifay, W. E., & Haviland, M. G. (2013). Scoring and",
        "modeling psychological measures in the presence of",
        "multidimensionality. Journal of Personality Assessment, 95(2),",
        "129-140."
      ),
      "10.1080/00223891.2012.725437"
    ),
    nomo_bib_entry(
      "rhemtulla_2012", "Rhemtulla, Brosseau-Liard, & Savalei (2012)",
      paste(
        "Rhemtulla, M., Brosseau-Liard, P. \u00c9., & Savalei, V. (2012). When can",
        "categorical variables be treated as continuous? A comparison of robust",
        "continuous and categorical SEM estimation methods under suboptimal",
        "conditions. Psychological Methods, 17(3), 354-373."
      ),
      "10.1037/a0029315"
    ),
    nomo_bib_entry(
      "rodriguez_2016", "Rodriguez, Reise, & Haviland (2016)",
      paste(
        "Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating",
        "bifactor models: Calculating and interpreting statistical indices.",
        "Psychological Methods, 21(2), 137-150."
      ),
      "10.1037/met0000045"
    ),
    nomo_bib_entry(
      "roemer_2021", "Roemer, Schuberth, & Henseler (2021)",
      paste(
        "Roemer, E., Schuberth, F., & Henseler, J. (2021). HTMT2 - An improved",
        "criterion for assessing discriminant validity in structural equation",
        "modeling. Industrial Management & Data Systems, 121(12), 2637-2650."
      ),
      "10.1108/IMDS-02-2021-0082"
    ),
    nomo_bib_entry(
      "ronkko_cho_2022", "R\u00f6nkk\u00f6 & Cho (2022)",
      paste(
        "R\u00f6nkk\u00f6, M., & Cho, E. (2022). An updated guideline for assessing",
        "discriminant validity. Organizational Research Methods, 25(1)."
      ),
      "10.1177/1094428120968614"
    ),
    nomo_bib_entry(
      "rosseel_2012", "Rosseel (2012)",
      paste(
        "Rosseel, Y. (2012). lavaan: An R package for structural equation",
        "modeling. Journal of Statistical Software, 48(2), 1-36."
      ),
      "10.18637/jss.v048.i02"
    ),
    nomo_bib_entry(
      "ruscio_roche_2012", "Ruscio & Roche (2012)",
      paste(
        "Ruscio, J., & Roche, B. (2012). Determining the number of factors to",
        "retain in an exploratory factor analysis using comparison data of known",
        "factorial structure. Psychological Assessment, 24(2), 282-292."
      ),
      "10.1037/a0025697"
    ),
    nomo_bib_entry(
      "satorra_2000", "Satorra (2000)",
      paste(
        "Satorra, A. (2000). Scaled and adjusted restricted tests in multi-sample",
        "analysis of moment structures. In Innovations in multivariate",
        "statistical analysis (pp. 233-247). Springer."
      ),
      "10.1007/978-1-4615-4603-0_17"
    ),
    nomo_bib_entry(
      "satorra_bentler_2001", "Satorra & Bentler (2001)",
      paste(
        "Satorra, A., & Bentler, P. M. (2001). A scaled difference chi-square",
        "test statistic for moment structure analysis. Psychometrika, 66(4),",
        "507-514."
      ),
      "10.1007/BF02296192"
    ),
    nomo_bib_entry(
      "satorra_bentler_2010", "Satorra & Bentler (2010)",
      paste(
        "Satorra, A., & Bentler, P. M. (2010). Ensuring positiveness of the",
        "scaled difference chi-square test statistic. Psychometrika, 75(2),",
        "243-248."
      ),
      "10.1007/s11336-009-9135-y"
    ),
    nomo_bib_entry(
      "schafer_graham_2002", "Schafer & Graham (2002)",
      paste(
        "Schafer, J. L., & Graham, J. W. (2002). Missing data: Our view of the",
        "state of the art. Psychological Methods, 7(2), 147-177."
      ),
      "10.1037/1082-989X.7.2.147"
    ),
    nomo_bib_entry(
      "schmid_leiman_1957", "Schmid & Leiman (1957)",
      paste(
        "Schmid, J., & Leiman, J. M. (1957). The development of hierarchical",
        "factor solutions. Psychometrika, 22(1), 53-61."
      ),
      "10.1007/BF02289209"
    ),
    nomo_bib_entry(
      "schuirmann_1987", "Schuirmann (1987)",
      paste(
        "Schuirmann, D. J. (1987). A comparison of the two one-sided tests",
        "procedure and the power approach for assessing the equivalence of",
        "average bioavailability. Journal of Pharmacokinetics and",
        "Biopharmaceutics, 15(6), 657-680."
      ),
      "10.1007/BF01068419"
    ),
    nomo_bib_entry(
      "schwarz_1978", "Schwarz (1978)",
      paste(
        "Schwarz, G. (1978). Estimating the dimension of a model. The Annals of",
        "Statistics, 6(2), 461-464."
      ),
      "10.1214/aos/1176344136"
    ),
    nomo_bib_entry(
      "sijtsma_2009", "Sijtsma (2009)",
      paste(
        "Sijtsma, K. (2009). On the use, the misuse, and the very limited",
        "usefulness of Cronbach's alpha. Psychometrika, 74(1), 107-120."
      ),
      "10.1007/s11336-008-9101-0"
    ),
    nomo_bib_entry(
      "simmons_2011", "Simmons, Nelson, & Simonsohn (2011)",
      paste(
        "Simmons, J. P., Nelson, L. D., & Simonsohn, U. (2011). False-positive",
        "psychology: Undisclosed flexibility in data collection and analysis",
        "allows presenting anything as significant. Psychological Science,",
        "22(11), 1359-1366."
      ),
      "10.1177/0956797611417632"
    ),
    nomo_bib_entry(
      "svetina_2020", "Svetina, Rutkowski, & Rutkowski (2020)",
      paste(
        "Svetina, D., Rutkowski, L., & Rutkowski, D. (2020). Multiple-group",
        "invariance with categorical outcomes using updated guidelines: An",
        "illustration using Mplus and the lavaan/semTools packages. Structural",
        "Equation Modeling, 27(1), 111-130."
      ),
      "10.1080/10705511.2019.1602776"
    ),
    nomo_bib_entry(
      "vandenberg_lance_2000", "Vandenberg & Lance (2000)",
      paste(
        "Vandenberg, R. J., & Lance, C. E. (2000). A review and synthesis of the",
        "measurement invariance literature: Suggestions, practices, and",
        "recommendations for organizational research. Organizational Research",
        "Methods, 3(1), 4-70."
      ),
      "10.1177/109442810031002"
    ),
    nomo_bib_entry(
      "velicer_1976", "Velicer (1976)",
      paste(
        "Velicer, W. F. (1976). Determining the number of components from the",
        "matrix of partial correlations. Psychometrika, 41(3), 321-327."
      ),
      "10.1007/BF02293557"
    ),
    nomo_bib_entry(
      "velicer_2000", "Velicer, Eaton, & Fava (2000)",
      paste(
        "Velicer, W. F., Eaton, C. A., & Fava, J. L. (2000). Construct",
        "explication through factor or component analysis: A review and",
        "evaluation of alternative procedures for determining the number of",
        "factors or components. In R. D. Goffin & E. Helmes (Eds.), Problems and",
        "solutions in human assessment (pp. 41-71). Springer."
      ),
      "10.1007/978-1-4615-4397-8_3"
    ),
    nomo_bib_entry(
      "voorhees_2016", "Voorhees et al. (2016)",
      paste(
        "Voorhees, C. M., Brady, M. K., Calantone, R., & Ramirez, E. (2016).",
        "Discriminant validity testing in marketing: An analysis, causes for",
        "concern, and proposed remedies. Journal of the Academy of Marketing",
        "Science, 44(1), 119-134."
      ),
      "10.1007/s11747-015-0455-4"
    ),
    nomo_bib_entry(
      "watkins_2018", "Watkins (2018)",
      paste(
        "Watkins, M. W. (2018). Exploratory factor analysis: A guide to best",
        "practice. Journal of Black Psychology, 44(3), 219-246."
      ),
      "10.1177/0095798418771807"
    ),
    nomo_bib_entry(
      "wicherts_2016", "Wicherts et al. (2016)",
      paste(
        "Wicherts, J. M., Veldkamp, C. L. S., Augusteijn, H. E. M., Bakker, M.,",
        "van Aert, R. C. M., & van Assen, M. A. L. M. (2016). Degrees of freedom",
        "in planning, running, analyzing, and reporting psychological studies: A",
        "checklist to avoid p-hacking. Frontiers in Psychology, 7, 1832."
      ),
      "10.3389/fpsyg.2016.01832"
    ),
    nomo_bib_entry(
      "yung_1999", "Yung, Thissen, & McLeod (1999)",
      paste(
        "Yung, Y.-F., Thissen, D., & McLeod, L. D. (1999). On the relationship",
        "between the higher-order factor model and the hierarchical factor",
        "model. Psychometrika, 64(2), 113-128."
      ),
      "10.1007/BF02294531"
    ),
    nomo_bib_entry(
      "wu_estabrook_2016", "Wu & Estabrook (2016)",
      paste(
        "Wu, H., & Estabrook, R. (2016). Identification of confirmatory factor",
        "analysis models of different levels of invariance for ordered",
        "categorical outcomes. Psychometrika, 81(4), 1014-1045."
      ),
      "10.1007/s11336-016-9506-0"
    )
  )

  tibble::tibble(
    key = vapply(entries, `[[`, character(1), "key"),
    short = vapply(entries, `[[`, character(1), "short"),
    citation = vapply(entries, `[[`, character(1), "citation"),
    doi = vapply(entries, `[[`, character(1), "doi")
  )
}


# Registry --------------------------------------------------------------------

nomo_method_entry <- function(id, stage, method, lineage, role, estimand,
                              assumptions, implemented_by, engine, citations) {
  list(
    id = id,
    stage = stage,
    method = method,
    lineage = lineage,
    role = role,
    estimand = estimand,
    assumptions = assumptions,
    implemented_by = implemented_by,
    engine = engine,
    citations = citations
  )
}


nomo_methods_registry <- function() {
  entries <- list(

    # Stage 1: item and data audit ------------------------------------------
    nomo_method_entry(
      "missingness_audit", "screen",
      "Item and case missingness audit",
      "contemporary", "supporting",
      "Proportion of missing responses per item and per case.",
      paste(
        "Descriptive; no missing-data mechanism is assumed and no case or item",
        "is removed."
      ),
      "nomo_screen()", "nomologR",
      c("clark_watson_2019")
    ),
    nomo_method_entry(
      "response_distribution_audit", "screen",
      "Response distribution and category-use audit",
      "contemporary", "supporting",
      paste(
        "Category frequencies, unused categories, response concentration, and",
        "descriptive shape statistics."
      ),
      "Requires intentional numeric scoring; storage type does not establish measurement level.",
      "nomo_screen()", "nomologR",
      c("clark_watson_2019")
    ),
    nomo_method_entry(
      "item_rest_correlation", "screen",
      "Corrected item-rest correlation",
      "contemporary", "supporting",
      "Correlation of each item with the sum of the remaining items in its set.",
      paste(
        "Depends on which items are in the set, so it changes as the item pool",
        "changes; descriptive evidence rather than a retention rule."
      ),
      "nomo_screen()", "nomologR",
      c("clark_watson_2019", "nunnally_bernstein_1994")
    ),
    nomo_method_entry(
      "item_total_reference", "screen",
      "Fixed item-total correlation reference (about .30)",
      "historical", "context",
      "A conventional value below which items were once deleted.",
      paste(
        "No simulation basis for a universal threshold. Displayed as a labeled",
        "teaching reference; nomologR never deletes an item by this rule."
      ),
      "nomo_screen()", "nomologR",
      c("nunnally_bernstein_1994", "clark_watson_2019")
    ),
    nomo_method_entry(
      "near_zero_variance", "screen",
      "Near-zero-variance screening",
      "contemporary", "supporting",
      "Frequency ratio and percent-unique heuristics identifying near-constant items.",
      "Heuristic thresholds; flags items for review rather than removing them.",
      "nomo_screen()", "nomologR",
      c("kuhn_johnson_2013")
    ),

    # Stage 2: dimensionality ------------------------------------------------
    nomo_method_entry(
      "parallel_analysis", "factors",
      "Common-factor parallel analysis",
      "contemporary", "primary",
      paste(
        "Number of factors whose observed eigenvalues exceed a reference",
        "quantile of eigenvalues from data with no common factors."
      ),
      paste(
        "Reference distribution depends on the simulation design, the decision",
        "rule (percentile, mean, or Crawford), and the correlation type."
      ),
      "nomo_factors()", "psych",
      c("horn_1965", "crawford_2010")
    ),
    nomo_method_entry(
      "map_original", "factors",
      "Velicer original minimum average partial (MAP)",
      "contemporary", "supporting",
      "Factor count minimizing the average squared partial correlation.",
      "Component-based; evaluated only over the factor counts the data support.",
      "nomo_factors()", "nomologR",
      c("velicer_1976")
    ),
    nomo_method_entry(
      "map_revised", "factors",
      "Velicer revised MAP (fourth powers)",
      "contemporary", "supporting",
      "Factor count minimizing the average fourth-power partial correlation.",
      "As for original MAP; less prone to under-extraction with weak factors.",
      "nomo_factors()", "nomologR",
      c("velicer_2000")
    ),
    nomo_method_entry(
      "ekc", "factors",
      "Empirical Kaiser criterion",
      "contemporary", "supporting",
      "Factor count from eigenvalues compared with a sample-size-aware reference.",
      "Derived for continuous indicators and approximately uncorrelated factors.",
      "nomo_factors()", "EFAtools",
      c("braeken_vanassen_2017")
    ),
    nomo_method_entry(
      "nest", "factors",
      "Next Eigenvalue Sufficiency Test (NEST)",
      "emerging", "supporting",
      "Sequential test of whether the next eigenvalue exceeds what the retained model implies.",
      "Resampling-based and computationally heavier; availability depends on the data.",
      "nomo_factors()", "EFAtools",
      c("achim_2017")
    ),
    nomo_method_entry(
      "hull", "factors",
      "Hull method",
      "contemporary", "supporting",
      "Factor count at the elbow of the fit-versus-parameters hull.",
      "Requires a fit index and degrees of freedom across a range of solutions.",
      "nomo_factors()", "EFAtools",
      c("lorenzoseva_2011")
    ),
    nomo_method_entry(
      "comparison_data", "factors",
      "Comparison data",
      "contemporary", "supporting",
      "Factor count whose simulated comparison data best reproduce the observed eigenvalues.",
      "Simulation-based; results vary with the number of datasets generated.",
      "nomo_factors()", "EFAtools",
      c("ruscio_roche_2012")
    ),
    nomo_method_entry(
      "kaiser_guttman", "factors",
      "Eigenvalue-greater-than-one rule",
      "historical", "context",
      "Number of eigenvalues of the correlation matrix greater than one.",
      paste(
        "Over-extracts in simulation and was derived as a lower bound rather",
        "than a retention rule. Displayed as labeled historical context and",
        "excluded from the retention synthesis."
      ),
      "nomo_factors()", "nomologR",
      c("guttman_1954", "kaiser_1960", "kaiser_1970")
    ),
    nomo_method_entry(
      "scree", "factors",
      "Scree test",
      "historical", "context",
      "Visual elbow in the ordered eigenvalue plot.",
      "Requires subjective judgement and is not reproducible across readers.",
      "nomo_factors()", "nomologR",
      c("cattell_1966")
    ),
    nomo_method_entry(
      "kmo", "factors",
      "Kaiser-Meyer-Olkin sampling adequacy",
      "historical", "supporting",
      "Ratio of squared correlations to squared partial correlations, overall and per item.",
      "Supporting adequacy evidence, not an item-retention rule.",
      "nomo_factors()", "psych",
      c("kaiser_1974")
    ),
    nomo_method_entry(
      "bartlett", "factors",
      "Bartlett's test of sphericity",
      "historical", "supporting",
      "Test that the correlation matrix differs from an identity matrix.",
      paste(
        "Strongly sample-size sensitive and almost always significant at",
        "realistic sample sizes; supporting evidence only."
      ),
      "nomo_factors()", "psych",
      c("bartlett_1950")
    ),
    nomo_method_entry(
      "categorical_correlations", "factors",
      "Polychoric and tetrachoric correlations",
      "contemporary", "supporting",
      "Correlations between the continuous variables assumed to underlie ordered responses.",
      paste(
        "Assumes underlying normality; requires sufficient observations in each",
        "response category."
      ),
      "nomo_factors()", "psych",
      c("flora_curran_2004", "rhemtulla_2012")
    ),

    # Stage 3: exploratory structure ----------------------------------------
    nomo_method_entry(
      "minres_extraction", "efa",
      "MINRES common-factor extraction",
      "contemporary", "primary",
      "Common-factor loadings minimizing residual correlations.",
      "Models common variance only; distinct from principal components.",
      "nomo_efa()", "psych",
      c("fabrigar_1999")
    ),
    nomo_method_entry(
      "oblique_rotation", "efa",
      "Oblique (oblimin) rotation",
      "contemporary", "primary",
      "Rotated pattern and structure matrices permitting correlated factors.",
      "Appropriate whenever factors may plausibly correlate, which is the usual case in psychology.",
      "nomo_efa()", "psych",
      c("browne_2001", "fabrigar_1999")
    ),
    nomo_method_entry(
      "orthogonal_rotation", "efa",
      "Orthogonal (varimax) rotation",
      "historical", "context",
      "Rotated loadings under an imposed zero factor correlation.",
      paste(
        "Forces factors to be uncorrelated, which is rarely defensible for",
        "psychological constructs. Available, but recorded as a choice that",
        "needs justification."
      ),
      "nomo_efa()", "psych",
      c("kaiser_1958", "conway_huffcutt_2003")
    ),
    nomo_method_entry(
      "loading_diagnostics", "efa",
      "Cross-loading, communality, and residual diagnostics",
      "contemporary", "supporting",
      "Per-item loading pattern, communality, and residual correlation summaries.",
      paste(
        "Numerical evidence only; item decisions require content judgement",
        "alongside these values."
      ),
      "nomo_efa()", "nomologR",
      c("costello_osborne_2005", "watkins_2018")
    ),
    nomo_method_entry(
      "loading_reference", "efa",
      "Fixed loading cutoff",
      "historical", "context",
      "A conventional loading value below which items were once deleted.",
      paste(
        "No universal cutoff is supported by simulation. Displayed as a labeled",
        "review prompt; nomologR never deletes an item by this rule."
      ),
      "nomo_efa()", "nomologR",
      c("costello_osborne_2005")
    ),

    # Stage 4: confirmatory measurement model --------------------------------
    nomo_method_entry(
      "ml_cfa", "cfa",
      "Maximum-likelihood confirmatory factor analysis",
      "contemporary", "primary",
      "Loadings, residual variances, and factor covariances for a specified measurement model.",
      "Assumes continuous indicators and a correctly specified model.",
      "nomo_cfa()", "lavaan",
      c("joreskog_1969", "rosseel_2012")
    ),
    nomo_method_entry(
      "wlsmv_cfa", "cfa",
      "WLSMV estimation for ordered indicators",
      "contemporary", "primary",
      "Measurement parameters estimated from polychoric correlations with robust corrections.",
      paste(
        "Requires indicators declared as ordered; information criteria are not",
        "defined for this estimator."
      ),
      "nomo_cfa()", "lavaan",
      c("flora_curran_2004", "rhemtulla_2012")
    ),
    nomo_method_entry(
      "chisq_exact_fit", "cfa",
      "Chi-square exact-fit test",
      "historical", "supporting",
      "Test of the null hypothesis that the model reproduces the population covariance matrix exactly.",
      paste(
        "Power increases with sample size, so trivial misspecification becomes",
        "significant in large samples. Reported, never used alone."
      ),
      "nomo_cfa()", "lavaan",
      c("joreskog_1969")
    ),
    nomo_method_entry(
      "incremental_fit", "cfa",
      "Incremental fit indices (CFI, TLI)",
      "contemporary", "supporting",
      "Improvement in fit relative to a null model of uncorrelated observed variables.",
      "Depends on the null model; comparable only across models of the same data.",
      "nomo_cfa()", "lavaan",
      c("bentler_1990", "bentler_bonett_1980")
    ),
    nomo_method_entry(
      "rmsea_interval", "cfa",
      "RMSEA with confidence interval",
      "contemporary", "supporting",
      "Population misfit per degree of freedom, with an interval estimate.",
      "Sensitive to model size and sample size; the interval is the point of reporting it.",
      "nomo_cfa()", "lavaan",
      c("browne_cudeck_1992")
    ),
    nomo_method_entry(
      "srmr", "cfa",
      "Standardized root mean square residual",
      "contemporary", "supporting",
      "Average standardized difference between observed and model-implied correlations.",
      "Summarizes residuals into one number and can hide localized strain.",
      "nomo_cfa()", "lavaan",
      c("hu_bentler_1999")
    ),
    nomo_method_entry(
      "fit_cutoffs", "cfa",
      "Fixed fit-index cutoffs",
      "historical", "context",
      "Conventional values such as CFI and TLI near .95, RMSEA near .06, SRMR near .08.",
      paste(
        "Derived under specific simulation conditions and widely overgeneralized",
        "since. Displayed as labeled teaching references, never as pass/fail",
        "rules."
      ),
      "nomo_cfa()", "nomologR",
      c("hu_bentler_1999", "marsh_2004")
    ),
    nomo_method_entry(
      "local_strain", "cfa",
      "Localized residual correlations",
      "contemporary", "supporting",
      "Largest standardized differences between observed and model-implied relationships.",
      "Descriptive evidence about where a model misses; not a binary fit rule.",
      "nomo_cfa()", "nomologR",
      c("marsh_2004")
    ),
    nomo_method_entry(
      "modification_indices", "cfa",
      "Modification indices",
      "historical", "context",
      "Expected chi-square improvement from freeing each fixed parameter.",
      paste(
        "Data-driven respecification capitalizes on chance and does not",
        "replicate. Quarantined as post-hoc diagnostics; nomologR never applies",
        "them to a model."
      ),
      "nomo_cfa()", "lavaan",
      c("maccallum_1992")
    ),
    nomo_method_entry(
      "improper_solutions", "cfa",
      "Improper-solution (Heywood case) diagnostics",
      "contemporary", "supporting",
      "Detection of negative residual variances, out-of-range loadings, and latent correlations beyond one.",
      "An improper solution is a possible symptom of misspecification, not proof of it.",
      "nomo_cfa()", "nomologR",
      c("kolenikov_bollen_2012")
    ),
    nomo_method_entry(
      "bifactor_model", "cfa",
      "Bifactor measurement model",
      "contemporary", "primary",
      paste(
        "A general factor measured by every item plus orthogonal group factors",
        "measured by subsets of items."
      ),
      paste(
        "General and group factors must be orthogonal. It usually fits at least",
        "as well as correlated-factors models of the same items even when it",
        "did not generate the data, so fit alone does not choose it; Bonifay,",
        "Lane, and Reise (2017) treat that superior fit as possible",
        "overfitting."
      ),
      "nomo_model()", "lavaan",
      c("holzinger_swineford_1937", "reise_2012", "bonifay_2017")
    ),
    nomo_method_entry(
      "higher_order_model", "cfa",
      "Higher-order (second-order) measurement model",
      "contemporary", "primary",
      "First-order factors whose correlations are explained by a second-order factor.",
      paste(
        "Needs at least three first-order factors; with exactly three it fits",
        "exactly as well as correlated factors. It is a constrained version of",
        "the bifactor model, and Murray and Johnson (2013) found the fit",
        "comparison between the two biased in favor of the bifactor model",
        "whenever complexity is left unmodelled."
      ),
      "nomo_model()", "lavaan",
      c("yung_1999", "reise_2012", "murray_johnson_2013")
    ),
    nomo_method_entry(
      "holdout_split", "cfa",
      "Calibration and validation sample split",
      "contemporary", "supporting",
      "Independent subsamples for deriving and confirming a measurement model.",
      paste(
        "Requires enough cases for both parts; a split loses independence once",
        "the validation half informs the model."
      ),
      "nomo_split()", "nomologR",
      c("fokkema_greiff_2017")
    ),

    # Model comparison --------------------------------------------------------
    nomo_method_entry(
      "lrt_standard", "compare",
      "Likelihood-ratio difference test",
      "contemporary", "primary",
      "Chi-square difference between nested models with a degrees-of-freedom difference.",
      "Valid only for genuinely nested models fitted to the same cases with the same estimator.",
      "nomo_compare()", "lavaan",
      c("bentler_satorra_2010")
    ),
    nomo_method_entry(
      "lrt_scaled", "compare",
      "Satorra-Bentler scaled difference test",
      "contemporary", "primary",
      "Chi-square difference corrected for the scaling of robust test statistics.",
      "Requires a robust estimator; the naive difference of scaled statistics is not chi-square distributed.",
      "nomo_compare()", "lavaan",
      c("satorra_bentler_2001", "satorra_bentler_2010")
    ),
    nomo_method_entry(
      "lrt_scaled_shifted", "compare",
      "Scaled-and-shifted difference test",
      "contemporary", "primary",
      "Difference test for categorical estimators, shifted to keep the statistic positive.",
      "Applies to mean- and variance-adjusted estimators such as WLSMV.",
      "nomo_compare()", "lavaan",
      c("satorra_2000")
    ),
    nomo_method_entry(
      "nesting_check", "compare",
      "Formal model-nesting check",
      "contemporary", "supporting",
      "Whether one model is nested in, equivalent to, or unrelated to another.",
      "Nesting is verified rather than assumed from parameter counts.",
      "nomo_compare()", "semTools",
      c("bentler_satorra_2010")
    ),
    nomo_method_entry(
      "delta_fit", "compare",
      "Change-in-fit indices",
      "contemporary", "supporting",
      "Differences in CFI, TLI, RMSEA, and SRMR between two models.",
      "Several changes are reported together; no single change is a decision rule.",
      "nomo_compare()", "nomologR",
      c("cheung_rensvold_2002", "chen_2007")
    ),
    nomo_method_entry(
      "information_criteria", "compare",
      "Information criteria (AIC, BIC)",
      "contemporary", "supporting",
      "Relative out-of-sample fit penalized for model complexity.",
      paste(
        "Comparable only across models of the same data and undefined for",
        "estimators without a likelihood, such as WLSMV."
      ),
      "nomo_compare()", "lavaan",
      c("akaike_1974", "schwarz_1978", "raftery_1995", "burnham_anderson_2004")
    ),

    # Stage 5: reliability ----------------------------------------------------
    nomo_method_entry(
      "omega", "reliability",
      "Model-based coefficient omega",
      "contemporary", "primary",
      "Proportion of composite-score variance attributable to the common factor.",
      paste(
        "Requires a fitted measurement model; misspecification of that model",
        "biases the estimate."
      ),
      "nomo_reliability()", "semTools",
      c("dunn_2014", "mcneish_2018", "flora_2020", "bell_2024")
    ),
    nomo_method_entry(
      "omega_ordinal_scale", "reliability",
      "Reliability on the ordered-score scale",
      "contemporary", "supporting",
      "Reliability of the summed score actually used, for ordered indicators.",
      paste(
        "The estimand differs from latent-scale reliability; nomologR reports",
        "alpha as unavailable rather than silently redefining it for this",
        "estimand."
      ),
      "nomo_reliability()", "semTools",
      c("green_yang_2009")
    ),
    nomo_method_entry(
      "alpha", "reliability",
      "Coefficient alpha",
      "historical", "supporting",
      "Lower bound on reliability under tau-equivalence.",
      paste(
        "Assumes equal true-score loadings and uncorrelated residuals; both are",
        "usually violated. Reported as a qualified secondary statistic."
      ),
      "nomo_reliability()", "nomologR",
      c("cronbach_1951", "sijtsma_2009")
    ),
    nomo_method_entry(
      "reliability_bootstrap_ci", "reliability",
      "Bootstrap confidence intervals for reliability",
      "contemporary", "supporting",
      "Interval estimate for a reliability coefficient.",
      "Resampling-based; requires enough successful draws to be trustworthy.",
      "nomo_reliability()", "nomologR",
      c("kelley_pornprasertmanit_2016")
    ),

    nomo_method_entry(
      "omega_hierarchical", "reliability",
      "Omega hierarchical",
      "contemporary", "primary",
      "Proportion of unit-weighted total-score variance explained by the general factor.",
      paste(
        "Requires orthogonal general and group sources. Describes the observed",
        "composite for continuous items and the latent-response composite for",
        "ordered items."
      ),
      "nomo_hierarchical()", "nomologR",
      c("reise_2012", "reise_bonifay_2013", "rodriguez_2016")
    ),
    nomo_method_entry(
      "omega_hierarchical_subscale", "reliability",
      "Omega hierarchical subscale",
      "contemporary", "supporting",
      paste(
        "Proportion of a subscale composite's variance that is reliable and",
        "specific to its group factor, after removing the general factor."
      ),
      "Requires orthogonal general and group sources.",
      "nomo_hierarchical()", "nomologR",
      c("reise_bonifay_2013", "rodriguez_2016")
    ),
    nomo_method_entry(
      "ecv", "reliability",
      "Explained common variance",
      "contemporary", "supporting",
      "Share of the common variance across items explained by the general factor.",
      paste(
        "Computed from standardized loadings. No benchmark value establishes",
        "that the items are unidimensional."
      ),
      "nomo_hierarchical()", "nomologR",
      c("reise_2012", "rodriguez_2016")
    ),
    nomo_method_entry(
      "factor_determinacy", "reliability",
      "Factor determinacy",
      "contemporary", "supporting",
      "Correlation between a factor and its estimated factor score.",
      paste(
        "Computed from the model-reproduced correlation matrix, so it does not",
        "depend on the omega denominator. Gorsuch's (1983) recommendation that",
        "scores be used above .90, and that competing score sets correlate",
        "above .70, is reported as his recommendation and not applied."
      ),
      "nomo_hierarchical()", "nomologR",
      c("beauducel_2011", "gorsuch_1983", "rodriguez_2016")
    ),
    nomo_method_entry(
      "construct_replicability", "reliability",
      "Construct replicability (H)",
      "contemporary", "supporting",
      paste(
        "Proportion of variance in a factor explainable by its own indicators",
        "when optimally weighted."
      ),
      paste(
        "Uses only that factor's loadings and treats the rest of each item as",
        "uncorrelated residual, which holds for a unidimensional construct. It",
        "equals squared factor determinacy in that case and can differ under a",
        "bifactor model. Hancock and Mueller's (2001) standard of .70 is",
        "reported as their standard and not applied."
      ),
      "nomo_hierarchical()", "nomologR",
      c("hancock_mueller_2001", "rodriguez_2016")
    ),
    nomo_method_entry(
      "puc", "reliability",
      "Percentage of uncontaminated correlations",
      "contemporary", "supporting",
      "Share of item correlations influenced only by the general factor.",
      paste(
        "Depends only on how items are assigned to group factors, not on the",
        "estimates."
      ),
      "nomo_hierarchical()", "nomologR",
      c("reise_2012", "rodriguez_2016")
    ),
    nomo_method_entry(
      "schmid_leiman", "reliability",
      "Schmid-Leiman decomposition",
      "historical", "supporting",
      paste(
        "General and residualized group loadings implied by a higher-order",
        "solution."
      ),
      paste(
        "Originally an orthogonalization of exploratory solutions; applied here",
        "to a confirmatory higher-order model, where it is exact. The",
        "proportionality it imposes is what distinguishes a higher-order model",
        "from a bifactor model."
      ),
      "nomo_hierarchical()", "nomologR",
      c("schmid_leiman_1957", "yung_1999")
    ),

    # Stage 6: convergent and discriminant evidence --------------------------
    nomo_method_entry(
      "standardized_loadings_ave", "validity",
      "Standardized loadings and average variance extracted",
      "historical", "supporting",
      "Average proportion of indicator variance explained by its factor.",
      paste(
        "Convergent evidence only. AVE is not reliability and nomologR never",
        "reports it as such."
      ),
      "nomo_validity()", "nomologR",
      c("fornell_larcker_1981")
    ),
    nomo_method_entry(
      "htmt", "validity",
      "Heterotrait-monotrait ratio",
      "contemporary", "supporting",
      "Ratio of between-construct to within-construct item correlations.",
      "Assumes tau-equivalent indicators; reported alongside HTMT2 for comparison.",
      "nomo_validity()", "nomologR",
      c("henseler_2015")
    ),
    nomo_method_entry(
      "htmt2", "validity",
      "HTMT2",
      "contemporary", "primary",
      "Congeneric-appropriate ratio of between-construct to within-construct correlations.",
      "Requires positive correlations within each construct block.",
      "nomo_validity()", "nomologR",
      c("roemer_2021")
    ),
    nomo_method_entry(
      "fornell_larcker", "validity",
      "Fornell-Larcker comparison",
      "historical", "context",
      "Comparison of AVE with squared between-construct correlations.",
      paste(
        "Simulation work shows it frequently fails to detect discriminant",
        "validity problems. Produced only on explicit request and labeled as",
        "legacy supporting evidence."
      ),
      "nomo_validity()", "nomologR",
      c("fornell_larcker_1981", "henseler_2015", "voorhees_2016")
    ),
    nomo_method_entry(
      "latent_correlation_ci", "validity",
      "Latent correlations with confidence intervals",
      "contemporary", "primary",
      "Model-estimated correlations between constructs, with uncertainty.",
      "Interpreted with the measurement model in view rather than against a fixed cutoff.",
      "nomo_validity()", "lavaan",
      c("ronkko_cho_2022")
    ),

    # Stage 7: measurement invariance ----------------------------------------
    nomo_method_entry(
      "multigroup_cfa", "invariance",
      "Multiple-group confirmatory factor analysis",
      "contemporary", "primary",
      "Measurement parameters estimated simultaneously across groups.",
      "Requires the same model to be identified and estimable in every group.",
      "nomo_invariance()", "lavaan",
      c("joreskog_1971")
    ),
    nomo_method_entry(
      "invariance_hierarchy", "invariance",
      "Configural, metric, scalar, and strict sequence",
      "contemporary", "primary",
      "Progressively constrained models testing equality of form, loadings, intercepts, and residuals.",
      "Each level presumes the preceding level is tenable.",
      "nomo_invariance()", "nomologR",
      c("meredith_1993", "vandenberg_lance_2000")
    ),
    nomo_method_entry(
      "categorical_invariance", "invariance",
      "Identification-aware sequence for ordered indicators",
      "contemporary", "primary",
      "Invariance levels defined for thresholds, loadings, and intercepts under categorical identification.",
      paste(
        "The sequence depends on the number of observed categories; a",
        "continuous-indicator sequence is not valid for ordered items."
      ),
      "nomo_invariance()", "lavaan",
      c("wu_estabrook_2016", "svetina_2020")
    ),
    nomo_method_entry(
      "invariance_delta_fit", "invariance",
      "Change-in-fit evidence across levels",
      "contemporary", "supporting",
      "Differences in fit between adjacent invariance levels.",
      "Interpreted with sample size and model context; no universal pass/fail value.",
      "nomo_invariance()", "nomologR",
      c("chen_2007", "putnick_bornstein_2016")
    ),
    nomo_method_entry(
      "score_diagnostics", "invariance",
      "Score-test strain diagnostics",
      "contemporary", "supporting",
      "Expected improvement from releasing each equality constraint.",
      paste(
        "Locates strain only. nomologR never frees a constraint automatically;",
        "releases require a researcher rationale."
      ),
      "nomo_invariance()", "lavaan",
      c("putnick_bornstein_2016")
    ),
    nomo_method_entry(
      "partial_invariance", "invariance",
      "Partial invariance with documented releases",
      "contemporary", "primary",
      "Invariance holding for a subset of parameters, with the released set stated.",
      paste(
        "Releases must be substantively justified and reported; carried forward",
        "to more restrictive levels."
      ),
      "nomo_partial()", "lavaan",
      c("byrne_1989")
    ),

    # Stage 8: nomological network -------------------------------------------
    nomo_method_entry(
      "nomological_network", "network",
      "Nomological network of construct relations",
      "historical", "primary",
      "The pattern of predicted relations a construct should show with other constructs.",
      paste(
        "Evidence for a network of relations, not a single validity coefficient.",
        "nomologR never computes an overall nomological-validity score."
      ),
      "nomo_network()", "nomologR",
      c("cronbach_meehl_1955", "messick_1995")
    ),
    nomo_method_entry(
      "two_step_sem", "network",
      "Measurement-then-structure modeling",
      "contemporary", "primary",
      "Structural relations estimated once the measurement model is established.",
      "Structural estimates are uninterpretable if the measurement model is inadequate.",
      "nomo_network()", "lavaan",
      c("anderson_gerbing_1988")
    ),
    nomo_method_entry(
      "equivalence_testing", "network",
      "Equivalence testing against a smallest effect size of interest",
      "contemporary", "primary",
      "Whether an estimate falls entirely inside a researcher-specified negligible region.",
      paste(
        "The region must be specified by the researcher on substantive grounds.",
        "nomologR never invents one, and a non-significant test is not evidence",
        "of no relation."
      ),
      "nomo_hypotheses()", "nomologR",
      c("schuirmann_1987", "lakens_2018")
    ),
    nomo_method_entry(
      "prediction_provenance", "network",
      "A-priori versus post-hoc prediction provenance",
      "contemporary", "supporting",
      "Whether each prediction was specified before or after seeing the results.",
      "Depends on honest recording; the package cannot verify when a prediction was formed.",
      "nomo_hypotheses()", "nomologR",
      c("nosek_2018")
    ),
    nomo_method_entry(
      "replication_same_model", "network",
      "Replication of the identical model in validation data",
      "contemporary", "supporting",
      "The same specified model refitted in independent cases.",
      "Independence is lost if the validation cases informed the model.",
      "nomo_network()", "nomologR",
      c("fokkema_greiff_2017")
    ),

    # Stage 9: workflow, provenance, and reporting ---------------------------
    nomo_method_entry(
      "staged_workflow", "workflow",
      "Staged scale-development workflow",
      "contemporary", "primary",
      "An ordered sequence from item review through structure, reliability, validity, and theory.",
      "Later stages presume earlier evidence was reviewed rather than skipped.",
      "nomo_run()", "nomologR",
      c("clark_watson_1995", "clark_watson_2019", "hinkin_1998", "boateng_2018")
    ),
    nomo_method_entry(
      "decision_log", "workflow",
      "Recorded decisions and rationales",
      "contemporary", "primary",
      "Every consequential choice, its rationale, and its consequence.",
      paste(
        "Records what the researcher decided; it does not make the decision",
        "defensible on its own."
      ),
      "nomo_run()", "nomologR",
      c("flake_2017", "flake_fried_2020", "simmons_2011", "wicherts_2016")
    ),
    nomo_method_entry(
      "revision_lineage", "workflow",
      "Recorded revision lineage",
      "contemporary", "primary",
      "The path from an original model to the reported one, with rationale and origin for each change.",
      paste(
        "A revision evaluated on the data that prompted it capitalizes on",
        "chance; independent confirmation is recommended, and nomologR cannot",
        "supply it."
      ),
      "nomo_revise()", "nomologR",
      c("maccallum_1992", "wicherts_2016")
    ),
    nomo_method_entry(
      "fiml", "workflow",
      "Full-information maximum likelihood for missing data",
      "contemporary", "supporting",
      "Parameters estimated from all available responses without imputing values.",
      "Assumes data are missing at random conditional on the modeled variables.",
      "nomo_cfa()", "lavaan",
      c("enders_bandalos_2001", "schafer_graham_2002")
    ),
    nomo_method_entry(
      "reproducible_report", "workflow",
      "Archived reproducible report",
      "contemporary", "primary",
      "Methods, evidence, decisions, deviations, and session information in one archived document.",
      "Archives what was recorded; it cannot recover decisions that were never logged.",
      "nomo_report()", "nomologR",
      c("flake_fried_2020", "wicherts_2016")
    )
  )

  tibble::tibble(
    id = vapply(entries, `[[`, character(1), "id"),
    stage = vapply(entries, `[[`, character(1), "stage"),
    method = vapply(entries, `[[`, character(1), "method"),
    lineage = vapply(entries, `[[`, character(1), "lineage"),
    role = vapply(entries, `[[`, character(1), "role"),
    estimand = vapply(entries, `[[`, character(1), "estimand"),
    assumptions = vapply(entries, `[[`, character(1), "assumptions"),
    implemented_by = vapply(entries, `[[`, character(1), "implemented_by"),
    engine = vapply(entries, `[[`, character(1), "engine"),
    citations = I(lapply(entries, `[[`, "citations"))
  )
}


nomo_methods_stages <- function() {
  c(
    "screen", "factors", "efa", "cfa", "compare", "reliability",
    "validity", "invariance", "network", "workflow"
  )
}


nomo_methods_lineages <- function() {
  c("historical", "contemporary", "emerging")
}


nomo_methods_roles <- function() {
  c("primary", "supporting", "context")
}


# Collapse citation keys into the short in-text form used in compact output.
nomo_methods_short_citations <- function(keys, bib) {
  vapply(
    keys,
    function(k) paste(bib$short[match(k, bib$key)], collapse = "; "),
    character(1),
    USE.NAMES = FALSE
  )
}


#' The methods registry: what nomologR computes, and where it comes from
#'
#' `nomo_methods()` returns a machine-readable registry of the statistical
#' methods `nomologR` implements. Each entry records the workflow stage, where
#' the method sits in the literature, what it estimates, its key assumptions,
#' the function that implements it, the computational engine, and DOI-verified
#' references.
#'
#' @details
#' The registry answers a question learners ask constantly and software rarely
#' answers: *why this method, and where did it come from?* Every entry carries a
#' `lineage` label:
#'
#' * `"historical"`: techniques a reader will meet in published work, retained
#'   so they can be recognized and interpreted. They are labeled and qualified.
#' * `"contemporary"`: what the current methodological literature recommends.
#' * `"emerging"`: recent proposals with a smaller evidence base.
#'
#' A lineage label summarizes where a method sits in the literature. It is a
#' teaching aid, not a claim that an older method is always wrong.
#'
#' The `role` column says how `nomologR` uses the method: `"primary"` evidence,
#' `"supporting"` evidence, or `"context"`. A `"context"` method is displayed
#' for recognition and is deliberately excluded from any synthesis: the
#' eigenvalue-greater-than-one rule and fixed fit-index cutoffs are shown so a
#' reader can interpret older reports, never so `nomologR` can act on them.
#'
#' **The registry describes only what the package actually computes.** Methods
#' that are planned but not implemented are discussed in the research-basis
#' article and tracked in issues; they are deliberately absent here, so that
#' `nomo_methods(run)` can never credit a run with a method it did not use.
#'
#' Passing a `nomologR` result object returns only the methods that object
#' actually used, which is what [nomo_report()] cites.
#'
#' @param x Optional `nomologR` result object. If supplied, only the methods
#'   actually used in producing that object are returned, in registry order. If
#'   `NULL` (default), the whole registry is returned.
#' @param stage Optional character vector restricting the result to these
#'   workflow stages. One or more of `"screen"`, `"factors"`, `"efa"`, `"cfa"`,
#'   `"compare"`, `"reliability"`, `"validity"`, `"invariance"`, `"network"`,
#'   `"workflow"`.
#' @param lineage Optional character vector restricting the result to
#'   `"historical"`, `"contemporary"`, or `"emerging"` methods.
#' @param references If `FALSE` (default), each row is one method and the
#'   `references` column holds short in-text citations. If `TRUE`, the result is
#'   expanded to one row per method per reference, with full `citation` and
#'   `doi` columns, suitable for a reference list.
#'
#' @return A tibble. With `references = FALSE`, one row per method. With
#'   `references = TRUE`, one row per method-reference pair.
#' @export
#'
#' @examples
#' # The whole registry
#' nomo_methods()
#'
#' # What is shown only as historical context, and why
#' nomo_methods(lineage = "historical")[, c("method", "role", "assumptions")]
#'
#' # A reference list for one stage
#' nomo_methods(stage = "reliability", references = TRUE)[, c("method", "citation")]
nomo_methods <- function(x = NULL,
                         stage = NULL,
                         lineage = NULL,
                         references = FALSE) {
  if (!is.logical(references) || length(references) != 1L || is.na(references)) {
    stop("`references` must be TRUE or FALSE.", call. = FALSE)
  }

  registry <- nomo_methods_registry()
  bib <- nomo_bibliography()

  if (!is.null(x)) {
    used <- nomo_methods_used(x)
    unknown <- setdiff(used, registry$id)
    if (length(unknown)) {
      stop(
        "Internal error: methods used but absent from the registry: ",
        paste(unknown, collapse = ", "),
        call. = FALSE
      )
    }
    registry <- registry[registry$id %in% used, , drop = FALSE]
  }

  if (!is.null(stage)) {
    valid <- nomo_methods_stages()
    bad <- setdiff(stage, valid)
    if (length(bad)) {
      stop(
        "Unknown `stage` value(s): ", paste(bad, collapse = ", "),
        ". Valid stages are: ", paste(valid, collapse = ", "), ".",
        call. = FALSE
      )
    }
    registry <- registry[registry$stage %in% stage, , drop = FALSE]
  }

  if (!is.null(lineage)) {
    valid <- nomo_methods_lineages()
    bad <- setdiff(lineage, valid)
    if (length(bad)) {
      stop(
        "Unknown `lineage` value(s): ", paste(bad, collapse = ", "),
        ". Valid values are: ", paste(valid, collapse = ", "), ".",
        call. = FALSE
      )
    }
    registry <- registry[registry$lineage %in% lineage, , drop = FALSE]
  }

  if (!references) {
    keys <- registry$citations
    registry$references <- nomo_methods_short_citations(keys, bib)
    registry$citations <- NULL
    return(tibble::as_tibble(registry))
  }

  if (!nrow(registry)) {
    out <- registry
    out$citations <- NULL
    out$citation_key <- character()
    out$citation <- character()
    out$doi <- character()
    return(tibble::as_tibble(out))
  }

  counts <- lengths(registry$citations)
  expanded <- registry[rep(seq_len(nrow(registry)), counts), , drop = FALSE]
  keys <- unlist(registry$citations, use.names = FALSE)
  expanded$citations <- NULL
  expanded$citation_key <- keys
  expanded$citation <- bib$citation[match(keys, bib$key)]
  expanded$doi <- bib$doi[match(keys, bib$key)]

  tibble::as_tibble(expanded)
}
