# Methods registry -------------------------------------------------------------

# Bibliography ----------------------------------------------------------------
#
# One entry per work cited by the registry. `short` is the in-text form used in
# compact output, following APA 7: both names for two authors, and the first
# author with "et al." for three or more, which no two entries share (#145,
# lit-10); `citation` is the full reference. Author names that contain
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
      "asparouhov_muthen_2009", "Asparouhov & Muth\u00e9n (2009)",
      paste(
        "Asparouhov, T., & Muth\u00e9n, B. (2009). Exploratory structural",
        "equation modeling. Structural Equation Modeling, 16(3), 397-438."
      ),
      "10.1080/10705510903008204"
    ),
    nomo_bib_entry(
      "bagozzi_heatherton_1994", "Bagozzi & Heatherton (1994)",
      paste(
        "Bagozzi, R. P., & Heatherton, T. F. (1994). A general approach to",
        "representing multifaceted personality constructs: Application to state",
        "self-esteem. Structural Equation Modeling, 1(1), 35-67."
      ),
      "10.1080/10705519409539961"
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
      "bell_2024", "Bell et al. (2024)",
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
      "bollen_1989", "Bollen (1989)",
      paste(
        "Bollen, K. A. (1989). Structural equations with latent variables.",
        "Wiley."
      ),
      "10.1002/9781118619179"
    ),
    nomo_bib_entry(
      "bonifay_2017", "Bonifay et al. (2017)",
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
      "byrne_1989", "Byrne et al. (1989)",
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
      "curran_2016", "Curran (2016)",
      paste(
        "Curran, P. G. (2016). Methods for the detection of carelessly invalid",
        "responses in survey data. Journal of Experimental Social Psychology, 66,",
        "4-19."
      ),
      "10.1016/j.jesp.2015.07.006"
    ),
    nomo_bib_entry(
      "deshon_1998", "DeShon (1998)",
      paste(
        "DeShon, R. P. (1998). A cautionary note on measurement error",
        "corrections in structural equation models. Psychological Methods, 3(4),",
        "412-423."
      ),
      "10.1037/1082-989X.3.4.412"
    ),
    nomo_bib_entry(
      "dunn_2014", "Dunn et al. (2014)",
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
      "flake_2017", "Flake et al. (2017)",
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
        "same data equals trouble: Overfitting in the assessment of internal",
        "structure and some editorial thoughts on it. European Journal of",
        "Psychological Assessment, 33(6), 399-402."
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
      "grice_2001", "Grice (2001)",
      paste(
        "Grice, J. W. (2001). Computing and evaluating factor scores.",
        "Psychological Methods, 6(4), 430-450."
      ),
      "10.1037/1082-989X.6.4.430"
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
      "hancock_2001", "Hancock (2001)",
      paste(
        "Hancock, G. R. (2001). Effect size, power, and sample size determination",
        "for structured means modeling and MIMIC approaches to between-groups",
        "hypothesis testing of means on a single latent construct.",
        "Psychometrika, 66(3), 373-388."
      ),
      "10.1007/BF02294440"
    ),
    nomo_bib_entry(
      "hancock_mueller_2001", "Hancock & Mueller (2001)",
      paste(
        "Hancock, G. R., & Mueller, R. O. (2001). Rethinking construct",
        "reliability within latent variable systems. In R. Cudeck, S. du Toit, &",
        "D. S\u00f6rbom (Eds.), Structural equation modeling: Present and future",
        "(pp. 195-216). Scientific Software International."
      ),
      NA_character_
    ),
    nomo_bib_entry(
      "hayduk_1987", "Hayduk (1987)",
      paste(
        "Hayduk, L. A. (1987). Structural equation modeling with LISREL:",
        "Essentials and advances. Johns Hopkins University Press."
      ),
      NA_character_
    ),
    nomo_bib_entry(
      "henseler_2015", "Henseler et al. (2015)",
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
        "Hu, L.-T., & Bentler, P. M. (1999). Cutoff criteria for fit indexes in",
        "covariance structure analysis: Conventional criteria versus new",
        "alternatives. Structural Equation Modeling, 6(1), 1-55."
      ),
      "10.1080/10705519909540118"
    ),
    nomo_bib_entry(
      "huang_2012", "Huang et al. (2012)",
      paste(
        "Huang, J. L., Curran, P. G., Keeney, J., Poposki, E. M., & DeShon, R. P.",
        "(2012). Detecting and deterring insufficient effort responding to surveys.",
        "Journal of Business and Psychology, 27(1), 99-114."
      ),
      "10.1007/s10869-011-9231-8"
    ),
    nomo_bib_entry(
      "jacobson_truax_1991", "Jacobson & Truax (1991)",
      paste(
        "Jacobson, N. S., & Truax, P. (1991). Clinical significance: A",
        "statistical approach to defining meaningful change in psychotherapy",
        "research. Journal of Consulting and Clinical Psychology, 59(1), 12-19."
      ),
      "10.1037/0022-006X.59.1.12"
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
      "koo_li_2016", "Koo & Li (2016)",
      paste(
        "Koo, T. K., & Li, M. Y. (2016). A guideline of selecting and reporting",
        "intraclass correlation coefficients for reliability research. Journal",
        "of Chiropractic Medicine, 15(2), 155-163."
      ),
      "10.1016/j.jcm.2016.02.012"
    ),
    nomo_bib_entry(
      "kuhn_johnson_2013", "Kuhn & Johnson (2013)",
      "Kuhn, M., & Johnson, K. (2013). Applied predictive modeling. Springer.",
      "10.1007/978-1-4614-6849-3"
    ),
    nomo_bib_entry(
      "lakens_2018", "Lakens et al. (2018)",
      paste(
        "Lakens, D., Scheel, A. M., & Isager, P. M. (2018). Equivalence testing",
        "for psychological research: A tutorial. Advances in Methods and",
        "Practices in Psychological Science, 1(2), 259-269."
      ),
      "10.1177/2515245918770963"
    ),
    nomo_bib_entry(
      "lindell_whitney_2001", "Lindell & Whitney (2001)",
      paste(
        "Lindell, M. K., & Whitney, D. J. (2001). Accounting for common method",
        "variance in cross-sectional research designs. Journal of Applied",
        "Psychology, 86(1), 114-121."
      ),
      "10.1037/0021-9010.86.1.114"
    ),
    nomo_bib_entry(
      "liu_2017", "Liu et al. (2017)",
      paste(
        "Liu, Y., Millsap, R. E., West, S. G., Tein, J.-Y., Tanaka, R., &",
        "Grimm, K. J. (2017). Testing measurement invariance in longitudinal",
        "data with ordered-categorical measures. Psychological Methods, 22(3),",
        "486-506."
      ),
      "10.1037/met0000075"
    ),
    nomo_bib_entry(
      "lorenzoseva_2011", "Lorenzo-Seva et al. (2011)",
      paste(
        "Lorenzo-Seva, U., Timmerman, M. E., & Kiers, H. A. L. (2011). The Hull",
        "method for selecting the number of common factors. Multivariate",
        "Behavioral Research, 46(2), 340-364."
      ),
      "10.1080/00273171.2011.564527"
    ),
    nomo_bib_entry(
      "maccallum_1992", "MacCallum et al. (1992)",
      paste(
        "MacCallum, R. C., Roznowski, M., & Necowitz, L. B. (1992). Model",
        "modifications in covariance structure analysis: The problem of",
        "capitalization on chance. Psychological Bulletin, 111(3), 490-504."
      ),
      "10.1037/0033-2909.111.3.490"
    ),
    nomo_bib_entry(
      "maccallum_1996", "MacCallum et al. (1996)",
      paste(
        "MacCallum, R. C., Browne, M. W., & Sugawara, H. M. (1996). Power",
        "analysis and determination of sample size for covariance structure",
        "modeling. Psychological Methods, 1(2), 130-149."
      ),
      "10.1037/1082-989X.1.2.130"
    ),
    nomo_bib_entry(
      "marjanovic_2015", "Marjanovic et al. (2015)",
      paste(
        "Marjanovic, Z., Holden, R., Struthers, W., Cribbie, R., & Greenglass, E.",
        "(2015). The inter-item standard deviation (ISD): An index that discriminates",
        "between conscientious and random responders. Personality and Individual",
        "Differences, 84, 79-83."
      ),
      "10.1016/j.paid.2014.08.021"
    ),
    nomo_bib_entry(
      "marsh_2004", "Marsh et al. (2004)",
      paste(
        "Marsh, H. W., Hau, K.-T., & Wen, Z. (2004). In search of golden rules:",
        "Comment on hypothesis-testing approaches to setting cutoff values for",
        "fit indexes and dangers in overgeneralizing Hu and Bentler's (1999)",
        "findings. Structural Equation Modeling, 11(3), 320-341."
      ),
      "10.1207/s15328007sem1103_2"
    ),
    nomo_bib_entry(
      "marsh_2014", "Marsh et al. (2014)",
      paste(
        "Marsh, H. W., Morin, A. J. S., Parker, P. D., & Kaur, G. (2014).",
        "Exploratory structural equation modeling: An integration of the best",
        "features of exploratory and confirmatory factor analysis. Annual Review",
        "of Clinical Psychology, 10, 85-110."
      ),
      "10.1146/annurev-clinpsy-032813-153700"
    ),
    nomo_bib_entry(
      "mcgraw_wong_1996", "McGraw & Wong (1996)",
      paste(
        "McGraw, K. O., & Wong, S. P. (1996). Forming inferences about some",
        "intraclass correlation coefficients. Psychological Methods, 1(1),",
        "30-46."
      ),
      "10.1037/1082-989X.1.1.30"
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
      "mcneish_wolf_2020", "McNeish & Wolf (2020)",
      paste(
        "McNeish, D., & Wolf, M. G. (2020). Thinking twice about sum scores.",
        "Behavior Research Methods, 52(6), 2287-2305."
      ),
      "10.3758/s13428-020-01398-0"
    ),
    nomo_bib_entry(
      "meade_craig_2012", "Meade & Craig (2012)",
      paste(
        "Meade, A. W., & Craig, S. B. (2012). Identifying careless responses in",
        "survey data. Psychological Methods, 17(3), 437-455."
      ),
      "10.1037/a0028085"
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
      "muthen_2002", "Muth\u00e9n & Muth\u00e9n (2002)",
      paste(
        "Muth\u00e9n, L. K., & Muth\u00e9n, B. O. (2002). How to use a Monte",
        "Carlo study to decide on sample size and determine power. Structural",
        "Equation Modeling, 9(4), 599-620."
      ),
      "10.1207/S15328007SEM0904_8"
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
      "oberski_satorra_2013", "Oberski & Satorra (2013)",
      paste(
        "Oberski, D. L., & Satorra, A. (2013). Measurement error models with",
        "uncertainty about the error variance. Structural Equation Modeling,",
        "20(3), 409-428."
      ),
      "10.1080/10705511.2013.797820"
    ),
    nomo_bib_entry(
      "podsakoff_2003", "Podsakoff et al. (2003)",
      paste(
        "Podsakoff, P. M., MacKenzie, S. B., Lee, J.-Y., & Podsakoff, N. P.",
        "(2003). Common method biases in behavioral research: A critical review",
        "of the literature and recommended remedies. Journal of Applied",
        "Psychology, 88(5), 879-903."
      ),
      "10.1037/0021-9010.88.5.879"
    ),
    nomo_bib_entry(
      "podsakoff_2012", "Podsakoff et al. (2012)",
      paste(
        "Podsakoff, P. M., MacKenzie, S. B., & Podsakoff, N. P. (2012). Sources",
        "of method bias in social science research and recommendations on how",
        "to control it. Annual Review of Psychology, 63, 539-569."
      ),
      "10.1146/annurev-psych-120710-100452"
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
      "reise_bonifay_2013", "Reise et al. (2013)",
      paste(
        "Reise, S. P., Bonifay, W. E., & Haviland, M. G. (2013). Scoring and",
        "modeling psychological measures in the presence of",
        "multidimensionality. Journal of Personality Assessment, 95(2),",
        "129-140."
      ),
      "10.1080/00223891.2012.725437"
    ),
    nomo_bib_entry(
      "rhemtulla_2012", "Rhemtulla et al. (2012)",
      paste(
        "Rhemtulla, M., Brosseau-Liard, P. \u00c9., & Savalei, V. (2012). When can",
        "categorical variables be treated as continuous? A comparison of robust",
        "continuous and categorical SEM estimation methods under suboptimal",
        "conditions. Psychological Methods, 17(3), 354-373."
      ),
      "10.1037/a0029315"
    ),
    nomo_bib_entry(
      "richardson_2009", "Richardson et al. (2009)",
      paste(
        "Richardson, H. A., Simmering, M. J., & Sturman, M. C. (2009). A tale",
        "of three perspectives: Examining post hoc statistical techniques for",
        "detection and correction of common method variance. Organizational",
        "Research Methods, 12(4), 762-800."
      ),
      "10.1177/1094428109332834"
    ),
    nomo_bib_entry(
      "rodriguez_2016", "Rodriguez et al. (2016)",
      paste(
        "Rodriguez, A., Reise, S. P., & Haviland, M. G. (2016). Evaluating",
        "bifactor models: Calculating and interpreting statistical indices.",
        "Psychological Methods, 21(2), 137-150."
      ),
      "10.1037/met0000045"
    ),
    nomo_bib_entry(
      "roemer_2021", "Roemer et al. (2021)",
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
        "discriminant validity. Organizational Research Methods, 25(1), 6-47."
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
        "analysis of moment structures. In R. D. H. Heijmans, D. S. G. Pollock,",
        "& A. Satorra (Eds.), Innovations in multivariate statistical analysis",
        "(pp. 233-247). Springer."
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
      "savalei_2019", "Savalei (2019)",
      paste(
        "Savalei, V. (2019). A comparison of several approaches for controlling",
        "measurement error in small samples. Psychological Methods, 24(3),",
        "352-370."
      ),
      "10.1037/met0000181"
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
      "shrout_fleiss_1979", "Shrout & Fleiss (1979)",
      paste(
        "Shrout, P. E., & Fleiss, J. L. (1979). Intraclass correlations: Uses in",
        "assessing rater reliability. Psychological Bulletin, 86(2), 420-428."
      ),
      "10.1037/0033-2909.86.2.420"
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
      "simmons_2011", "Simmons et al. (2011)",
      paste(
        "Simmons, J. P., Nelson, L. D., & Simonsohn, U. (2011). False-positive",
        "psychology: Undisclosed flexibility in data collection and analysis",
        "allows presenting anything as significant. Psychological Science,",
        "22(11), 1359-1366."
      ),
      "10.1177/0956797611417632"
    ),
    nomo_bib_entry(
      "spearman_1904", "Spearman (1904)",
      paste(
        "Spearman, C. (1904). The proof and measurement of association between",
        "two things. The American Journal of Psychology, 15(1), 72-101."
      ),
      "10.2307/1412159"
    ),
    nomo_bib_entry(
      "svetina_2020", "Svetina et al. (2020)",
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
      "velicer_2000", "Velicer et al. (2000)",
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
      "weir_2005", "Weir (2005)",
      paste(
        "Weir, J. P. (2005). Quantifying test-retest reliability using the",
        "intraclass correlation coefficient and the SEM. Journal of Strength and",
        "Conditioning Research, 19(1), 231-240."
      ),
      "10.1519/15184.1"
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
      "widaman_2010", "Widaman et al. (2010)",
      paste(
        "Widaman, K. F., Ferrer, E., & Conger, R. D. (2010). Factorial",
        "invariance within longitudinal structural equation models: Measuring",
        "the same construct across time. Child Development Perspectives, 4(1),",
        "10-18."
      ),
      "10.1111/j.1750-8606.2009.00110.x"
    ),
    nomo_bib_entry(
      "williams_2010", "Williams et al. (2010)",
      paste(
        "Williams, L. J., Hartman, N., & Cavazotte, F. (2010). Method variance",
        "and marker variables: A review and comprehensive CFA marker technique.",
        "Organizational Research Methods, 13(3), 477-514."
      ),
      "10.1177/1094428110366036"
    ),
    nomo_bib_entry(
      "williams_hazer_1986", "Williams & Hazer (1986)",
      paste(
        "Williams, L. J., & Hazer, J. T. (1986). Antecedents and consequences of",
        "satisfaction and commitment in turnover models: A reanalysis using",
        "latent variable structural equation methods. Journal of Applied",
        "Psychology, 71(2), 219-231."
      ),
      "10.1037/0021-9010.71.2.219"
    ),
    nomo_bib_entry(
      "wolf_2013", "Wolf et al. (2013)",
      paste(
        "Wolf, E. J., Harrington, K. M., Clark, S. L., & Miller, M. W. (2013).",
        "Sample size requirements for structural equation models: An evaluation",
        "of power, bias, and solution propriety. Educational and Psychological",
        "Measurement, 73(6), 913-934."
      ),
      "10.1177/0013164413495237"
    ),
    nomo_bib_entry(
      "yung_1999", "Yung et al. (1999)",
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
        "changes; with two or more declared scales it is computed within each",
        "item's scale. Descriptive evidence rather than a retention rule."
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
      "long_string", "screen",
      "Long-string analysis",
      "contemporary", "supporting",
      "Longest run of identical consecutive responses for each case.",
      paste(
        "Reads raw responses. Curran's half-the-scale rule of thumb is flagged",
        "as his conservative starting point, which he says is not the best cut",
        "score for every scale. Applied only to item sets long enough for it;",
        "a shorter set reports the run without a flag."
      ),
      "nomo_screen()", "nomologR",
      c("curran_2016", "meade_craig_2012")
    ),
    nomo_method_entry(
      "inter_item_sd", "screen",
      "Inter-item standard deviation",
      "contemporary", "supporting",
      "Within-person standard deviation of responses, averaged within scales.",
      paste(
        "Detects random responding, and gives a respondent who answers every",
        "item identically the best possible score, so it is read alongside",
        "long-string rather than alone. No stated cut score."
      ),
      "nomo_screen()", "nomologR",
      c("marjanovic_2015", "curran_2016")
    ),
    nomo_method_entry(
      "mahalanobis_screen", "screen",
      "Mahalanobis distance screen",
      "contemporary", "supporting",
      "Multivariate distance of each response vector from the sample means.",
      paste(
        "Unaffected by recoding reverse-keyed items. Relies on the covariance",
        "structure of the sample, so extreme but attentive respondents are",
        "distant too. No stated cut score."
      ),
      "nomo_screen()", "nomologR",
      c("meade_craig_2012", "curran_2016")
    ),
    nomo_method_entry(
      "even_odd_consistency", "screen",
      "Even-odd consistency",
      "contemporary", "supporting",
      "Within-person correlation between odd and even halves of each scale.",
      paste(
        "Needs at least three scales and declared reverse keying. Spearman-Brown",
        "corrected and bounded at -1, below which the correction has no meaning.",
        "No stated cut score."
      ),
      "nomo_screen()", "nomologR",
      c("meade_craig_2012", "curran_2016")
    ),
    nomo_method_entry(
      "psychometric_antonyms", "screen",
      "Psychometric antonyms",
      "contemporary", "supporting",
      "Within-person correlation across item pairs with strong negative correlations.",
      paste(
        "Needs at least three pairs. Few pairs make each respondent's value",
        "coarse, so attentive respondents cross zero by chance."
      ),
      "nomo_screen()", "nomologR",
      c("meade_craig_2012", "curran_2016", "huang_2012")
    ),
    nomo_method_entry(
      "psychometric_synonyms", "screen",
      "Psychometric synonyms",
      "contemporary", "supporting",
      "Within-person correlation across item pairs with strong positive correlations.",
      paste(
        "Needs at least three pairs. Few pairs make each respondent's value",
        "coarse, so attentive respondents cross zero by chance."
      ),
      "nomo_screen()", "nomologR",
      c("meade_craig_2012", "curran_2016")
    ),
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
      "Velicer revised MAP (fourth matrix power)",
      "contemporary", "supporting",
      paste(
        "Factor count minimizing the trace of the fourth power of the",
        "partial-correlation matrix, less the number of items p, divided by",
        "p(p - 1). It is a matrix power, not the mean of fourth-power partial",
        "correlations, and can exceed 1."
      ),
      paste(
        "As for original MAP. Velicer et al. (2000) proposed it as a revision of",
        "the original squared-partial criterion."
      ),
      "nomo_factors()", "nomologR",
      c("velicer_2000")
    ),
    nomo_method_entry(
      "ekc", "factors",
      "Empirical Kaiser criterion",
      "contemporary", "supporting",
      "Factor count from eigenvalues compared with a sample-size-aware reference.",
      paste(
        "Reference eigenvalues come from the eigenvalue distribution of",
        "uncorrelated variables, adjusted for sample size and for the variance",
        "the earlier eigenvalues account for. That distribution assumes",
        "product-moment correlations, so with polychoric ones it is an",
        "approximation. Braeken and van Assen (2017) found it about as accurate",
        "as parallel analysis with orthogonal factors and more accurate with",
        "correlated factors measured by few variables."
      ),
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
      paste(
        "Sum of squared correlations divided by the sum of squared correlations",
        "plus squared partial correlations, overall and per item."
      ),
      "Supporting adequacy evidence, not an item-retention rule.",
      "nomo_factors()", "psych",
      c("kaiser_1970", "kaiser_1974")
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
      paste(
        "Common-factor loadings minimizing residual correlations (unweighted",
        "least squares), which psych fits as minres, uls, ols, or old.min."
      ),
      "Models common variance only; distinct from principal components.",
      "nomo_efa()", "psych",
      c("fabrigar_1999")
    ),
    nomo_method_entry(
      "common_factor_extraction", "efa",
      "Other common-factor extraction (ML, principal axis, WLS, GLS)",
      "contemporary", "primary",
      paste(
        "Common-factor loadings from the extraction recorded in fm: maximum",
        "likelihood, principal axis factoring, weighted or generalized least",
        "squares, minimum chi-square, or alpha factoring."
      ),
      paste(
        "Models common variance only, as MINRES does. Maximum likelihood adds",
        "an assumption of multivariate normality and a test of fit; principal",
        "axis factoring makes no distributional assumption."
      ),
      "nomo_efa()", "psych",
      c("fabrigar_1999", "costello_osborne_2005")
    ),
    nomo_method_entry(
      "oblique_rotation", "efa",
      "Oblique rotation",
      "contemporary", "primary",
      paste(
        "Rotated pattern and structure matrices permitting correlated factors,",
        "by the oblique criterion recorded with the solution: oblimin by",
        "default, or promax, geomin, quartimin, simplimax, Bentler's criterion,",
        "or cluster rotation."
      ),
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
      "orthogonal_rotation_other", "efa",
      "Other orthogonal rotation (quartimax, equamax, varimin, geomin, Bentler)",
      "historical", "context",
      paste(
        "Rotated loadings under an imposed zero factor correlation, by an",
        "orthogonal criterion other than varimax."
      ),
      paste(
        "Forces factors to be uncorrelated, as varimax does, which is rarely",
        "defensible for psychological constructs. Available, but recorded as a",
        "choice that needs justification."
      ),
      "nomo_efa()", "psych",
      c("browne_2001", "conway_huffcutt_2003")
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
      "cfa_other_estimator", "cfa",
      "Confirmatory factor analysis with another estimator (such as ULS, GLS, WLS, or DWLS)",
      "contemporary", "primary",
      paste(
        "Loadings, residual variances, and factor covariances for a specified",
        "measurement model, estimated with the estimator the fit records,",
        "other than maximum likelihood or WLSMV for ordered indicators."
      ),
      paste(
        "Each estimator minimizes its own discrepancy function under its own",
        "assumptions, so a methods section names the estimator that ran, which",
        "the fit and its decision log record."
      ),
      "nomo_cfa()", "lavaan",
      c("bollen_1989", "rosseel_2012")
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
      paste(
        "Improvement in fit relative to a null model of uncorrelated observed",
        "variables: the TLI, which Bentler and Bonett (1980) carried into",
        "covariance structure analysis as their non-normed index, and the CFI",
        "(Bentler, 1990)."
      ),
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
        "replicate. Quarantined as post hoc diagnostics; nomologR never applies",
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
        "did not generate the data, so fit alone does not choose it; Bonifay et",
        "al. (2017) treat that superior fit as possible overfitting."
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
        "whenever complexity is left unmodeled."
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
      paste(
        "Difference test for mean- and variance-adjusted estimators, scaled and",
        "shifted so that its mean and variance match a chi-square distribution",
        "on the difference in degrees of freedom, which is left unadjusted."
      ),
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

    nomo_method_entry(
      "rmsea_power", "cfa",
      "Power of the RMSEA tests of close, not-close, and exact fit",
      "contemporary", "supporting",
      paste(
        "The probability that a model's overall fit test rejects its null",
        "RMSEA when the alternative holds, and the sample size that makes it",
        "likely."
      ),
      paste(
        "Concerns the overall fit test, not any one parameter; depends heavily",
        "on the model's degrees of freedom."
      ),
      "nomo_power_rmsea()", "nomologR",
      c("maccallum_1996")
    ),
    nomo_method_entry(
      "monte_carlo_power", "cfa",
      "Monte Carlo power and sample size for a planned model",
      "contemporary", "supporting",
      paste(
        "How often a planned model converges, gives proper solutions, recovers",
        "its parameters without bias, covers them, and detects them, at each",
        "sample size."
      ),
      paste(
        "Only as good as the population model assumed; the references for",
        "bias, coverage, and power guide the choice of N rather than decide it."
      ),
      "nomo_power_simulate()", "lavaan",
      c("muthen_2002", "wolf_2013")
    ),
    nomo_method_entry(
      "cfa_marker_technique", "cfa",
      "Comprehensive CFA marker technique for method variance",
      "contemporary", "supporting",
      paste(
        "Method variance a marker variable carries into a measurement model's",
        "indicators, whether it biases the substantive correlations, and its",
        "share of each factor's reliability."
      ),
      paste(
        "Assumes the marker is theoretically unrelated to, and orthogonal to,",
        "the substantive factors and taps biases in the measurement context;",
        "with a nonideal marker it can detect method variance that is absent,",
        "and it does not recover substantive correlations accurately."
      ),
      "nomo_method_variance()", "lavaan",
      c("williams_2010", "lindell_whitney_2001", "podsakoff_2003",
        "podsakoff_2012", "richardson_2009")
    ),
    nomo_method_entry(
      "esem", "cfa",
      "Exploratory structural equation modeling beside its CFA",
      "contemporary", "supporting",
      paste(
        "Cross-loadings estimated rather than fixed at zero, with target",
        "rotation towards the a priori structure, and the factor correlations",
        "and fit compared with the independent-clusters CFA."
      ),
      paste(
        "The solution depends on the rotation; cross-loadings are evidence",
        "about items, and the CFA remains the more parsimonious account when",
        "ESEM does not fit better on indices that penalize complexity."
      ),
      "nomo_esem()", "lavaan",
      c("asparouhov_muthen_2009", "marsh_2014", "browne_2001")
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
      paste(
        "Reliability of a unit-weighted composite when items are essentially",
        "tau-equivalent with uncorrelated errors; a lower bound when loadings",
        "differ and errors are uncorrelated."
      ),
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

    nomo_method_entry(
      "icc_retest", "reliability",
      "Test-retest intraclass correlation",
      "contemporary", "primary",
      paste(
        "Agreement of scores across occasions: two-way mixed effects, absolute",
        "agreement, single measurement, with the consistency form beside it."
      ),
      paste(
        "Describes stability over the interval studied, and the construct may",
        "itself have changed. The reliability range is read from the",
        "confidence interval."
      ),
      "nomo_retest()", "psych",
      c("shrout_fleiss_1979", "mcgraw_wong_1996", "koo_li_2016")
    ),
    nomo_method_entry(
      "sem_sdc", "reliability",
      "Standard error of measurement and smallest detectable change",
      "contemporary", "supporting",
      paste(
        "The measurement error of one score, and the smallest change unlikely",
        "to be measurement error alone at 95%."
      ),
      "Assumes measurement error is the same across the score range.",
      "nomo_retest()", "nomologR",
      c("nunnally_bernstein_1994", "weir_2005")
    ),
    nomo_method_entry(
      "reliable_change_index", "reliability",
      "Reliable change index",
      "contemporary", "supporting",
      "A person's change divided by the standard error of a difference.",
      paste(
        "Says whether a change exceeds measurement error, not whether it is",
        "meaningful."
      ),
      "nomo_retest()", "nomologR",
      c("jacobson_truax_1991")
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
      c("cheung_rensvold_2002", "chen_2007", "putnick_bornstein_2016")
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

    nomo_method_entry(
      "latent_mean_comparison", "invariance",
      "Latent mean comparison between groups",
      "contemporary", "supporting",
      paste(
        "Groups' latent means relative to a reference group, in its latent",
        "standard deviations: structured-means known-groups evidence."
      ),
      paste(
        "Comparable only when intercepts are invariant, fully or partially;",
        "the reference group is identified with a mean of 0 and a variance of 1."
      ),
      "nomo_invariance()", "lavaan",
      c("byrne_1989", "hancock_2001", "vandenberg_lance_2000")
    ),
    nomo_method_entry(
      "longitudinal_invariance", "invariance",
      "Longitudinal measurement invariance and latent change",
      "contemporary", "primary",
      paste(
        "Equality of the same items' loadings, intercepts or thresholds, and",
        "residual variances across occasions, with each item's unique factors",
        "correlated over time, and the latent change once intercepts are",
        "invariant."
      ),
      paste(
        "The same items at every occasion; latent change is comparable only with",
        "invariant intercepts, fully or partially, and is expressed in the first",
        "occasion's latent standard deviations."
      ),
      "nomo_invariance_longitudinal()", "lavaan",
      c("widaman_2010", "liu_2017", "meredith_1993")
    ),

    # Stage 8: nomological network -------------------------------------------
    nomo_method_entry(
      "unit_weighted_score", "scores",
      "Unit-weighted sum or mean score",
      "historical", "primary",
      "Total or average of a set of item responses, weighting each item equally.",
      paste(
        "Not a model-free calculation. Adding items assumes a parallel model,",
        "with equal unstandardized loadings and equal residual variances, so",
        "it carries the same burden of justification as any other measurement",
        "model. Coefficient alpha is the matching reliability coefficient."
      ),
      "nomo_scores()", "nomologR",
      c("mcneish_wolf_2020")
    ),
    nomo_method_entry(
      "parallel_model_test", "scores",
      "Test of the constraints unit weighting assumes",
      "contemporary", "supporting",
      paste(
        "Chi-square difference between the fitted model and the parallel model",
        "that unit weighting assumes."
      ),
      paste(
        "Compares nested models on the same items and cases. Rejection does not",
        "forbid a sum score; it establishes that the items are not",
        "interchangeable in the way adding them assumes."
      ),
      "nomo_scores()", "lavaan",
      c("mcneish_wolf_2020")
    ),
    nomo_method_entry(
      "factor_score_regression", "scores",
      "Regression (Thurstone) factor scores",
      "historical", "primary",
      "Model-weighted factor score estimates that maximize validity.",
      paste(
        "Indeterminate: infinitely many score sets are consistent with the same",
        "loadings. Maximizes the correlation with its own factor, and is not",
        "univocal, so scores carry variance from the other factors."
      ),
      "nomo_scores()", "lavaan",
      c("grice_2001")
    ),
    nomo_method_entry(
      "factor_score_bartlett", "scores",
      "Bartlett factor scores",
      "historical", "primary",
      "Model-weighted factor score estimates computed from the residual variances.",
      paste(
        "Indeterminate, as all factor scores are. Differs from regression",
        "scores only by a scaling constant when one factor is estimated, so",
        "the choice between them matters only with more than one factor."
      ),
      "nomo_scores()", "lavaan",
      c("grice_2001")
    ),
    nomo_method_entry(
      "factor_score_validity", "scores",
      "Factor score validity",
      "contemporary", "supporting",
      "Correlation between a factor score estimate and the factor it estimates.",
      paste(
        "Equals the factor determinacy coefficient for regression scores, which",
        "maximize it. Gorsuch's (1983) recommendation of at least .80, and",
        "above .90 for scores used as substitutes for the factors, is reported",
        "as his recommendation and not applied."
      ),
      "nomo_scores()", "nomologR",
      c("grice_2001", "gorsuch_1983")
    ),
    nomo_method_entry(
      "factor_score_univocality", "scores",
      "Factor score univocality",
      "contemporary", "supporting",
      "Correlation between a factor score estimate and the factors it does not represent.",
      paste(
        "Requires more than one factor. With correlated factors a score reaches",
        "the others through its own, by the factor correlation times its",
        "validity, so it is judged against that value: only a departure from it",
        "marks a score that is not univocal, which cannot be treated as though",
        "it measured its own factor alone."
      ),
      "nomo_scores()", "nomologR",
      c("grice_2001")
    ),
    nomo_method_entry(
      "factor_score_correlational_accuracy", "scores",
      "Factor score correlational accuracy",
      "contemporary", "supporting",
      paste(
        "Difference between correlations among factor score estimates and",
        "correlations among the factors themselves."
      ),
      paste(
        "Requires more than one factor. A relationship estimated from scores",
        "carries this discrepancy as bias. Its direction depends on the scoring",
        "method and the model, so it is reported rather than corrected for."
      ),
      "nomo_scores()", "nomologR",
      c("grice_2001")
    ),
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
      "A priori versus post hoc prediction provenance",
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

    nomo_method_entry(
      "single_indicator_reliability", "network",
      "Single-indicator latent variables with error variance from reliability",
      "contemporary", "supporting",
      paste(
        "Relations of a composite's true score, with its error variance fixed",
        "at (1 - reliability) times its observed variance."
      ),
      paste(
        "The composite is unidimensional and its reliability captures its",
        "measurement error; alpha understates reliability when loadings differ,",
        "which overcorrects. Standard errors treat the reliability as known",
        "unless its uncertainty is supplied."
      ),
      "nomo_network()", "lavaan",
      c(
        "spearman_1904", "hayduk_1987", "williams_hazer_1986", "bollen_1989",
        "bagozzi_heatherton_1994", "deshon_1998", "oberski_satorra_2013",
        "savalei_2019"
      )
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
      "listwise_deletion", "workflow",
      "Listwise deletion",
      "historical", "context",
      "Parameters estimated from the cases observed on every modeled variable.",
      paste(
        "Requires data missing completely at random; under data missing at",
        "random the complete cases can be unrepresentative. Shown as a",
        "comparison, not recommended."
      ),
      "nomo_missing()", "lavaan",
      c("enders_bandalos_2001", "schafer_graham_2002")
    ),
    nomo_method_entry(
      "pairwise_deletion", "workflow",
      "Pairwise deletion",
      "historical", "context",
      "Each covariance or correlation estimated from the cases observed on that pair.",
      paste(
        "Requires data missing completely at random. The reference strategy",
        "for ordered indicators, for which lavaan does not offer FIML."
      ),
      "nomo_missing()", "lavaan",
      c("enders_bandalos_2001", "schafer_graham_2002")
    ),
    nomo_method_entry(
      "missing_sensitivity", "workflow",
      "Missing-data sensitivity comparison",
      "contemporary", "supporting",
      paste(
        "The same prespecified model refitted under alternative missing-data",
        "strategies, with each estimate's difference from the reference in",
        "standard-error units."
      ),
      paste(
        "Cannot test whether data are missing at random. A difference estimates",
        "the bias of listwise deletion only if they are and the model is correct."
      ),
      "nomo_missing()", "lavaan",
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

  registry <- tibble::tibble(
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
  history <- nomo_methods_history()
  bib <- nomo_bibliography()
  idx <- match(registry$id, history$id)
  origin <- history$origin[idx]
  registry$introduced <- nomo_methods_year(bib$short[match(origin, bib$key)])
  registry$contemporary_practice <- history$contemporary_practice[idx]
  registry
}


# History ----------------------------------------------------------------------
#
# When each method entered the literature, and what took over from each
# historical one: the record of how practice changed (design principle 13).
#
# `origin` is the citation key of the publication that introduced the method.
# It is filled in only when the registry cites that publication. When the
# registry cites only a later review or critique, it is NA, rather than dating
# the method by a secondary source. `introduced` is then derived from the origin
# reference's year, so the two cannot disagree.
#
# `contemporary_practice` names, for a historical method only, the registry's
# contemporary methods that now address the question it answered.
nomo_methods_history <- function() {
  rows <- list(
    c("parallel_analysis", "horn_1965", NA),
    c("map_original", "velicer_1976", NA),
    c("map_revised", "velicer_2000", NA),
    c("ekc", "braeken_vanassen_2017", NA),
    c("nest", "achim_2017", NA),
    c("hull", "lorenzoseva_2011", NA),
    c("comparison_data", "ruscio_roche_2012", NA),
    c("kaiser_guttman", "guttman_1954", "parallel_analysis; map_revised; ekc"),
    c("scree", "cattell_1966", "parallel_analysis"),
    c("kmo", "kaiser_1970", NA),
    c("bartlett", "bartlett_1950", NA),
    c("inter_item_sd", "marjanovic_2015", NA),
    c("orthogonal_rotation", "kaiser_1958", "oblique_rotation"),
    c("orthogonal_rotation_other", NA, "oblique_rotation"),
    c("loading_reference", NA, "loading_diagnostics"),
    c("item_total_reference", NA, "item_rest_correlation"),
    c("icc_retest", NA, NA),
    c("sem_sdc", NA, NA),
    c("reliable_change_index", NA, NA),
    c("ml_cfa", "joreskog_1969", NA),
    c("rmsea_power", "maccallum_1996", NA),
    c("monte_carlo_power", NA, NA),
    c("cfa_marker_technique", "williams_2010", NA),
    c("esem", "asparouhov_muthen_2009", NA),
    c("chisq_exact_fit", "joreskog_1969", "incremental_fit; rmsea_interval; srmr; local_strain"),
    c("incremental_fit", "bentler_bonett_1980", NA),
    c("rmsea_interval", "browne_cudeck_1992", NA),
    c("fit_cutoffs", "hu_bentler_1999", "local_strain"),
    c("modification_indices", NA, "local_strain; revision_lineage"),
    c("bifactor_model", "holzinger_swineford_1937", NA),
    c("lrt_scaled", "satorra_bentler_2001", NA),
    c("lrt_scaled_shifted", "satorra_2000", NA),
    c("nesting_check", "bentler_satorra_2010", NA),
    c("delta_fit", "cheung_rensvold_2002", NA),
    c("information_criteria", "akaike_1974", NA),
    c("alpha", "cronbach_1951", "omega"),
    c("omega_ordinal_scale", "green_yang_2009", NA),
    # Kelley and Pornprasertmanit (2016) evaluated existing interval methods and
    # recommended bootstrap intervals; they did not introduce them (#145, lit-4).
    c("reliability_bootstrap_ci", NA, NA),
    c("omega_hierarchical_subscale", "reise_bonifay_2013", NA),
    c("construct_replicability", "hancock_mueller_2001", NA),
    c("puc", "reise_2012", NA),
    c("schmid_leiman", "schmid_leiman_1957", "bifactor_model"),
    c("standardized_loadings_ave", "fornell_larcker_1981", NA),
    c("fornell_larcker", "fornell_larcker_1981", "htmt2; latent_correlation_ci"),
    c("htmt", "henseler_2015", NA),
    c("htmt2", "roemer_2021", NA),
    c("latent_correlation_ci", "ronkko_cho_2022", NA),
    c("multigroup_cfa", "joreskog_1971", NA),
    c("invariance_hierarchy", "meredith_1993", NA),
    c("categorical_invariance", "wu_estabrook_2016", NA),
    c("invariance_delta_fit", "cheung_rensvold_2002", NA),
    c("partial_invariance", "byrne_1989", NA),
    c("unit_weighted_score", NA, "parallel_model_test"),
    c("parallel_model_test", "mcneish_wolf_2020", NA),
    c("nomological_network", "cronbach_meehl_1955", "two_step_sem; prediction_provenance"),
    c("latent_mean_comparison", NA, NA),
    c("longitudinal_invariance", NA, NA),
    c("two_step_sem", "anderson_gerbing_1988", NA),
    c("single_indicator_reliability", NA, NA),
    c("equivalence_testing", "schuirmann_1987", NA),
    c("prediction_provenance", "nosek_2018", NA),
    c("listwise_deletion", NA, "fiml; missing_sensitivity"),
    c("pairwise_deletion", NA, "fiml; missing_sensitivity")
  )
  tibble::tibble(
    id = vapply(rows, `[[`, character(1), 1L),
    origin = vapply(rows, `[[`, character(1), 2L),
    contemporary_practice = vapply(rows, `[[`, character(1), 3L)
  )
}


# The year in a bibliography entry's short citation, such as "Horn (1965)".
nomo_methods_year <- function(short) {
  as.integer(sub(".*\\((\\d{4})\\).*", "\\1", short))
}


nomo_methods_stages <- function() {
  c(
    "screen", "factors", "efa", "cfa", "compare", "reliability",
    "validity", "invariance", "scores", "network", "workflow"
  )
}


nomo_methods_lineages <- function() {
  c("historical", "contemporary", "emerging")
}


nomo_methods_roles <- function() {
  c("primary", "supporting", "context")
}


# The error for a `stage` or `lineage` value the registry does not have, in the
# package's form for a wrong choice: the argument, the choices, and what was
# given (#144, guide point 26). Both arguments take one or more values.
nomo_methods_choice_error <- function(arg, valid, bad) {
  sprintf(
    "`%s` must be one or more of %s, not %s.",
    arg,
    nomo_present_or(sprintf('"%s"', valid)),
    paste(sprintf('"%s"', bad), collapse = ", ")
  )
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
#' Two columns record how practice changed:
#'
#' * `introduced`: the year of the publication that introduced the method.
#'   It is given only when the registry cites that publication, and is `NA`
#'   when the registry cites only a later review or critique. It is never
#'   dated from a secondary source.
#' * `contemporary_practice`: for a historical method, the registry's
#'   contemporary methods that now address the question it answered, such as
#'   parallel analysis for the eigenvalue-greater-than-one rule, or omega for
#'   coefficient alpha.
#'
#' `vignette("research-basis")` draws a timeline from these columns.
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
#' actually used, which is what [nomo_report()] cites. They are read from what
#' the object records: the estimator that ran, the extraction and rotation of
#' an exploratory solution, and an equivalence test only where one could be
#' computed.
#'
#' @param x Optional `nomologR` result object. If supplied, only the methods
#'   actually used in producing that object are returned, in registry order. If
#'   `NULL` (default), the whole registry is returned.
#' @param stage Optional character vector restricting the result to these
#'   workflow stages. One or more of `"screen"`, `"factors"`, `"efa"`, `"cfa"`,
#'   `"compare"`, `"reliability"`, `"validity"`, `"invariance"`, `"scores"`,
#'   `"network"`, `"workflow"`.
#' @param lineage Optional character vector restricting the result to
#'   `"historical"`, `"contemporary"`, or `"emerging"` methods.
#' @param references If `FALSE` (default), each row is one method and the
#'   `references` column holds short in-text citations. If `TRUE`, the result is
#'   expanded to one row per method per reference, with full `citation` and
#'   `doi` columns, suitable for a reference list.
#'
#' @return A tibble with columns `id`, `stage`, `method`, `lineage`, `role`,
#'   `estimand`, `assumptions`, `implemented_by`, `engine`, `introduced`, and
#'   `contemporary_practice`. With `references = FALSE` there is one row per
#'   method, and `references` holds short in-text citations. With
#'   `references = TRUE` there is one row per method-reference pair, with
#'   `citation_key`, `citation`, and `doi`.
#'
#'   The result is an ordinary tibble with no `print()` or `summary()` method
#'   of its own, so the console shortens its long text columns; select columns,
#'   or call `as.list()` on one row, to read an entry in full.
#' @export
#'
#' @examples
#' # The whole registry, one row per method
#' methods <- nomo_methods()
#' methods[, c("method", "lineage", "role")]
#'
#' # Everything the registry records about one method
#' as.list(methods[methods$id == "omega", ])
#'
#' # What is shown only as historical context, and why
#' nomo_methods(lineage = "historical")[, c("method", "role", "assumptions")]
#'
#' # How practice changed: historical methods and what now does their work
#' old <- nomo_methods(lineage = "historical")
#' old[!is.na(old$contemporary_practice),
#'     c("introduced", "method", "contemporary_practice")]
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
    if (length(bad)) stop(nomo_methods_choice_error("stage", valid, bad), call. = FALSE)
    registry <- registry[registry$stage %in% stage, , drop = FALSE]
  }

  if (!is.null(lineage)) {
    valid <- nomo_methods_lineages()
    bad <- setdiff(lineage, valid)
    if (length(bad)) stop(nomo_methods_choice_error("lineage", valid, bad), call. = FALSE)
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
