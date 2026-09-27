# the CFA print and summary read as designed (#89)

    Code
      print(cfa)
    Output
      <nomo_cfa> Confirmatory factor analysis
      Cases: 473 of 500 used | Estimator: ML | Converged: yes
      Fit: CFI 0.973 | TLI 0.965 | RMSEA 0.051 | SRMR 0.052
      Loadings: 10 | Flags: 1 review, 0 concern
      No parameter was freed and no model was refit automatically. summary() shows
      the evidence.

---

    Code
      print(summary(cfa))
    Output
      <nomo_cfa summary> Confirmatory factor analysis
      Cases: 473 of 500 used (27 not used, 5.4%) | Estimator: ML | Converged: yes
      
      Global fit
        chi-square(34) = 75.83, p < .001
        Index  Value  90% CI          Reference
        CFI    0.973                      0.950
        TLI    0.965                      0.950
        RMSEA  0.051  [0.036, 0.066]      0.060
        SRMR   0.052                      0.080
        References are teaching values for review, not cutoffs.
      
      Standardized loadings
        Factor  Item  Loading     SE  95% CI          Flag
        A       a1      0.771  0.025  [0.723, 0.820]
        A       a2      0.744  0.026  [0.692, 0.795]
        A       a3      0.669  0.030  [0.609, 0.728]
        A       a4      0.738  0.026  [0.687, 0.790]
        A       a5      0.598  0.034  [0.531, 0.665]
        B       b1      0.794  0.025  [0.746, 0.843]
        B       b2      0.695  0.030  [0.636, 0.753]
        B       b3      0.757  0.026  [0.705, 0.809]
        B       b4      0.628  0.033  [0.563, 0.693]
        B       b5      0.337  0.045  [0.249, 0.426]  review
      
      Flagged loadings
        - b5 on B (review): Absolute standardized loading is below the configured
          teaching reference of 0.5; inspect item content, precision, and model
          specification.
      
      Factor correlations
        Factor 1  Factor 2      r  95% CI
        A         B         0.499  [0.413, 0.586]
      
      Improper solutions
        No improper-solution signal, such as a negative residual variance, was
        detected.
      
      Largest residual correlations
        Item 1  Item 2  Residual
        b3      a5         0.189
        b1      a5         0.187
        b2      a5         0.144
        b4      a5         0.139
        b3      a4        -0.067
      
      Modification indices (diagnostic only)
        Parameter     MI     EPC  Std. EPC
        B =~ a5    48.34   0.488     0.365
        B =~ a1     9.41  -0.183    -0.145
        a5 ~~ b3    7.57   0.081     0.150
        a5 ~~ b1    4.78   0.061     0.125
        a3 ~~ a4    4.64   0.073     0.133
        Modification indices locate strain. They do not authorize freeing a
        parameter, and nomologR never does so automatically.
      
      Global fit, local strain, and parameter estimates are evidence to interpret
      together; no single cutoff establishes model validity.

# the item audit, factor retention, and EFA read as designed (#89)

    Code
      print(scr)
    Output
      <nomo_screen> Item and data audit
      Cases: 500 | Candidate items: 10
      Items with missing responses: 2 | Constant: 0 | All missing: 0
      Relationship diagnostics: 10 eligible items | 10 item-rest estimates
      Response concentration flags: 0 | Near-zero variance: 0
      Decision log: 3 info, 1 review, 0 concern
      No rows or items were removed or modified.

---

    Code
      print(summary(scr))
    Output
      <nomo_screen summary> Item and data audit
      Cases: 500 | Items: 10 | Flags: 1 review, 0 concern
      Items with missing responses: 2 | Constant: 0 | All missing: 0
      Relationship eligible: 10
      
      Item review
        Item  Type        Missing  Top share  Item-rest r  Flag
        a1    continuous     0.0%       1.0%        0.559
        a2    continuous     3.0%       1.2%        0.580
        a3    continuous     0.0%       1.0%        0.511
        a4    continuous     0.0%       1.2%        0.549
        a5    continuous     0.0%       1.6%        0.588
        b1    continuous     0.0%       1.6%        0.602
        b2    continuous     0.0%       1.4%        0.513
        b3    continuous     2.4%       1.0%        0.571
        b4    continuous     0.0%       1.2%        0.481
        b5    continuous     0.0%       1.4%        0.279  review
        Top share is the proportion of responses in the most common category.
      
      Flagged items
        - b5 (review): `b5` has a corrected item-rest correlation of r = 0.28 (n =
          473), below the teaching reference.
      
      Flags are review aids, not decisions to keep or delete an item.

---

    Code
      print(fac)
    Output
      <nomo_factors> Factor-retention evidence
      Cases: 500 | Items: 10 | Correlation: pearson
      Criterion set: core | Available methods: 3 | Families: 2 | Skipped: 1
      Parallel analysis (percentile): 2 | MAP TR2/TR4: 2/2 | KMO: 0.874
      All 2 available criterion families (3 methods) point to 2 factors. Related
      methods within a family are grouped before concordance is summarized; this is
      strong converging evidence for investigating that solution, not proof of
      dimensionality. 1 requested method was not evaluated; see criterion status for
      the documented reason.

---

    Code
      print(summary(fac))
    Output
      <nomo_factors summary> Factor-retention evidence
      Cases: 500 | Items: 10 | Correlation: pearson | Criteria: core
      
      Retention evidence
        Method              Factors  Role
        Parallel analysis         2  primary
        MAP (original TR2)        2  complementary
        MAP (revised TR4)         2  complementary
      
      Parallel-analysis rule sensitivity
        Rule        Factors  Used
        percentile        2  selected
        mean              2
        crawford          2
      
      Criteria requested but not run
        - Empirical Kaiser criterion: EKC needs one common sample size for the
          analyzed matrix; pairwise missing-data handling produced varying pairwise
          Ns.
      
      Concordance across criterion families
        Factors  Families  Which
              2         2  Parallel analysis; MAP
      
      Supporting adequacy evidence
        - KMO: 0.874
        - Bartlett: Bartlett's test was not computed because pairwise missing-data
          handling does not provide one common sample size for the full matrix.
      
      Synthesis
        All 2 available criterion families (3 methods) point to 2 factors. Related
        methods within a family are grouped before concordance is summarized; this
        is strong converging evidence for investigating that solution, not proof of
        dimensionality. 1 requested method was not evaluated; see criterion status
        for the documented reason.
      
      Factor counts are candidates for investigation, not automatic dimensionality
      verdicts. Common-factor eigenvalues come from a reduced common-variance
      matrix; later values can be negative.

---

    Code
      print(efa)
    Output
      <nomo_efa> Exploratory factor analysis
      Cases: 500 | Items: 10 | Factors: 2 (nomo_factors() handoff)
      Correlation: pearson | Extraction: minres | Rotation: oblimin
      Off-diagonal RMSR: 0.018 | Flags: 2 review, 1 concern
      No items were automatically deleted or refit.

---

    Code
      print(summary(efa))
    Output
      <nomo_efa summary> Exploratory factor analysis
      Cases: 500 | Items: 10 | Factors: 2 (from nomo_factors())
      Correlation: pearson | Extraction: minres | Rotation: oblimin
      Supporting adequacy: KMO 0.874
      
      Item structure
        Item  Factor  Loading  Next factor  Loading  Communality  Flag
        a1    F1        0.808  F2            -0.044        0.624
        a2    F1        0.709  F2             0.064        0.547
        a3    F1        0.667  F2             0.015        0.454
        a4    F1        0.770  F2            -0.038        0.569
        a5    F1        0.414  F2             0.335        0.406  review
        b1    F2        0.783  F1             0.014        0.623
        b2    F2        0.693  F1            -0.020        0.469
        b3    F2        0.783  F1            -0.007        0.608
        b4    F2        0.637  F1            -0.012        0.400  review
        b5    F2        0.332  F1             0.029        0.120  concern
      
      Flagged items
        - a5 (review): secondary loading |0.34| meets/exceeds the 0.30 cross-loading
          reference
        - b4 (review): communality 0.40 is below the 0.40 teaching reference
        - b5 (concern): primary loading |0.33| is below the 0.40 teaching reference;
          communality 0.12 is below the 0.40 teaching reference
      
      Factor correlations
        Factor 1  Factor 2      r
        F1        F2        0.439
      
      Largest residual correlations
        Off-diagonal RMSR: 0.018
        Item 1  Item 2  Residual
        b4      b5         0.042
        a5      b5        -0.039
        a4      b5         0.038
        a2      a5         0.030
        b2      b5        -0.028
      
      Numerical references trigger inspection, not automatic deletion or hidden
      refitting.

# reliability, validity, scores, and missing-data output read as designed (#89)

    Code
      print(rel)
    Output
      <nomo_reliability> Reliability
      Constructs: 2 | Primary coefficient: model-based omega | Review reference: 0.7
      Omega range: 0.784 to 0.835 | Flags: none
      Alpha: 2 of 2 construct(s) available as a secondary coefficient
      Uncertainty: point estimates only; use `ci = "bootstrap"` for interval
      estimates.
      Reference values guide review; they are not pass/fail reliability rules.

---

    Code
      print(summary(rel))
    Output
      <nomo_reliability summary> Reliability
      
      Coefficients
        Construct  Indicators  Omega  Alpha  Omega scale
        A          continuous  0.835  0.827  observed continuous
        B          continuous  0.784  0.771  observed continuous
      
      Sampling uncertainty was not bootstrapped. For report-ready intervals, rerun
      with `ci = "bootstrap"`.
      Omega is primary for the congeneric CFA workflow; alpha is secondary and
      assumption-dependent. Reliability contributes score-precision evidence, not
      construct validity.

---

    Code
      print(val)
    Output
      <nomo_validity> Convergent and discriminant evidence
      Constructs: 2 | AVE review reference: 0.5 | HTMT-family review reference: 0.85
      Convergent evidence: 2 review, 0 concern across 2 construct(s)
      Construct separation: none across 1 pair(s)
      No single index is treated as a declaration that a construct is valid or
      invalid.

---

    Code
      print(summary(val))
    Output
      <nomo_validity summary> Convergent and discriminant evidence
      
      Convergent evidence by construct
        Construct    AVE  Min |loading|  Median |loading|  Loadings flagged  Flag
        A          0.497          0.598             0.738                 0  review
        B          0.434          0.337             0.695                 1  review
      
      Construct separation
        Construct 1  Construct 2  Latent r  95% CI          HTMT2   HTMT
        A            B               0.499  [0.413, 0.586]  0.533  0.540
      
      Standardized loadings and AVE address convergent evidence; latent correlations
      and HTMT-family statistics address construct separation. These are
      complementary questions, not interchangeable pass/fail tests.

---

    Code
      print(sc)
    Output
      <nomo_scores> Scores
      Unit weighting (method: sum) | 2 factor(s) | 473 scored case(s)
      
      Score properties (Grice, 2001)
        Factor  Items  Validity  Univocality  Correlational accuracy
        A           5     0.911       +0.455                  -0.097
        B           5     0.885       +0.442                  -0.097
      
      Parallel model (what unit weighting assumes)
        chi-square difference 171.84 on 16 df, p < .001
      
      Notes
        - review: The parallel model that unit weighting assumes fits worse than the
          model you fitted (chi-square difference 171.84 on 16 df, p < .001). The
          items are not interchangeable in the way adding them assumes. This does
          not forbid a sum score; it means the choice needs a reason beyond
          convenience, and that `validity` and `correlational_accuracy` describe
          what it costs.
        - review: Validity is below .90 for B. Gorsuch (1983, p. 260) recommended at
          least .80, and above .90 if the scores are to serve as adequate
          substitutes for the factors themselves. Reported as his recommendation,
          not applied as a rule.
        - concern: Correlations among these scores do not reproduce the correlations
          among the factors: the largest discrepancy is -0.097, for B. A
          relationship estimated from these scores carries that much bias, and its
          direction is a property of the method and the model rather than a constant
          that can be corrected for. Where the question can be asked of the latent
          variables, ask it there. For a linear regression among factors, Skrondal
          and Laake (2001) showed a scoring design that gives consistent
          coefficients, and scores from one model containing every factor, like
          these, are not it: the predictors need regression-method scores and the
          outcome Bartlett scores, each from a measurement model of its own.
        - review: These scores also carry the other factors: the score for A
          correlates +0.455 with a factor it does not represent (Grice, 2001). A
          score that is not univocal cannot be treated as though it measured its own
          factor alone.
        - review: The strongest standardized loading is at least twice the weakest
          for B. Adding those items gives the weakest indicator the same say as the
          strongest, so two people with the same total can differ on the construct
          by having endorsed different items.
      
      No value here is a pass/fail threshold; see nomo_table(x, "diagnostics").

---

    Code
      print(summary(sc))
    Output
      <nomo_scores summary> Scores
      Unit weighting (method: sum) | 473 scored case(s)
      
      Score properties (Grice, 2001)
        Factor  Items  Validity  Univocality  Correlational accuracy
        A           5     0.911       +0.455                  -0.097
        B           5     0.885       +0.442                  -0.097
      
      Standardized loading spread
        Factor  Lowest  Highest  Ratio
        A         0.60     0.77   1.29
        B         0.34     0.79   2.35
      
      Notes
        - A score is not the latent variable. Every method here produces an estimate
          whose correlation with its own factor is below one, reported as `validity`
          (Grice, 2001). Where a later question can be asked of the latent variables
          directly, asking it of scores replaces an unbiased answer with a biased
          one.
        - Unit weighting is not a model-free calculation. McNeish and Wolf (2020)
          show that adding items assumes a parallel model: equal unstandardized
          loadings and equal residual variances. That assumption needs the same
          justification as any other measurement model.
        - review: The parallel model that unit weighting assumes fits worse than the
          model you fitted (chi-square difference 171.84 on 16 df, p < .001). The
          items are not interchangeable in the way adding them assumes. This does
          not forbid a sum score; it means the choice needs a reason beyond
          convenience, and that `validity` and `correlational_accuracy` describe
          what it costs.
        - Coefficient alpha is the reliability coefficient for a unit-weighted
          scale; coefficient H belongs to optimally weighted scores (McNeish & Wolf,
          2020). Reporting H for a sum score, or alpha for a weighted one, describes
          a scale that was not used.
        - review: Validity is below .90 for B. Gorsuch (1983, p. 260) recommended at
          least .80, and above .90 if the scores are to serve as adequate
          substitutes for the factors themselves. Reported as his recommendation,
          not applied as a rule.
        - concern: Correlations among these scores do not reproduce the correlations
          among the factors: the largest discrepancy is -0.097, for B. A
          relationship estimated from these scores carries that much bias, and its
          direction is a property of the method and the model rather than a constant
          that can be corrected for. Where the question can be asked of the latent
          variables, ask it there. For a linear regression among factors, Skrondal
          and Laake (2001) showed a scoring design that gives consistent
          coefficients, and scores from one model containing every factor, like
          these, are not it: the predictors need regression-method scores and the
          outcome Bartlett scores, each from a measurement model of its own.
        - review: These scores also carry the other factors: the score for A
          correlates +0.455 with a factor it does not represent (Grice, 2001). A
          score that is not univocal cannot be treated as though it measured its own
          factor alone.
        - review: The strongest standardized loading is at least twice the weakest
          for B. Adding those items gives the weakest indicator the same say as the
          strongest, so two people with the same total can differ on the construct
          by having endorsed different items.

---

    Code
      print(nomo_missing(cfa, data = nomo_demo_continuous))
    Output
      <nomo_missing> Missing-data sensitivity
      Model: nomo_cfa | Reference: FIML | Fitted with: Listwise deletion
      Cases: 27 of 500 incomplete (5.4%) | Patterns: 3
      Lowest covariance coverage: 0.946 (a2, b3)
      
      Strategies
        Strategy           lavaan    Needs  Role          N  Converged  Admissible
        Listwise deletion  listwise  MCAR   comparison  473  yes        yes
        FIML               ml        MAR    reference   500  yes        yes
      
      Largest differences from the reference, in reference standard errors
        Parameter  Strategy           Estimate  Reference  Difference (SE)
        A ~~ B     Listwise deletion     0.499      0.480            +0.45
        B =~ b5    Listwise deletion     0.337      0.353            -0.37
        A =~ a5    Listwise deletion     0.598      0.590            +0.23
        A =~ a3    Listwise deletion     0.669      0.675            -0.21
        B =~ b3    Listwise deletion     0.757      0.762            -0.17
      
      Whether data are missing at random cannot be tested from these data; see
      nomo_table(x, "decision_log").

# hierarchical output reads as designed (#89)

    Code
      print(hier)
    Output
      <nomo_hierarchical> Hierarchical model evaluation
      Bifactor model | General factor: G | Group factors: A, B, C
      Estimand: unit-weighted observed composite
      
      Total score
        Index                        Estimate
        omega total                     0.896
        omega hierarchical              0.741
        omega hierarchical relative     0.827
        ECV                             0.609
        PUC                             0.750
      
      Subscales
        Subscale  Items  Omega subscale  Omega hierarchical subscale
        A             3           0.798                        0.330
        B             3           0.791                        0.354
        C             3           0.799                        0.239
      
      Factor scores
        Factor  Role     Determinacy  Min competing r  Replicability H
        G       general        0.867            0.502            0.829
        A       group          0.704           -0.009            0.489
        B       group          0.715            0.023            0.501
        C       group          0.641           -0.178            0.406
      
      Notes
        - review: A bifactor model will usually fit at least as well as
          correlated-factors or higher-order models of the same items, even when it
          did not generate the data (Reise, 2012), and a higher-order model is a
          constrained version of it (Yung, Thissen, & McLeod, 1999). Bonifay, Lane,
          and Reise (2017) call the bifactor model's tendency to show superior
          goodness of fit in model comparison studies a particular concern, and say
          that superior fit may be a symptom of overfitting: modeling not only the
          trends in the data but also unwanted noise. Murray and Johnson (2013)
          compared these two structures directly and found the comparison biased in
          favor of the bifactor model: unless there was essentially no unmodeled
          complexity, their simulation favored the bifactor model even when a
          higher-order model generated the data. They concluded that which model to
          adopt should not rely on which is better fitting. Compare the alternatives
          with nomo_compare() and choose on substantive grounds, not on fit alone.
        - review: Factor determinacy is at or below .90 for G, A, B, C. Gorsuch
          (1983, p. 260) recommended using factor score estimates only above that
          value. This is his recommendation reported as context, not a rule applied
          here; the score may still be usable for some purposes.
        - review: Two equally valid sets of factor scores could correlate as low as
          G (0.50), A (-0.01), B (0.02), C (-0.18). Gorsuch (1983, p. 260) suggested
          this minimum be above .70. A negative value means two researchers scoring
          the same data could rank people in opposite orders and both be consistent
          with the model.
        - review: Construct replicability H is below .70 for A, B, C. Hancock and
          Mueller (2001) proposed .70 as a standard; a factor below it is not well
          defined by its own indicators and is expected to change across studies.
          Reported as their standard, not applied as a rule.
      
      No index is treated as a pass/fail threshold; see nomo_table(x, "indices").

---

    Code
      print(summary(hier))
    Output
      <nomo_hierarchical summary> Hierarchical model evaluation
      Bifactor model | General factor: G
      
      Total score
        - omega total = 0.896: Common sources explain 0.90 of the variance of the
          unit-weighted total score.
        - omega hierarchical = 0.741: The general factor (G) explains 0.74 of the
          variance of the unit-weighted total score.
        - omega hierarchical relative = 0.827: Of the total score's reliable
          variance, 0.83 reflects the general factor (G) and the rest reflects group
          factors.
        - ECV = 0.609: The general factor (G) explains 0.61 of the common variance
          across items. Higher values indicate a stronger general factor relative to
          the group factors; no benchmark value establishes that the items are
          unidimensional.
        - PUC = 0.750: 0.75 of item correlations are influenced only by the general
          factor. When this is very high, even a modest ECV can yield relatively
          unbiased estimates from a unidimensional model.
      
      Subscales
        Subscale  Items  Omega subscale  Omega hierarchical subscale
        A             3           0.798                        0.330
        B             3           0.791                        0.354
        C             3           0.799                        0.239
      
      Notes
        - Bifactor model: G is measured by all 9 items, with 3 group factor(s) (A,
          B, C).
        - review: A bifactor model will usually fit at least as well as
          correlated-factors or higher-order models of the same items, even when it
          did not generate the data (Reise, 2012), and a higher-order model is a
          constrained version of it (Yung, Thissen, & McLeod, 1999). Bonifay, Lane,
          and Reise (2017) call the bifactor model's tendency to show superior
          goodness of fit in model comparison studies a particular concern, and say
          that superior fit may be a symptom of overfitting: modeling not only the
          trends in the data but also unwanted noise. Murray and Johnson (2013)
          compared these two structures directly and found the comparison biased in
          favor of the bifactor model: unless there was essentially no unmodeled
          complexity, their simulation favored the bifactor model even when a
          higher-order model generated the data. They concluded that which model to
          adopt should not rely on which is better fitting. Compare the alternatives
          with nomo_compare() and choose on substantive grounds, not on fit alone.
        - Factor determinacy is the correlation between a factor and its estimated
          factor score (Beauducel, 2011; Rodriguez, Reise, & Haviland, 2016). It is
          computed from the whole model-reproduced correlation matrix, so a group
          factor's score can use the other items to partial out the general factor.
          Construct replicability H (Hancock & Mueller, 2001) uses only that
          factor's own loadings and treats the rest of each item as uncorrelated
          residual. The two are equivalent when the data are unidimensional and can
          differ under a bifactor model, which Rodriguez et al. note without
          preferring either. Read each as the question it answers.
        - review: Factor determinacy is at or below .90 for G, A, B, C. Gorsuch
          (1983, p. 260) recommended using factor score estimates only above that
          value. This is his recommendation reported as context, not a rule applied
          here; the score may still be usable for some purposes.
        - review: Two equally valid sets of factor scores could correlate as low as
          G (0.50), A (-0.01), B (0.02), C (-0.18). Gorsuch (1983, p. 260) suggested
          this minimum be above .70. A negative value means two researchers scoring
          the same data could rank people in opposite orders and both be consistent
          with the model.
        - review: Construct replicability H is below .70 for A, B, C. Hancock and
          Mueller (2001) proposed .70 as a standard; a factor below it is not well
          defined by its own indicators and is expected to change across studies.
          Reported as their standard, not applied as a rule.
        - Each omega describes a unit-weighted observed composite, with observed
          covariances in the denominator.

# comparison, invariance, and network output read as designed (#89)

    Code
      print(cmp)
    Output
      <nomo_compare> Measurement-model comparison
      Models: 2 | Reference: full | Estimator: ML | Cases: 473 | Origin: a-priori
      Rationale: Is b5 needed?
      
      Compared with `full`
        - no_b5 (nested, more constrained): chi-square difference = 46.91, df = 1, p
          < .001; CFI change -0.029, RMSEA change +0.022; AIC change +44.9
      
      No model was selected automatically. summary() shows interpretations and
      measurement evidence.

---

    Code
      print(summary(cmp))
    Output
      <nomo_compare summary> Measurement-model comparison
      Rationale: Is b5 needed?
      Origin: a-priori | Reference model: full
      
      Model fit
        Model  Parameters  df  Chi-square    CFI    TLI  RMSEA   SRMR
        full           21  34       75.83  0.973  0.965  0.051  0.052
        no_b5          20  35      122.75  0.944  0.928  0.073  0.093
      
      Information criteria
        Model      AIC      BIC  Loadings fixed to zero
        full   11981.0  12068.4                       0
        no_b5  12025.9  12109.1                       1
      
      Difference tests against the reference model
        Model  Relation                  Check   Method    Chi-sq diff  df       p
        no_b5  nested, more constrained  nested  standard        46.91   1  < .001
      
      Changes in fit (model minus reference)
        Model     CFI     TLI   RMSEA    SRMR    AIC    BIC
        no_b5  -0.029  -0.036  +0.022  +0.042  +44.9  +40.8
      
      Interpretation
        - `no_b5` is nested within `full` and has 1 more degree(s) of freedom
          (additional constraints). Chi-Squared Difference Test: chi-square
          difference = 46.91, df = 1, p < .001. A small p-value indicates that the
          extra constraints are not fully consistent with the data; with large
          samples, even small misspecifications produce small p-values. Change in
          fit (`no_b5` minus `full`): CFI -0.029, TLI -0.036, RMSEA +0.022, SRMR
          +0.042. AIC +44.9 and BIC +40.8 (`no_b5` minus `full`); lower values favor
          a model for these data, and only differences are interpretable. No model
          is selected automatically; read this evidence with theory and the recorded
          rationale.
      
      Standardized loadings by model
        Factor  Item   full  no_b5
        A       a1    0.771  0.771
        A       a2    0.744  0.744
        A       a3    0.669  0.669
        A       a4    0.738  0.738
        A       a5    0.598  0.598
        B       b1    0.794  0.795
        B       b2    0.695  0.699
        B       b3    0.757  0.757
        B       b4    0.628  0.624
        B       b5    0.337  0.000
      
      Measurement evidence by model
        Construct  Metric   full  no_b5
        A          omega   0.835  0.835
        B          omega   0.784  0.622
        A          alpha   0.827  0.827
        B          alpha   0.771  0.771
        A          AVE     0.497  0.497
        B          AVE     0.434  0.521
        B vs A     HTMT2   0.533  0.533
        - Loading(s) fixed to zero for b5 keep those item(s) in this composite; the
          coefficient does not describe a shortened scale.
      
      No model was selected automatically. Difference tests, changes in fit,
      information criteria, and measurement evidence answer different questions;
      read them together with theory and the recorded rationale.

---

    Code
      print(inv)
    Output
      <nomo_invariance> Measurement invariance
      Grouping variable: group (2 groups: online, paper) | Indicators: continuous
      Requested: configural -> metric -> scalar
      Completed: configural -> metric -> scalar
      
        Level         CFI  RMSEA   SRMR  CFI change  RMSEA change   LRT p
        configural  1.000  0.000  0.002           -             -       -
        metric      1.000  0.000  0.028      +0.000        +0.000    .146
        scalar      0.954  0.121  0.065      -0.046        +0.121  < .001
      Localized equality-constraint diagnostics retained: 12
      
      Fit changes and score diagnostics are evidence. They are not pass/fail rules,
      and nomologR never frees a parameter because of them.

---

    Code
      print(summary(inv))
    Output
      <nomo_invariance summary> Measurement invariance
      Indicators: continuous | Groups: online, paper
      Levels completed: configural -> metric -> scalar
      
      Identification and sequence
        Continuous indicators use the conventional configural, metric, scalar, and
        strict sequence.
      
      Fit by level
        Level       Constraints           Chi-square  df       p    CFI  RMSEA   SRMR
        configural  none                        0.33   4    .988  1.000  0.000  0.002
        metric      loadings                    5.71   7    .575  1.000  0.000  0.028
        scalar      loadings, intercepts       68.93  10  < .001  0.954  0.121  0.065
      
      Changes from the preceding level
        Level   CFI change  RMSEA change  SRMR change  LRT chi-square  df       p
        metric      +0.000        +0.000       +0.026            5.38   3    .146
        scalar      -0.046        +0.121       +0.037           63.23   3  < .001
      
      Largest equality-constraint score diagnostics (diagnostic only)
        Level   Constraint                                 Score  df       p
        metric  Loading: Agency -> ag3 (online vs. paper)   4.92   1    .027
        metric  Loading: Agency -> ag2 (online vs. paper)   1.03   1    .310
        metric  Loading: Agency -> ag4 (online vs. paper)   0.69   1    .406
        metric  Loading: Agency -> ag1 (online vs. paper)   0.03   1    .869
        scalar  Intercept: ag3 (online vs. paper)          61.12   1  < .001
        scalar  Intercept: ag1 (online vs. paper)          11.32   1  < .001
        scalar  Intercept: ag4 (online vs. paper)           5.56   1    .018
        scalar  Intercept: ag2 (online vs. paper)           0.98   1    .323
        scalar  Loading: Agency -> ag2 (online vs. paper)   0.55   1    .460
        scalar  Loading: Agency -> ag1 (online vs. paper)   0.32   1    .574
      
      No single delta-CFI, delta-RMSEA, delta-SRMR, chi-square difference, or score
      diagnostic is treated as a universal invariance rule.

---

    Code
      print(nomo_partial(level = "scalar", syntax = "ag3 ~ 1", rationale = "Anticipated mode difference."))
    Output
      <nomo_partial> Partial invariance releases
      1 researcher-specified release(s)
      
        - P1 (scalar): ag3 ~ 1. Anticipated mode difference.
      
      No release was selected automatically by nomologR.

---

    Code
      print(h)
    Output
      <nomo_hypotheses> Theory-specified relations
      3 theory-specified relation(s)
      
      Every relation is on the standardized scale.
        ID  Relation                       Prediction  Region         Origin
        H1  Agency -> Persistence          positive    [0.2, +Inf)    a priori
        H2  Agency <-> SocialDesirability  negligible  [-0.15, 0.15]  a priori
        H3  Agency -> Performance          positive    (0, +Inf)      a priori

---

    Code
      print(summary(h))
    Output
      <nomo_hypotheses summary> Theory-specified relations
      Relations: 3 | A priori: 3 | Post hoc: 0
      Quantitatively confirmable with the supplied specification: 3/3
      
      Every relation is on the standardized scale.
        ID  Relation                       Prediction  Region         Origin
        H1  Agency -> Persistence          positive    [0.2, +Inf)    a priori
        H2  Agency <-> SocialDesirability  negligible  [-0.15, 0.15]  a priori
        H3  Agency -> Performance          positive    (0, +Inf)      a priori

---

    Code
      print(net)
    Output
      <nomo_network> Nomological network
      Primary sample: N = 800 | Converged: yes
      Theory relations: 3 | Added to the model from hypotheses: 2
      Measurement context: no configured measurement-context review signal was
      triggered
      
      Hypothesis evidence
        ID  Relation                       Estimate  95% CI           Concordance
        H1  Agency -> Persistence             0.458  [0.389, 0.526]   Concordant
        H2  Agency <-> SocialDesirability     0.008  [-0.079, 0.095]  Concordant
        H3  Agency -> Performance             0.389  [0.325, 0.454]   Concordant
      
      Theory concordance, uncertainty, measurement quality, and replication are
      distinct evidence streams. Statistical significance alone is not a validity
      verdict.

---

    Code
      print(summary(net))
    Output
      <nomo_network summary> Nomological network
      Primary sample: N = 800 | Converged: yes
      
      Measurement context
        Flag: none | Constructs: 3 | Loading flags: 0 | Negative variances: 0 |
        Global-fit flags: 0 | Engine warnings: 0
        no configured measurement-context review signal was triggered
      
      Model fit
        chi-square(51) = 61.62, p = .147
        CFI 0.997 | TLI 0.996 | RMSEA 0.016 | SRMR 0.020
      
      Hypothesis evidence
        ID  Relation                       Estimate  95% CI           Concordance
        H1  Agency -> Persistence             0.458  [0.389, 0.526]   Concordant
        H2  Agency <-> SocialDesirability     0.008  [-0.079, 0.095]  Concordant
        H3  Agency -> Performance             0.389  [0.325, 0.454]   Concordant
      
      Predictions and context
        ID  Prediction  Region         Evidence scope              Status
        H1  positive    [0.2, +Inf)    latent structural           A priori
        H2  negligible  [-0.15, 0.15]  latent association          A priori
        H3  positive    (0, +Inf)      latent to observed outcome  A priori
      
      Concordance
        Concordant 3

---

    Code
      print(nomo_split(nomo_demo_network, validation_prop = 0.4, seed = 2026))
    Output
      <nomo_split> Calibration and validation split
      Rows: 800 total | 480 calibration | 320 validation
      Validation proportion: 0.400 requested | 0.400 realized | Seed: 2026
      Use splitting only when the gain in independence justifies the loss of
      precision.

