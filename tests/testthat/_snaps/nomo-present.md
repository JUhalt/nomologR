# the CFA print and summary read as designed (#89)

    Code
      print(cfa)
    Output
      <nomo_cfa> Confirmatory factor analysis
      Cases: 473 of 500 used | Estimator: ML | Converged: yes
      Fit: CFI .973 | TLI 0.965 | RMSEA 0.051 | SRMR 0.052
      Loadings: 10 | Flags: 2 review, 0 concern
      No parameter was freed and no model was refit automatically.
      
      ML = maximum likelihood; CFI = comparative fit index; TLI = Tucker-Lewis
      index; RMSEA = root mean square error of approximation; SRMR = standardized
      root mean square residual.
      
      See summary(x) for the evidence and each flag and nomo_table(x, "fit") for
      every fit index.

---

    Code
      print(summary(cfa))
    Output
      <nomo_cfa summary> Confirmatory factor analysis
      Cases: 473 of 500 used (27 not used, 5.4%) | Estimator: ML | Converged: yes
      
      Global fit
        chi-square(34) = 75.83, p < .001
        Index  Value  Reference  90% CI
        CFI     .973       .950
        TLI    0.965      0.950
        RMSEA  0.051      0.060  [0.036, 0.066]
        SRMR   0.052      0.080
        References are teaching values for review, not cutoffs.
      
      Standardized loadings
        Factor  Indicator  Loading    SE  95% CI        Flag
        A       a1            0.77  0.02  [0.72, 0.82]
        A       a2            0.74  0.03  [0.69, 0.80]
        A       a3            0.67  0.03  [0.61, 0.73]
        A       a4            0.74  0.03  [0.69, 0.79]
        A       a5            0.60  0.03  [0.53, 0.66]
        B       b1            0.79  0.02  [0.75, 0.84]
        B       b2            0.69  0.03  [0.64, 0.75]
        B       b3            0.76  0.03  [0.71, 0.81]
        B       b4            0.63  0.03  [0.56, 0.69]
        B       b5            0.34  0.05  [0.25, 0.43]  Review
      
      Factor correlations
        Factor 1  Factor 2    r  95% CI
        A         B         .50  [.41, .59]
      
      Improper solutions
        No improper-solution signal, such as a negative residual variance, was
        detected.
      
      Largest residual correlations
        Item 1  Item 2  Residual
        b3      a5          0.19
        b1      a5          0.19
        b2      a5          0.14
        b4      a5          0.14
        b3      a4         -0.07
      
      Modification indices (diagnostic only)
        Parameter     MI    EPC  Std. EPC
        B =~ a5    48.34   0.49      0.37
        B =~ a1     9.41  -0.18     -0.14
        a5 ~~ b3    7.57   0.08      0.15
        a5 ~~ b1    4.78   0.06      0.12
        a3 ~~ a4    4.64   0.07      0.13
        Modification indices locate strain. They do not authorize freeing a
        parameter, and nomologR never does so automatically.
      
      Flagged
        - Cases (Review): 473 of 500 input cases were used. Confirm that the loss
          follows the intended missing-data strategy.
        - b5 on B (Review): Absolute standardized loading is below the configured
          teaching reference of 0.50; inspect item content, precision, and model
          specification.
      
      What these columns mean
        ML -- Maximum likelihood.
        CFI -- Comparative fit index.
        TLI -- Tucker-Lewis index.
        RMSEA -- Root mean square error of approximation.
        SRMR -- Standardized root mean square residual.
        CI -- Confidence interval.
        SE -- Standard error.
        MI -- Modification index, the expected drop in the chi-square if the
            parameter were freed.
        EPC -- Expected parameter change if the parameter were freed.
        Std. EPC -- The expected parameter change with all variables standardized.
      
      Global fit, local strain, and parameter estimates are evidence to interpret
      together; no single cutoff establishes model validity.
      
      See nomo_table(x, "decision_log") for the decision log and x$fit for lavaan's
      full output.

# the item audit, factor retention, and EFA read as designed (#89)

    Code
      print(scr)
    Output
      <nomo_screen> Item and data audit
      Cases: 500 | Candidate items: 10
      Items with missing responses: 2 | Constant: 0 | All missing: 0
      Items in correlation diagnostics: 10 | Item-rest correlations: 10
      Response concentration flags: 0 | Near-zero variance: 0
      Decision log: 4 entries | Flagged: 1 review, 0 concern
      No rows or items were removed or modified.
      
      See summary(x) for each item's review and nomo_table(x, "decision_log") for
      every log entry.

---

    Code
      print(summary(scr))
    Output
      <nomo_screen summary> Item and data audit
      Cases: 500 | Items: 10 | Item flags: 1 review, 0 concern
      Items with missing responses: 2 | Constant: 0 | All missing: 0
      Items in correlation diagnostics: 10
      
      Item review
        Item  Type        Missing  Top share  Item-rest r  Flag
        a1    continuous     0.0%       1.0%          .56
        a2    continuous     3.0%       1.2%          .58
        a3    continuous     0.0%       1.0%          .51
        a4    continuous     0.0%       1.2%          .55
        a5    continuous     0.0%       1.6%          .59
        b1    continuous     0.0%       1.6%          .60
        b2    continuous     0.0%       1.4%          .51
        b3    continuous     2.4%       1.0%          .57
        b4    continuous     0.0%       1.2%          .48
        b5    continuous     0.0%       1.4%          .28  Review
        Top share is the share of observed responses in the most common category.
        Item-rest r is the correlation of an item with the sum of the other items.
      
      Flagged
        - b5 (Review): `b5` has a corrected item-rest correlation of r = .28
          (n = 473), below the teaching reference of .30.
      
      Flags are review aids, not decisions to keep or delete an item.
      
      See nomo_table(x, "decision_log") for every log entry and plot(x) for the item
      evidence map.

---

    Code
      print(fac)
    Output
      <nomo_factors> Factor-retention evidence
      Cases: 500 (minimum pairwise N: 473) | Items: 10 | Correlation: Pearson
      Criterion set: core | Methods run: 3 | Families: 2 | Not run: 1 | Flags: none
      Parallel analysis (percentile rule): 2 | MAP: 2 (TR2), 2 (TR4) | KMO: .87
      MAP = Velicer's minimum average partial criterion, original (TR2) and revised
      (TR4); KMO = Kaiser-Meyer-Olkin measure of sampling adequacy.
      
      Both available criterion families (3 methods) point to 2 factors. Agreement
      between two criterion families is limited evidence for investigating that
      solution, not proof of dimensionality. 1 requested method was not evaluated;
      nomo_table(x, "criteria") gives the reason.
      
      See summary(x) for the evidence by method and the criteria that did not run.

---

    Code
      print(summary(fac))
    Output
      <nomo_factors summary> Factor-retention evidence
      Cases: 500 (minimum pairwise N: 473) | Items: 10 | Correlation: Pearson
      Criterion set: core
      
      Retention evidence
        Method              Factors  Role
        Parallel analysis         2  Primary
        MAP (original TR2)        2  Complementary
        MAP (revised TR4)         2  Complementary
      
      Parallel-analysis rule sensitivity
        Rule        Factors  Used
        Percentile        2  Selected
        Mean              2
        Crawford          2
      
      Criteria requested but not run
        - Empirical Kaiser criterion: EKC needs one common sample size for the
          analyzed matrix; pairwise missing-data handling produced varying pairwise
          Ns.
      
      Concordance across criterion families
        Factors  Families  Which
              2         2  Parallel analysis; MAP
      
      Supporting adequacy evidence
        - KMO: .87
        - Bartlett's test was not computed because pairwise missing-data handling
          does not provide one common sample size for the full matrix.
      
      Synthesis
        Both available criterion families (3 methods) point to 2 factors. Agreement
        between two criterion families is limited evidence for investigating that
        solution, not proof of dimensionality. 1 requested method was not evaluated;
        nomo_table(x, "criteria") gives the reason.
      
      Abbreviations
        MAP -- Minimum average partial criterion (Velicer). TR2, the original,
            averages squared partial correlations; TR4, the revised, averages fourth
            powers.
        KMO -- Kaiser-Meyer-Olkin measure of sampling adequacy.
        EKC -- Empirical Kaiser criterion.
      
      Factor counts are candidates for investigation, not automatic dimensionality
      verdicts. Common-factor eigenvalues come from a reduced common-variance
      matrix; later values can be negative.
      
      See nomo_table(x, "criteria") for the status of every requested criterion.

---

    Code
      print(efa)
    Output
      <nomo_efa> Exploratory factor analysis
      Cases: 500 (minimum pairwise N: 473) | Items: 10
      Factors: 2 (from nomo_factors())
      Correlation: Pearson | Extraction: minres | Rotation: oblimin (oblique)
      RMSR: 0.018 | Item flags: 2 review, 1 concern
      RMSR = root mean square of the off-diagonal residual correlations. No item was
      deleted and no model was refit automatically.
      
      See summary(x) for the loadings and the reason for each flag.

---

    Code
      print(summary(efa))
    Output
      <nomo_efa summary> Exploratory factor analysis
      Cases: 500 (minimum pairwise N: 473) | Items: 10
      Factors: 2 (from nomo_factors())
      Correlation: Pearson | Extraction: minres | Rotation: oblimin (oblique)
      KMO: .87
      
      Item structure
        Item  Factor  Loading  Next factor  Loading  Communality  Flag
        a1    F1         0.81  F2             -0.04          .62
        a2    F1         0.71  F2              0.06          .55
        a3    F1         0.67  F2              0.02          .45
        a4    F1         0.77  F2             -0.04          .57
        a5    F1         0.41  F2              0.34          .41  Review
        b1    F2         0.78  F1              0.01          .62
        b2    F2         0.69  F1             -0.02          .47
        b3    F2         0.78  F1             -0.01          .61
        b4    F2         0.64  F1             -0.01        .3997  Review
        b5    F2         0.33  F1              0.03          .12  Concern
      
      Flagged
        - b5 (Concern): The primary loading, 0.33 in absolute value, is below the
          0.40 teaching reference. The communality, .12, is below the .40 teaching
          reference.
        - a5 (Review): The secondary loading, 0.34 in absolute value, is at or above
          the 0.30 cross-loading reference.
        - b4 (Review): The communality, .3997, is below the .40 teaching reference.
      
      Factor correlations
        Factor 1  Factor 2    r
        F1        F2        .44
      
      Largest residual correlations
        RMSR: 0.018
        Item 1  Item 2  Residual
        b4      b5          .042
        a5      b5         -.039
        a4      b5          .038
        a2      a5          .030
        b2      b5         -.028
      
      Abbreviations
        KMO -- Kaiser-Meyer-Olkin measure of sampling adequacy.
        RMSR -- Root mean square of the off-diagonal residual correlations.
      
      Numerical references trigger inspection, not automatic deletion or hidden
      refitting.
      
      See nomo_table(x, "pattern") for the full pattern matrix and
      nomo_table(x, "decision_log") for every recorded decision.

# reliability, validity, scores, and missing-data output read as designed (#89)

    Code
      print(rel)
    Output
      <nomo_reliability> Reliability
      Constructs: 2 | Primary coefficient: model-based omega | Review reference: .70
      Omega: .78 to .83 | Omega flags: none
      Alpha: 2 of 2 constructs, as a secondary coefficient
      Uncertainty: point estimates only; use `ci = "bootstrap"` for interval
      estimates.
      
      Reference values guide review; they are not pass/fail reliability rules.
      
      See summary(x) for each construct's omega and alpha.

---

    Code
      print(summary(rel))
    Output
      <nomo_reliability summary> Reliability
      Constructs: 2 | Review reference: .70
      
      Coefficients
        Construct  Indicators  Omega  Alpha
        A          continuous    .83    .83
        B          continuous    .78    .77
      
      Sampling uncertainty was not bootstrapped. For report-ready intervals, rerun
      with `ci = "bootstrap"`.
      Omega is primary for the congeneric CFA workflow; alpha is secondary and
      assumption-dependent. Reliability contributes score-precision evidence, not
      construct validity.
      
      See x$decision_log for the reasoning behind each coefficient.

---

    Code
      print(val)
    Output
      <nomo_validity> Convergent and discriminant evidence
      Constructs: 2 | AVE reference: .50 | HTMT reference: 0.85
      Convergent flags: 2 review, 0 concern (2 constructs)
      Separation flags: none (1 pair)
      
      Flagged
        - A (Review): AVE .497 is below the review reference .50.
        - B (Review): AVE .43 is below the review reference .50; 1 of 5 standardized
          loadings is flagged (b5).
      
      AVE = average variance extracted; HTMT = heterotrait-monotrait ratio, HTMT2
      its geometric-mean form.
      No single index is treated as a declaration that a construct is valid or
      invalid.
      
      See summary(x) for the evidence for each construct and pair.

---

    Code
      print(summary(val))
    Output
      <nomo_validity summary> Convergent and discriminant evidence
      Constructs: 2 | AVE reference: .50 | HTMT reference: 0.85
      Convergent flags: 2 review, 0 concern (2 constructs)
      Separation flags: none (1 pair)
      
      Convergent evidence by construct
        Construct   AVE  Min |loading|  Median |loading|  Loadings flagged  Flag
        A          .497           0.60              0.74                 0  Review
        B           .43           0.34              0.69                 1  Review
      
      Construct separation
        Construct 1  Construct 2  Latent r  95% CI      HTMT2  HTMT
        A            B                 .50  [.41, .59]   0.53  0.54
      
      Flagged
        - b5 (Review): Standardized loading 0.34 is below the review reference 0.50
          in absolute value; inspect item content, precision, and model
          specification.
        - A (Review): AVE (.497) is below the configured convergent-evidence
          reference (.50). Inspect standardized loadings, indicator-specific error,
          and content coverage; do not automatically delete items.
        - B (Review): AVE (.43) is below the configured convergent-evidence
          reference (.50). Inspect standardized loadings, indicator-specific error,
          and content coverage; do not automatically delete items.
      
      What these columns mean
        AVE -- Average variance extracted, the mean share of its indicators'
            variance a construct explains.
        |loading| -- Absolute standardized loading.
        Latent r -- Correlation between two constructs in the CFA.
        CI -- Confidence interval, as lavaan computes it.
        HTMT2 -- Heterotrait-monotrait ratio with geometric means (Roemer et al.,
            2021).
        HTMT -- Heterotrait-monotrait ratio (Henseler et al., 2015).
      
      Standardized loadings and AVE address convergent evidence; latent correlations
      and HTMT-family statistics address construct separation. These are
      complementary questions, not interchangeable pass/fail tests.
      
      See nomo_table(x, "discriminant") for every value and x$decision_log for the
      reasoning behind each flag.

---

    Code
      print(sc)
    Output
      <nomo_scores> Scores
      Unit weighting (method: sum) | 2 factors | 473 scored cases
      
      Score properties (Grice, 2001)
        Factor  Items  Validity  Univocality  Correlational accuracy
        A           5     0.911       +0.455                  -0.097
        B           5     0.885       +0.442                  -0.097
      
      Parallel model (what unit weighting assumes)
        chi-square difference 171.84 on 16 df, p < .001
      
      Notes
        - Review: The parallel model that unit weighting assumes fits worse than the
          model you fitted (chi-square difference 171.84 on 16 df, p < .001). The
          items are not interchangeable in the way adding them assumes. This does
          not forbid a sum score; it means the choice needs a reason beyond
          convenience, and that `validity` and `correlational_accuracy` describe
          what it costs.
        - Review: Validity is below .90 for B. Gorsuch (1983, p. 260) recommended at
          least .80, and above .90 if the scores are to serve as adequate
          substitutes for the factors themselves. Reported as his recommendation,
          not applied as a rule.
        - Concern: Correlations among these scores do not reproduce the correlations
          among the factors: the largest discrepancy is -0.097, between A and B. A
          relationship estimated from these scores carries that much bias, and its
          direction is a property of the method and the model rather than a constant
          that can be corrected for. Where the question can be asked of the latent
          variables, ask it there. For a linear regression among factors, Skrondal
          and Laake (2001) showed a scoring design that gives consistent
          coefficients, and scores from one model containing every factor, like
          these, are not it: the predictors need regression-method scores and the
          outcome Bartlett scores, each from a measurement model of its own.
        - Review: These scores also carry the other factors: the score for A
          correlates +0.455 with a factor it does not represent (Grice, 2001). A
          score that is not univocal cannot be treated as though it measured its own
          factor alone.
        - Review: The strongest standardized loading is at least twice the weakest
          for B. Adding those items gives the weakest indicator the same say as the
          strongest, so two people with the same total can differ on the construct
          by having endorsed different items.
      
      No value here is a pass/fail threshold; see nomo_table(x, "diagnostics").

---

    Code
      print(summary(sc))
    Output
      <nomo_scores summary> Scores
      Unit weighting (method: sum) | 473 scored cases
      
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
        - Review: The parallel model that unit weighting assumes fits worse than the
          model you fitted (chi-square difference 171.84 on 16 df, p < .001). The
          items are not interchangeable in the way adding them assumes. This does
          not forbid a sum score; it means the choice needs a reason beyond
          convenience, and that `validity` and `correlational_accuracy` describe
          what it costs.
        - Coefficient alpha is the reliability coefficient for a unit-weighted
          scale; coefficient H belongs to optimally weighted scores (McNeish & Wolf,
          2020). Reporting H for a sum score, or alpha for a weighted one, describes
          a scale that was not used.
        - Review: Validity is below .90 for B. Gorsuch (1983, p. 260) recommended at
          least .80, and above .90 if the scores are to serve as adequate
          substitutes for the factors themselves. Reported as his recommendation,
          not applied as a rule.
        - Concern: Correlations among these scores do not reproduce the correlations
          among the factors: the largest discrepancy is -0.097, between A and B. A
          relationship estimated from these scores carries that much bias, and its
          direction is a property of the method and the model rather than a constant
          that can be corrected for. Where the question can be asked of the latent
          variables, ask it there. For a linear regression among factors, Skrondal
          and Laake (2001) showed a scoring design that gives consistent
          coefficients, and scores from one model containing every factor, like
          these, are not it: the predictors need regression-method scores and the
          outcome Bartlett scores, each from a measurement model of its own.
        - Review: These scores also carry the other factors: the score for A
          correlates +0.455 with a factor it does not represent (Grice, 2001). A
          score that is not univocal cannot be treated as though it measured its own
          factor alone.
        - Review: The strongest standardized loading is at least twice the weakest
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
      Rodriguez, Reise, and Haviland (2016).
      Bifactor model | General factor: G | Group factors: A, B, C
      Estimand: unit-weighted observed composite
      
      Total score
        Index                         Estimate
        Omega total                        .90
        Omega hierarchical                 .74
        Omega hierarchical, relative       .83
      
      Item set
        Index  Estimate
        ECV         .61
        PUC         .75
      
      Subscales
        Subscale  Items  Omega subscale  Omega hierarchical subscale
        A             3             .80                          .33
        B             3             .79                          .35
        C             3             .80                          .24
      
      Factor scores
        Factor  Role     Determinacy  Min competing r  Replicability H
        G       general          .87              .50              .83
        A       group            .70             -.01              .49
        B       group            .72              .02              .50
        C       group            .64             -.18              .41
      
      Flagged
        - Review: Factor determinacy is at or below .90 for G, A, B, C, the value
          above which Gorsuch (1983) recommended using factor score estimates.
        - Review: Two equally valid sets of factor scores could correlate as low as
          G (.50), A (-.01), B (.02), C (-.18), below the .70 Gorsuch (1983)
          suggested.
        - Review: Construct replicability H is below .70, the standard Hancock and
          Mueller (2001) proposed, for A, B, C.
      
      What these abbreviations mean
        ECV -- Explained common variance: the share of the items' common variance
            that the general factor explains.
        PUC -- Percentage of uncontaminated correlations, shown as a proportion: the
            share of item correlations that reflect the general factor alone.
        Min competing r -- The lowest correlation two equally valid sets of factor
            scores could have, twice the squared determinacy minus one.
        Replicability H -- Construct replicability (Hancock & Mueller, 2001): how
            well a factor's own indicators, optimally weighted, define it.
      
      These indices do not choose between a bifactor and a higher-order structure,
      and no value is treated as a pass/fail threshold.
      
      See summary(x) for each index's meaning with the notes in full and
      nomo_table(x, "factors") for every factor-score value.

---

    Code
      print(summary(hier))
    Output
      <nomo_hierarchical summary> Hierarchical model evaluation
      Rodriguez, Reise, and Haviland (2016).
      Bifactor model | General factor: G | Group factors: A, B, C
      Estimand: unit-weighted observed composite
      
      Total score
        - Omega total = .90: Common sources explain .90 of the variance of the
          unit-weighted total score.
        - Omega hierarchical = .74: The general factor (G) explains .74 of the
          variance of the unit-weighted total score.
        - Omega hierarchical, relative = .83: Of the total score's reliable
          variance, .83 reflects the general factor (G) and the rest reflects group
          factors.
      
      Item set
        - ECV = .61: The general factor (G) explains .61 of the common variance
          across items. Higher values indicate a stronger general factor relative to
          the group factors; no benchmark value establishes that the items are
          unidimensional.
        - PUC = .75: Of the item correlations, .75 are influenced only by the
          general factor. When this share is very high, even a modest ECV can yield
          relatively unbiased estimates from a unidimensional model.
      
      Subscales
        Subscale  Items  Omega subscale  Omega hierarchical subscale
        A             3             .80                          .33
        B             3             .79                          .35
        C             3             .80                          .24
      
      Factor scores
        Factor  Role     Determinacy  Min competing r  Replicability H
        G       general          .87              .50              .83
        A       group            .70             -.01              .49
        B       group            .72              .02              .50
        C       group            .64             -.18              .41
      
      Flagged
        - Review: Factor determinacy is at or below .90 for G, A, B, C. Gorsuch
          (1983, p. 260) recommended using factor score estimates only above that
          value. This is his recommendation reported as context, not a rule applied
          here; the score may still be usable for some purposes.
        - Review: Two equally valid sets of factor scores could correlate as low as
          G (.50), A (-.01), B (.02), C (-.18). Gorsuch (1983, p. 260) suggested
          this minimum be above .70. A negative value means two researchers scoring
          the same data could rank people in opposite orders and both be consistent
          with the model.
        - Review: Construct replicability H is below .70 for A, B, C. Hancock and
          Mueller (2001) proposed .70 as a standard; a factor below it is not well
          defined by its own indicators and is expected to change across studies.
          Reported as their standard, not applied as a rule.
      
      Notes
        - Bifactor model: G is measured by all 9 items, with 3 group factors (A, B,
          C).
        - A bifactor model will usually fit at least as well as correlated-factors
          or higher-order models of the same items, even when it did not generate
          the data (Reise, 2012), and a higher-order model is a constrained version
          of it (Yung, Thissen, & McLeod, 1999). Bonifay, Lane, and Reise (2017)
          call the bifactor model's tendency to show superior goodness of fit in
          model comparison studies a particular concern, and say that superior fit
          may be a symptom of overfitting: modeling not only the trends in the data
          but also unwanted noise. Murray and Johnson (2013) compared these two
          structures directly and found the comparison biased in favor of the
          bifactor model: unless there was essentially no unmodeled complexity,
          their simulation favored the bifactor model even when a higher-order model
          generated the data. They concluded that which model to adopt should not
          rely on which is better fitting. Compare the alternatives with
          nomo_compare() and choose on substantive grounds, not on fit alone.
        - Factor determinacy is the correlation between a factor and its estimated
          factor score (Beauducel, 2011; Rodriguez, Reise, & Haviland, 2016). It is
          computed from the whole model-reproduced correlation matrix, so a group
          factor's score can use the other items to partial out the general factor.
          Construct replicability H (Hancock & Mueller, 2001) uses only that
          factor's own loadings and treats the rest of each item as uncorrelated
          residual. When the data are unidimensional, H equals the squared
          determinacy (`determinacy_r2`); under a bifactor model the two can differ,
          which Rodriguez et al. note without preferring either. Read each as the
          question it answers.
        - Each omega describes a unit-weighted observed composite, with observed
          covariances in the denominator.
      
      What these abbreviations mean
        ECV -- Explained common variance: the share of the items' common variance
            that the general factor explains.
        PUC -- Percentage of uncontaminated correlations, shown as a proportion: the
            share of item correlations that reflect the general factor alone.
        Min competing r -- The lowest correlation two equally valid sets of factor
            scores could have, twice the squared determinacy minus one.
        Replicability H -- Construct replicability (Hancock & Mueller, 2001): how
            well a factor's own indicators, optimally weighted, define it.
      
      These indices do not choose between a bifactor and a higher-order structure,
      and no value is treated as a pass/fail threshold.
      
      See nomo_table(x, "decision_log") for the record of each index and note.

# comparison, invariance, and network output read as designed (#89)

    Code
      print(cmp)
    Output
      <nomo_compare> Measurement-model comparison
      Models: 2 | Reference: full | Estimator: ML | Cases: 473 of 500 used
      Origin: a priori
      Rationale: Is b5 needed?
      
      Compared with full
        - no_b5 (nested, more constrained): Delta chi-square(1) = 46.91, p < .001;
          CFI change -.029, RMSEA change +0.022; AIC change +44.91
      
      ML = maximum likelihood; CFI = comparative fit index; RMSEA = root mean square
      error of approximation; AIC = Akaike information criterion.
      
      No model was selected automatically; read the comparison with theory and the
      recorded rationale.
      
      See summary(x) for the interpretations and measurement evidence and
      nomo_table(x, "comparisons") for every test.

---

    Code
      print(summary(cmp))
    Output
      <nomo_compare summary> Measurement-model comparison
      Rationale: Is b5 needed?
      Reference: full | Estimator: ML | Cases: 473 of 500 used | Origin: a priori
      
      Model fit
        Model  Chi-square  df       p   CFI    TLI  RMSEA   SRMR  Parameters
        full        75.83  34  < .001  .973  0.965  0.051  0.052          21
        no_b5      122.75  35  < .001  .944  0.928  0.073  0.093          20
      
      Information criteria
        Model       AIC       BIC  Loadings fixed to zero
        full   11981.02  12068.36                       0
        no_b5  12025.93  12109.11                       1
      
      Difference tests against the reference model
        Model  Delta chi-square  df       p  Relation
        no_b5             46.91   1  < .001  nested, more constrained
      
      Changes in fit (model minus reference)
        Model    CFI     TLI   RMSEA    SRMR     AIC     BIC
        no_b5  -.029  -0.036  +0.022  +0.042  +44.91  +40.75
      
      Interpretation
        - `no_b5` is nested within `full` and has 1 more degree of freedom
          (additional constraints). Delta chi-square(1) = 46.91, p < .001. A small p
          value indicates that the extra constraints are not fully consistent with
          the data; with large samples, even small misspecifications produce small p
          values. Change in fit (`no_b5` minus `full`): CFI -.029, TLI -0.036, RMSEA
          +0.022, SRMR +0.042. AIC +44.91 and BIC +40.75 (`no_b5` minus `full`);
          lower values favor a model for these data, and only differences are
          interpretable. No model is selected automatically; read this evidence with
          theory and the recorded rationale.
      
      Standardized loadings by model
        Factor  Indicator  full  no_b5
        A       a1         0.77   0.77
        A       a2         0.74   0.74
        A       a3         0.67   0.67
        A       a4         0.74   0.74
        A       a5         0.60   0.60
        B       b1         0.79   0.79
        B       b2         0.69   0.70
        B       b3         0.76   0.76
        B       b4         0.63   0.62
        B       b5         0.34   0.00
      
      Measurement evidence by model
        Construct  Metric  full  no_b5
        A          omega    .83    .83
        B          omega    .78    .62
        A          alpha    .83    .83
        B          alpha    .77    .77
        A          AVE      .50    .50
        B          AVE      .43    .52
        A vs. B    HTMT2   0.53   0.53
        - no_b5: The loading fixed to zero for b5 keeps that item in this composite;
          the coefficient does not describe a shortened scale.
      
      What these columns mean
        ML -- Maximum likelihood.
        CFI -- Comparative fit index.
        TLI -- Tucker-Lewis index.
        RMSEA -- Root mean square error of approximation.
        SRMR -- Standardized root mean square residual.
        df -- Degrees of freedom.
        AIC -- Akaike information criterion.
        BIC -- Bayesian information criterion.
        AVE -- Average variance extracted.
        HTMT2 -- Heterotrait-monotrait ratio of correlations, geometric-mean
            version.
      
      No model was selected automatically. Difference tests, changes in fit,
      information criteria, and measurement evidence answer different questions;
      read them together with theory and the recorded rationale.
      
      See nomo_table(x, "decision_log") for the decision log and x$fits for each
      model's own analysis.

---

    Code
      print(inv)
    Output
      <nomo_invariance> Measurement invariance across groups
      Cases: 800 (online n = 400, paper n = 400) | Estimator: ML
      Grouping variable: group | Indicators: continuous
      Requested: configural -> metric -> scalar
      Completed: configural -> metric -> scalar
      
      Fit by level
        Level         CFI  RMSEA   SRMR  CFI change  RMSEA change   LRT p
        configural  1.000  0.000  0.002          --            --      --
        metric      1.000  0.000  0.028        .000         0.000    .146
        scalar       .954  0.121  0.065       -.046        +0.121  < .001
      
      Flagged
        - Score diagnostics (Review): 12 univariate equality-constraint score
          diagnostics were retained.
      
      CFI = comparative fit index; RMSEA = root mean square error of approximation;
      SRMR = standardized root mean square residual; LRT = likelihood-ratio test of
      a level against the level before it; ML = maximum likelihood.
      
      Fit changes and score diagnostics are evidence, not pass/fail rules, and
      nomologR never frees a parameter because of them.
      
      See summary(x) for each level's chi-square test and
      nomo_table(x, "local_strain") for all 12 score diagnostics.

---

    Code
      print(summary(inv))
    Output
      <nomo_invariance summary> Measurement invariance across groups
      Cases: 800 (online n = 400, paper n = 400) | Estimator: ML
      Grouping variable: group | Indicators: continuous
      Levels completed: configural -> metric -> scalar
      
      Identification and sequence
        Continuous indicators use the conventional configural, metric, scalar, and
        strict sequence.
      
      Fit by level
        Level       Chi-square  df       p    CFI  RMSEA   SRMR
        configural        0.33   4    .988  1.000  0.000  0.002
        metric            5.71   7    .575  1.000  0.000  0.028
        scalar           68.93  10  < .001   .954  0.121  0.065
        Held equal: loadings from metric; intercepts from scalar.
      
      Changes from the preceding level
        Level   CFI change  RMSEA change  SRMR change  Delta chi-square  df       p
        metric        .000         0.000       +0.026              5.38   3    .146
        scalar       -.046        +0.121       +0.037             63.23   3  < .001
      
      Latent means relative to online, in its latent standard deviations
        Level   Group  Factor  Difference  95% CI             p
        scalar  paper  Agency        0.44  [0.29, 0.60]  < .001
        Comparable only with invariant intercepts, full or partial.
      
      Largest score diagnostics for equality constraints
        Level  Constraint                                Score chi-square df      p
        scalar Intercept: ag3 (online vs. paper)                    61.12  1 < .001
        scalar Intercept: ag1 (online vs. paper)                    11.32  1 < .001
        scalar Intercept: ag4 (online vs. paper)                     5.56  1   .018
        metric Loading: Agency -> ag3 (online vs. paper)             4.92  1   .027
        metric Loading: Agency -> ag2 (online vs. paper)             1.03  1   .310
        scalar Intercept: ag2 (online vs. paper)                     0.98  1   .323
        metric Loading: Agency -> ag4 (online vs. paper)             0.69  1   .406
        scalar Loading: Agency -> ag2 (online vs. paper)             0.55  1   .460
        scalar Loading: Agency -> ag1 (online vs. paper)             0.32  1   .574
        scalar Loading: Agency -> ag3 (online vs. paper)             0.19  1   .664
      
      Flagged
        - Score diagnostics (Review): 12 univariate equality-constraint score
          diagnostics were retained. Use these diagnostics to localize strain, not
          to authorize automatic constraint release. Partial invariance requires an
          explicit researcher specification and rationale.
      
      What these columns mean
        CFI -- Comparative fit index.
        RMSEA -- Root mean square error of approximation.
        SRMR -- Standardized root mean square residual.
        df -- Degrees of freedom.
        CI -- Confidence interval.
        ML -- Maximum likelihood.
      
      No single CFI change, RMSEA change, SRMR change, chi-square difference, or
      score diagnostic is treated as a universal invariance rule.
      
      See nomo_table(x, "local_strain") for all 12 score diagnostics and
      nomo_table(x, "decision_log") for every recorded decision.

---

    Code
      print(nomo_partial(level = "scalar", syntax = "ag3 ~ 1", rationale = "Anticipated mode difference."))
    Output
      <nomo_partial> Partial invariance releases
      Releases: 1 (researcher specified)
      
      Releases
        - P1 (scalar): ag3 ~ 1. Anticipated mode difference.
      
      No release was selected automatically by nomologR. Each applies from the level
      that first holds its parameter equal.
      
      See nomo_invariance(..., partial = x) for the models fitted with these
      releases.

---

    Code
      print(h)
    Output
      <nomo_hypotheses> Theory-specified relations
      Relations: 3 | A priori: 3 | Post hoc: 0
      
        ID  Relation                       Prediction  Region       Origin
        H1  Agency -> Persistence          positive    >= 0.20      A priori
        H2  Agency <-> SocialDesirability  negligible  [-.15, .15]  A priori
        H3  Agency -> Performance          positive    > 0          A priori
        Every relation is on the standardized scale.
      
      The relations record theory; only nomo_network() evaluates them against data.
      
      See nomo_table(x) for every column and nomo_network(model, data, x) for the
      evidence.

---

    Code
      print(summary(h))
    Output
      <nomo_hypotheses summary> Theory-specified relations
      Relations: 3 | A priori: 3 | Post hoc: 0
      Confirmable as specified: 3 of 3
      
        ID  Relation                       Prediction  Region       Origin
        H1  Agency -> Persistence          positive    >= 0.20      A priori
        H2  Agency <-> SocialDesirability  negligible  [-.15, .15]  A priori
        H3  Agency -> Performance          positive    > 0          A priori
        Every relation is on the standardized scale.
      
      What each prediction claims
        - H1 Agency -> Persistence: a positive relation (region: >= 0.20).
        - H2 Agency <-> SocialDesirability: a negligible relation (region:
          [-.15, .15]).
        - H3 Agency -> Performance: a positive relation of any size.
      
      See nomo_table(x) for every column.

---

    Code
      print(net)
    Output
      <nomo_network> Nomological network
      Cases: 800 | Converged: yes
      Theory relations: 3 | Added to the model from hypotheses: 2
      Measurement context: no flags | Model fit: no flags
      
      Hypothesis evidence
        ID  Relation                       Concordance  Estimate  CI
        H1  Agency -> Persistence          Concordant       0.46  [0.39, 0.53]
        H2  Agency <-> SocialDesirability  Concordant        .01  [-.07, .08]
        H3  Agency -> Performance          Concordant       0.39  [0.32, 0.45]
        Estimates are standardized. CI = confidence interval (95%); for H2, a
        negligible() prediction, it is the 90% equivalence interval the concordance
        is judged on.
      
      Flagged
        - Review: `Persistence <-> SocialDesirability`, estimated in the model as
          given, is fixed to zero in the fitted model: with the hypothesized paths,
          `Persistence` is an outcome, and lavaan does not covary an outcome's
          residual with a variable that does not predict it.
        - Review: `Performance <-> SocialDesirability` is fixed to zero in the
          fitted model: the hypotheses bring `Performance` into the model, and
          neither they nor lavaan's defaults relate the two.
      
      Theory concordance, uncertainty, measurement quality, and replication are
      distinct evidence streams. Statistical significance alone is not a validity
      verdict.
      
      See summary(x) for the fit and the flagged evidence in full and
      nomo_table(x, "hypotheses") for every column.

---

    Code
      print(summary(net))
    Output
      <nomo_network summary> Nomological network
      Cases: 800 | Converged: yes
      Estimator: ML
      
      Measurement context
        Status: no flags | Constructs: 3 | Loading flags: 0 | Negative variances: 0
        Fit flags: 0 | Engine warnings: 0
        No measurement-context flag was raised.
      
      Model fit
        Network model: chi-square(51) = 61.62, p = .147
          CFI .997 | TLI 0.996 | RMSEA 0.016 | SRMR 0.020
        Measurement model alone: chi-square(49) = 61.60, p = .107
          CFI .996 | TLI 0.994 | RMSEA 0.018 | SRMR 0.020
        Structural restrictions: Delta chi-square(2) = 0.02, p = .991
      
      Relations the hypothesized paths changed
        - Persistence <-> SocialDesirability: fixed to zero, although the model as
          given estimates it.
        - Performance <-> SocialDesirability: fixed to zero; the model as given does
          not contain Performance.
        - Persistence <-> Performance: estimated as a residual covariance of two
          outcomes that neither the model nor the hypotheses name.
      
      Hypothesis evidence
        ID  Relation                       Concordance  Estimate  CI
        H1  Agency -> Persistence          Concordant       0.46  [0.39, 0.53]
        H2  Agency <-> SocialDesirability  Concordant        .01  [-.07, .08]
        H3  Agency -> Performance          Concordant       0.39  [0.32, 0.45]
        Estimates are standardized. CI = confidence interval (95%); for H2, a
        negligible() prediction, it is the 90% equivalence interval the concordance
        is judged on.
      
      Predictions and context
        ID  Prediction  Region       Evidence scope              Origin
        H1  positive    >= 0.20      latent structural           A priori
        H2  negligible  [-.15, .15]  latent association          A priori
        H3  positive    > 0          latent to observed outcome  A priori
      
      Concordance
        Concordant: 3
      
      Flagged
        - Review: `Persistence <-> SocialDesirability`, estimated in the model as
          given, is fixed to zero in the fitted model: with the hypothesized paths,
          `Persistence` is an outcome, and lavaan does not covary an outcome's
          residual with a variable that does not predict it. Fixing a relation to
          zero is a restriction of the network, and its misfit counts against the
          theory's structure. If the theory allows the relation, add it as a
          hypothesis or write it in `model`.
        - Review: `Performance <-> SocialDesirability` is fixed to zero in the
          fitted model: the hypotheses bring `Performance` into the model, and
          neither they nor lavaan's defaults relate the two. Fixing a relation to
          zero is a restriction of the network, and its misfit counts against the
          theory's structure. If the theory allows the relation, add it as a
          hypothesis or write it in `model`.
      
      Abbreviations
        CFI -- comparative fit index.
        TLI -- Tucker-Lewis index.
        RMSEA -- root mean square error of approximation.
        SRMR -- standardized root mean square residual.
        ML -- maximum likelihood.
      
      Theory concordance, uncertainty, measurement quality, model fit, and
      replication are distinct evidence streams; none is a validity verdict.
      
      See nomo_table(x, "decision_log") for every decision-log row and plot(x) for
      the evidence.

---

    Code
      print(nomo_split(nomo_demo_network, validation_prop = 0.4, seed = 2026))
    Output
      <nomo_split> Calibration and validation split
      Rows: 800 | Calibration: 480 | Validation: 320
      Validation proportion: .40 requested, .40 realized | Seed: 2026
      Generator: Mersenne-Twister, Inversion, Rejection
      
      Use splitting only when the gain in independence justifies the loss of
      precision.
      
      See x$assignment for the sample each row went to.

# guided runs say what they found, and group repeated requests (#89)

    Code
      print(paused)
    Output
      <nomo_run> Guided workflow
      Status: PAUSED | Mode: teaching | Sample design: same sample
      Exploratory N = 800 | Confirmatory N = 800 | Scales: 3
      Completed: screen -> factors | Next: efa
      
      Key evidence
        - Item audit: 11 items; flags: none
        - Parallel analysis suggests: Agency 1, Persistence 1, SocialDesirability 1
      
      Researcher decision required: efa (Agency, Persistence, SocialDesirability)
        Reason: The EFA factor count changes the fitted model. Retention evidence
        can inform that choice, but it does not authorize the pipeline to choose for
        the researcher.
        Options: Inspect the full `nomo_factors` result, compare plausible
        neighboring solutions when appropriate, and supply a positive integer for
        each scale.
        Consequence: No EFA is fitted until an explicit researcher factor-count
        decision is supplied.
        - Agency: Parallel analysis currently suggests 1 factor; the retained
          plausible set is 1. The pipeline has not adopted a factor count.
        - Persistence: Parallel analysis currently suggests 1 factor; the retained
          plausible set is 1. The pipeline has not adopted a factor count.
        - SocialDesirability: Parallel analysis currently suggests 1 factor; the
          retained plausible set is 1. The pipeline has not adopted a factor count.
        Example: decisions = list(factor_count = c(Agency = <integer>,
        Persistence = <integer>, SocialDesirability = <integer>))
      
      No later stage has been run automatically while this consequential decision is
      unresolved.

---

    Code
      print(complete)
    Output
      <nomo_run> Guided workflow
      Status: COMPLETE | Mode: teaching | Sample design: same sample
      Exploratory N = 800 | Confirmatory N = 800 | Scales: 3
      Completed: screen -> factors -> efa -> cfa -> reliability -> validity ->
        invariance -> network
      Next: none
      
      Key evidence
        - Item audit: 11 items; flags: none
        - Parallel analysis suggests: Agency 1, Persistence 1, SocialDesirability 1
        - EFA item flags: none
        - CFA: CFI 0.996, RMSEA 0.020, SRMR 0.021; loading flags: none
        - Reliability: omega 0.729 to 0.849
        - Validity: convergent flags 1 review, 0 concern; separation flags none
        - Invariance: completed configural -> metric
        - Network: 1 hypothesis; 1 concordant
        - Careless responding: 0 cases flagged, none removed
        - Scores: sum method
        - Missing-data sensitivity: computed for cfa and network
      
      All requested stages are complete or explicitly marked not requested. No
      hidden item deletion, model respecification, parameter freeing, or validity
      verdict was performed. summary(x) shows the stages and decisions, and
      nomo_report(x, file = "report.html") archives the evidence.

---

    Code
      print(summary(complete))
    Output
      <nomo_run summary> Guided workflow
      Status: COMPLETE | Mode: teaching | Sample design: same sample | Next: none
      
      Stages
        Stage        Status
        screen       completed
        factors      completed
        efa          completed
        cfa          completed
        reliability  completed
        validity     completed
        invariance   completed
        network      completed
        - screen: Candidate items audited exactly as supplied; no data or item
          membership changed.
        - factors: Factor-retention evidence computed; no factor count was adopted
          automatically.
        - efa: EFA fitted using explicit researcher factor-count decisions; no items
          were removed automatically.
        - cfa: Researcher-specified CFA estimated; the model was not respecified
          automatically.
        - reliability: Reliability evidence computed from the retained CFA model.
        - validity: Convergent/discriminant evidence computed; no valid/invalid
          verdict was created.
        - invariance: Requested invariance evidence computed; diagnostics did not
          free parameters automatically.
        - network: Theory-specified network evidence computed without a one-number
          validity score or validation-sample respecification.
      
      Scales
        - Agency (4 items): ag1, ag2, ag3, ag4
        - Persistence (4 items): pe1, pe2, pe3, pe4
        - SocialDesirability (3 items): sd1, sd2, sd3
      
      Recorded decisions
        - sample_design (design, researcher input): same_sample.
        - scale_definition:Agency (design, researcher input): ag1, ag2, ag3, ag4.
        - scale_definition:Persistence (design, researcher input): pe1, pe2, pe3,
          pe4.
        - scale_definition:SocialDesirability (design, researcher input): sd1, sd2,
          sd3.
        - careless_responding (screen, researcher input): effort = TRUE.
        - factor_count:Agency (efa, researcher decision): 1.
        - factor_count:Persistence (efa, researcher decision): 1.
        - factor_count:SocialDesirability (efa, researcher decision): 1.
        - cfa_model (cfa, researcher decision): Agency =~ ag1 + ag2 + ag3 + ag4;
          Persistence =~ pe1 + pe2 + pe3 + pe4; SocialDesirability =~ sd1 + sd2 +
          sd3.
        - scores (cfa, researcher input): method = "sum".
        - missing_data_cfa (cfa, researcher input): strategies: listwise, ml.
        - measurement_model (measurement_review, researcher decision): proceed.
        - missing_data_network (network, researcher input): strategies: listwise,
          ml.
        - workflow_complete (workflow, pipeline): complete.
      
      Component recipe
        Stage        Scope               Function            Status
        screen       Agency              nomo_screen()       completed
        factors      Agency              nomo_factors()      completed
        efa          Agency              nomo_efa()          completed
        screen       Persistence         nomo_screen()       completed
        factors      Persistence         nomo_factors()      completed
        efa          Persistence         nomo_efa()          completed
        screen       SocialDesirability  nomo_screen()       completed
        factors      SocialDesirability  nomo_factors()      completed
        efa          SocialDesirability  nomo_efa()          completed
        cfa          measurement model   nomo_cfa()          completed
        reliability  measurement model   nomo_reliability()  completed
        validity     measurement model   nomo_validity()     completed
        invariance   configured branch   nomo_invariance()   completed
        network      configured branch   nomo_network()      completed
        Data roles and researcher control for each step: nomo_table(x, "recipe").
      
      Methods used: 48
        Stage        Methods  Primary  Historical
        screen             9        0           1
        factors            5        1           3
        efa                3        1           1
        cfa                9        1           3
        reliability        2        1           1
        validity           4        2           1
        invariance         4        2           0
        scores             5        1           1
        network            3        2           1
        workflow           4        2           1
        Primary methods:
        - factors: Common-factor parallel analysis
        - efa: MINRES common-factor extraction
        - cfa: Maximum-likelihood confirmatory factor analysis
        - reliability: Model-based coefficient omega
        - validity: HTMT2; Latent correlations with confidence intervals
        - invariance: Multiple-group confirmatory factor analysis; Configural,
          metric, scalar, and strict sequence
        - scores: Unit-weighted sum or mean score
        - network: Nomological network of construct relations;
          Measurement-then-structure modeling
        - workflow: Staged scale-development workflow; Recorded decisions and
          rationales
        Full entries and references: nomo_methods(x).
      
      Component decision and evidence-log rows retained: 96; see nomo_table(x,
      "component_log").

