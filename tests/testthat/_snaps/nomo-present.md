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

