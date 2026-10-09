# the loadings table is stable

    Code
      print(nomo_apa_table(apa_cfa(), "loadings", number = 1))
    Output
      <nomo_apa_table> Manuscript table in APA style
      
      Table 1
      Standardized Factor Loadings
      -------------------------
      Item  Agency  Persistence
      -------------------------
      ag1     0.82
      ag2     0.75
      ag3     0.71
      ag4     0.78
      pe1                  0.77
      pe2                  0.70
      pe3                  0.74
      pe4                  0.69
      -------------------------
      Note. Standardized loadings from a confirmatory factor analysis. Estimated
      with maximum likelihood (ML); N = 800. Blank cells are loadings fixed to zero
      by the model.
      
      See x$body for the cells and knitr::knit_print(x) for the Markdown a knitted
      document renders.

# the validity tables are stable

    Code
      print(nomo_apa_table(apa_validity(), number = 4)$body)
    Output
                                 Constructs    *r* [95% CI] HTMT2 HTMT
      1             Agency with Persistence  .46 [.39, .53]  0.45 0.46
      2      Agency with SocialDesirability .01 [-.08, .10]  0.02 0.04
      3 Persistence with SocialDesirability .00 [-.09, .09]  0.04 0.05

---

    Code
      print(nomo_apa_table(apa_validity(), "convergent", number = 5))
    Output
      <nomo_apa_table> Manuscript table in APA style
      
      Table 5
      Average Variance Extracted
      --------------------------
      Construct           k  AVE
      --------------------------
      Agency              4  .59
      Persistence         4  .53
      SocialDesirability  3  .47
      --------------------------
      Note. k = number of indicators; AVE = average variance extracted (Fornell &
      Larcker, 1981), the average proportion of indicator variance the construct
      explains. The review reference is .50; a value below it prompts a look at the
      loadings and content coverage. AVE is convergent evidence and is not a
      reliability coefficient.
      
      See x$body for the cells and knitr::knit_print(x) for the Markdown a knitted
      document renders.

