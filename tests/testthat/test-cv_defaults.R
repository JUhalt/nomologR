test_that("cv_defaults returns expected keys", {
  cfg <- cv_defaults()
  expect_true(is.list(cfg))
  expect_true(all(c("item_total_min","efa_loading_min","cfa_loading_min","CR_min","AVE_min") %in% names(cfg)))
})
