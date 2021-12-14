path_to_gs <- system.file("extdata", "example.gs", package = "manuscriptIL23R")
gs <- flowWorkspace::load_gs(path_to_gs)
gg <- plotFACSExample(gs, "IFNg.XGBoost", "CD8a", "CD8 IFNg")

test_that("returns ggplot", {
  expect_true(is(gg, "ggplot"))
})

