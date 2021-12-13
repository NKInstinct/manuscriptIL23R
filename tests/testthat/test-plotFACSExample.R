path_to_gs <- system.file("extdata", package = "manuscriptIL23R")
gs <- flowWorkspace::load_gs(paste0(path_to_gs, "/example.gs"))
gg <- plotFACSExample(gs, "IFNg.XGBoost", "CD8a", "CD8 IFNg")

# Check that plotFACSExample returns a ggplot object
test_that("returns ggplot", {
  expect_true(is(gg, "ggplot"))
})
