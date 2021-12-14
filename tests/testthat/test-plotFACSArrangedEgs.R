path_to_gs <- system.file("extdata", "example.gs", package = "manuscriptIL23R")
gs <- flowWorkspace::load_gs(path_to_gs)
egList <- list(gs[[1]], gs[[2]])
egs <- plotFACSArrangedEgs(egList, "IFNg.XGBoost", "CD8a", "CD8 IFNg")

test_that("returns ggarrange", {
  expect_true(is(egs, "ggarrange"))
})
