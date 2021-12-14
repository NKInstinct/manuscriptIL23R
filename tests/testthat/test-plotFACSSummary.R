path_to_res <- system.file("extdata", "flowResults.Rds", package = "manuscriptIL23R")
res <- readRDS(path_to_res)
gg <- plotFACSSummary(res,
                "CD8/IFNg",
                ylab = "% IFNg (of CD8)",
                xGroup = Genotype,
                statbar = 60)

test_that("path_to_res returns ggplot", {
  expect_true(is(gg, "ggplot"))
})
