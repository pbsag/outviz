library(outviz)
context("tlfd")


test_that("check prepare_tlfd_data", {

  skim <- expand.grid(
    from = c(1:3),
    to = c(1:3)
  )
  skim$imp <- seq(from = 1, to = 5, by = .5)
  model <- expand.grid(
    from = c(1:3),
    to = c(1:3)
  )
  model$trips <- c(1:9)
  result <- prep_tlfd_data(skim, model, pct = FALSE)

  expect_equal(result$tbl$count, c(3, 7, 11, 15, 9))
  expect_equal(result$avg, 3.67)
  expect_equal(result$iz, 33.33)
})

test_that("check plotly_tlfd", {

  model <- data.frame(
    bin = c(1, 2, 3, 4),
    count = c(2, 3, 5, 2)
  )
  target <- data.frame(
    bin = c(1, 2, 3, 4),
    count = c(4, 6, 10, 4)
  )
  p <- plotly_tlfd(model, target, names = c("one", "two"), xaxis = "miles")

  expect_is(p, "plotly")
  expect_is(p, "htmlwidget")

})
