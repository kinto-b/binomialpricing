

test_that("we get expected option values in a few simple examples", {

  # European call
  res <- crr_optionvalue(1, 2, 1/2, 1/4, 2, implicit_value(3))
  p <- data.frame(
    price0 = 1,
    price1 = c(0.5, 2, 0.5, 2),
    price2 = c(0.25, 1, 1, 4)
  )

  v <- data.frame(
    value0 = 0.16,
    value1 = c(0, 0.4, 0, 0.4),
    value2 = c(0, 0, 0, 1)
  )

  expect_equal(as.data.frame(res$prices), p)
  expect_equal(as.data.frame(res$values), v)

  # American put
  res <- crr_optionvalue(1, 2, 1/2, 1/4, 2, implicit_value(3, contract = "put"), american = TRUE)
  v <- data.frame(
    value0 = 2,
    value1 = c(2.5, 1, 2.5, 1),
    value2 = c(2.75, 2, 2, 0)
  )
  expect_equal(as.data.frame(res$values), v)

  # Asian call
  res <- crr_optionvalue(1, 2, 1/2, 1/4, 2, implicit_value(1, exotic="asian"))
  v <- data.frame(
    value0 = 4/15,
    value1 = c(0, 2/3, 0, 2/3),
    value2 = c(0, 1/3, 0, 4/3)
  )
  expect_equal(as.data.frame(res$values), v)

  # Knock-in
  res <- crr_optionvalue(1, 2, 1/2, 1/4, 2, implicit_value(1, Sb = 2, barrier = "ui"))
  v <- data.frame(
    value0 = 0.48,
    value1 = c(0, 1.2, 0, 1.2),
    value2 = c(0, 0, 0, 3)
  )
  expect_equal(as.data.frame(res$values), v)

  # Lookback
  res <- crr_optionvalue(1, 2, 1/2, 1/4, 2, implicit_value(1, exotic="lookback_max"))
  v <- data.frame(
    value0 = 0.64,
    value1 = c(0, 1.6, 0, 1.6),
    value2 = c(0, 1, 0, 3)
  )
  expect_equal(as.data.frame(res$values), v)
})
