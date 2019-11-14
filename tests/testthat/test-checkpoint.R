context("checkpointing data")

test_that("data that is checkpointed can be loaded again", {
  x <- seq(0, 100)
  y <- checkpoint(
    {
      m <- mean(x)
      s <- sd(x)
      42
    },
    "test_checkpoint1"
  )
  expect_equal(y, 42)
  expect_equal(m, mean(seq(0, 100)))
  expect_equal(s, sd(seq(0, 100)))

  rm(x, y, m, s)

  y <- checkpoint(
    {
      m <- mean(x)
      s <- sd(x)
      42
    },
    "test_checkpoint1"
  )
  expect_equal(y, 42)
  expect_equal(m, mean(seq(0, 100)))
  expect_equal(s, sd(seq(0, 100)))
})

test_that("the expression is re-evaluated when the dependent variables changes", {
  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    "test_checkpoint2",
    1
  )
  expect_equal(m, mean(x))

  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    "test_checkpoint2",
    1
  )
  expect_false(m == mean(x))

  m <- checkpoint(
    {
      mean(x)
    },
    "test_checkpoint2",
    2
  )
  expect_equal(m, mean(x))
})

test_that("the expression is re-evaluated when the expression changes", {
  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    "test_checkpoint3"
  )
  expect_equal(m, mean(x))

  x <- rnorm(100)
  y <- x
  m <- checkpoint(
    {
      mean(x)
    },
    "test_checkpoint3"
  )
  expect_false(m == mean(x))

  m <- checkpoint(
    {
      mean(y)
    },
    "test_checkpoint3"
  )
  expect_equal(m, mean(x))
})

test_that("the expression can be forcefully re-evaluated", {
  m1 <- checkpoint(
    {
      rnorm(100)
    },
    "test_checkpoint4"
  )
  m2 <- checkpoint(
    {
      rnorm(100)
    },
    "test_checkpoint4"
  )
  m3 <- checkpoint(
    {
      rnorm(100)
    },
    "test_checkpoint4",
    force = TRUE
  )
  m4 <- checkpoint(
    {
      rnorm(100)
    },
    "test_checkpoint4"
  )

  expect_true(all(sapply(m1, round, 4) == sapply(m2, round, 4)))
  expect_true(all(sapply(m3, round, 4) == sapply(m4, round, 4)))
  expect_false(all(sapply(m2, round, 4) == sapply(m3, round, 4)))
})

test_that("the checking of dependent variables can be disabled", {
  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    "test_checkpoint5",
    1
  )
  expect_equal(m, mean(x))

  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    "test_checkpoint5",
    1
  )
  expect_false(m == mean(x))

  m <- checkpoint(
    {
      mean(x)
    },
    "test_checkpoint5",
    2,
    check.deps = FALSE
  )
  expect_false(m == mean(x))
})

teardown({
  unlink("test_checkpoint1.dat")
  unlink("test_checkpoint2.dat")
  unlink("test_checkpoint3.dat")
  unlink("test_checkpoint4.dat")
  unlink("test_checkpoint5.dat")
})
