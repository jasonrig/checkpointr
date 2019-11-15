context("checkpointing data")

test_that("data that is checkpointed can be loaded again", {
  x <- seq(0, 100)
  y <- checkpoint(
    {
      m <- mean(x)
      s <- sd(x)
      42
    },
    ckpt.id = "test_checkpoint",
    file = "checkpoint1.dat"
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
    ckpt.id = "test_checkpoint",
    file = "checkpoint1.dat"
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
    1,
    ckpt.id = "test_checkpoint",
    file = "checkpoint2.dat"
  )
  expect_equal(m, mean(x))

  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    1,
    ckpt.id = "test_checkpoint",
    file = "checkpoint2.dat"
  )
  expect_false(m == mean(x))

  m <- checkpoint(
    {
      mean(x)
    },
    2,
    ckpt.id = "test_checkpoint",
    file = "checkpoint2.dat"
  )
  expect_equal(m, mean(x))
})

test_that("the expression is re-evaluated when the expression changes", {
  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    ckpt.id = "test_checkpoint",
    file = "checkpoint3.dat"
  )
  expect_equal(m, mean(x))

  x <- rnorm(100)
  y <- x
  m <- checkpoint(
    {
      mean(x)
    },
    ckpt.id = "test_checkpoint",
    file = "checkpoint3.dat"
  )
  expect_false(m == mean(x))

  m <- checkpoint(
    {
      mean(y)
    },
    ckpt.id = "test_checkpoint",
    file = "checkpoint3.dat"
  )
  expect_equal(m, mean(x))
})

test_that("the expression can be forcefully re-evaluated", {
  m1 <- checkpoint(
    {
      rnorm(100)
    },
    ckpt.id = "test_checkpoint",
    file = "checkpoint4.dat"
  )
  m2 <- checkpoint(
    {
      rnorm(100)
    },
    ckpt.id = "test_checkpoint",
    file = "checkpoint4.dat"
  )
  m3 <- checkpoint(
    {
      rnorm(100)
    },
    ckpt.id = "test_checkpoint",
    force = TRUE,
    file = "checkpoint4.dat"
  )
  m4 <- checkpoint(
    {
      rnorm(100)
    },
    ckpt.id = "test_checkpoint",
    file = "checkpoint4.dat"
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
    1,
    ckpt.id = "test_checkpoint",
    file = "checkpoint5.dat"
  )
  expect_equal(m, mean(x))

  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    1,
    ckpt.id = "test_checkpoint",
    file = "checkpoint5.dat"
  )
  expect_false(m == mean(x))

  m <- checkpoint(
    {
      mean(x)
    },
    2,
    ckpt.id = "test_checkpoint",
    check.deps = FALSE,
    file = "checkpoint5.dat"
  )
  expect_false(m == mean(x))
})

test_that("checkpoints set a sensible default id", {
  dep_var <- 1
  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    dep_var,
    file = "checkpoint6.dat"
  )
  expect_equal(m, mean(x))

  x <- rnorm(100)
  m <- checkpoint(
    {
      mean(x)
    },
    dep_var,
    file = "checkpoint6.dat"
  )
  expect_false(m == mean(x))

  dep_var <- 2
  m <- checkpoint(
    {
      mean(x)
    },
    dep_var,
    file = "checkpoint6.dat"
  )
  expect_equal(m, mean(x))

  load("checkpoint6.dat")
  expect_length(names(cache), 1)
})

teardown({
  unlink("checkpoint1.dat")
  unlink("checkpoint2.dat")
  unlink("checkpoint3.dat")
  unlink("checkpoint4.dat")
  unlink("checkpoint5.dat")
  unlink("checkpoint6.dat")
})
