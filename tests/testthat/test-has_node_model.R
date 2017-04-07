context("has_node_model")

x <- dbn(list(xtabs( ~ cyl, data = mtcars),
              lm(mpg ~ factor(cyl) + qsec, data = mtcars)))

# Functional Requirement 1 ------------------------------------------

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  expect_warning(
    has_node_model(x, node = c("mpg", "cyl", "vs"))
  )
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If node = NULL assume all nodes in the network",
  expect_equal(
    names(has_node_model(x)),
    c("cyl", "mpg", "qsec")
  )
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Return a logical vector the length of node",
  expect_equal(
    unname(has_node_model(x)),
    c(TRUE, TRUE, FALSE)
  )
)

test_that(
  "Return a logical vector the length of node",
  expect_equal(
    unname(has_node_model(x, c("mpg", "qsec"))),
    c(TRUE, FALSE)
  )
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if network is not a dbn object",
  {
    expect_error(
      has_node_model(mtcars)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if node is not a character vector.",
  {
    expect_error(
      has_node_model(x, list("mpg", "cyl", "qsec"))
    )
  }
)
