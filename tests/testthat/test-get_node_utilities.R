context("get_node_utilities")

x <- dbn(list(xtabs( ~ cyl, data = mtcars),
              lm(mpg ~ factor(cyl) + qsec, data = mtcars)))

# get_node_parent ---------------------------------------------------

# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a list of character vectors",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_list(
      get_node_parent(x, c("qsec", "mpg")),
      types = "character",
      len = 2
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If node = NULL, node is assumed to be the vector of all node names.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_list(
      get_node_parent(x),
      types = "character",
      len = 3
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_warning(
      get_node_parent(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_list(
      suppressWarnings(get_node_parent(x, c("am", "cyl", "qsec"))),
      types = "character",
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(get_node_parent(mtcars))
  }
)



# get_node_maxt
# Functional Requirement 1 ------------------------------------------
test_that(
  "Return a numeric vector giving the value of max-t for each value of node",
  {
    expect_equal(
      get_node_maxt(network = x, node = c("cyl", "qsec")),
      c(cyl = 0, qsec = 0)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If node = NULL, node is assumed to be the vector of all node names",
  {
    expect_equal(
      get_node_maxt(network = x),
      c(cyl = 0, mpg = 0, qsec = 0)
    )
  }
)

# Functional Requirement 3 ------------------------------------------
test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes",
  {
    expect_warning(
      get_node_maxt(network = x,
                    node = c("am", "mpg"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes",
  {
    expect_equal(
      suppressWarnings(
        get_node_maxt(network = x,
                      node = c("am", "mpg"))
      ),
      c(mpg = 0)
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(get_node_maxt(mtcars))
  }
)

# Functional Requirement 1 ------------------------------------------
# Functional Requirement 1 ------------------------------------------
# Functional Requirement 1 ------------------------------------------

# get_node_dynamic ---------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      get_node_dynamic(x, c("qsec", "mpg"))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If node = NULL, node is assumed to be the vector of all node names.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      get_node_dynamic(x),
      len = 3
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_warning(
      get_node_dynamic(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(get_node_dynamic(x, c("am", "cyl", "qsec"))),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(get_node_dynamic(mtcars))
  }
)


# get_node_decision --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      get_node_decision(x, c("qsec", "mpg"))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If node = NULL, node is assumed to be the vector of all node names.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      get_node_decision(x),
      len = 3
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_warning(
      get_node_decision(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(get_node_decision(x, c("am", "cyl", "qsec"))),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(get_node_decision(mtcars))
  }
)


# get_node_utility --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      get_node_utility(x, c("qsec", "mpg"))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If node = NULL, node is assumed to be the vector of all node names.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      get_node_utility(x),
      len = 3
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_warning(
      get_node_utility(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(get_node_utility(x, c("am", "cyl", "qsec"))),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(get_node_utility(mtcars))
  }
)


# get_node_deterministic --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      get_node_deterministic(x, c("qsec", "mpg"))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If node = NULL, node is assumed to be the vector of all node names.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      get_node_deterministic(x),
      len = 3
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_warning(
      get_node_deterministic(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(get_node_deterministic(x, c("am", "cyl", "qsec"))),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(get_node_deterministic(mtcars))
  }
)



