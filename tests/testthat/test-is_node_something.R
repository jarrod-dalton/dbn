context("is_node_something")

# Hold on...this is going to take a while

# is_node_something derivatives -------------------------------------

# is_node_dynamic ---------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      is_node_dynamic(x, c("qsec", "mpg"))
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
      is_node_dynamic(x),
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
      is_node_dynamic(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(is_node_dynamic(x, c("am", "cyl", "qsec"))),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(is_node_dynamic(mtcars))
  }
)

# is_node_decision --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      is_node_decision(x, c("qsec", "mpg"))
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
      is_node_decision(x),
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
      is_node_decision(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(is_node_decision(x, c("am", "cyl", "qsec"))),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(is_node_decision(mtcars))
  }
)

# is_node_utility --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      is_node_utility(x, c("qsec", "mpg"))
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
      is_node_utility(x),
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
      is_node_utility(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(is_node_utility(x, c("am", "cyl", "qsec"))),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(is_node_utility(mtcars))
  }
)

# is_node_deterministic --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      is_node_deterministic(x, c("qsec", "mpg"))
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
      is_node_deterministic(x),
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
      is_node_deterministic(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(is_node_deterministic(x, c("am", "cyl", "qsec"))),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(is_node_deterministic(mtcars))
  }
)

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

# is_node_parent ----------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      is_node_parent(x, c("qsec", "mpg"))
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
      is_node_parent(x),
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
      is_node_parent(x, c("am", "cyl", "qsec"))
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(is_node_parent(x, c("am", "cyl", "qsec"))),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(is_node_parent(mtcars))
  }
)

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

# is_node_parent_of -------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Returns a logical vector",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      is_node_parent_of(x, c("qsec", "mpg"), "mpg")
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
      is_node_parent_of(x, child = "mpg"),
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
      is_node_parent_of(x, c("am", "cyl", "qsec"), "mpg")
    )
  }
)

test_that(
  "If any node is not a node in network, cast a warning and drop the invalid nodes.",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_logical(
      suppressWarnings(is_node_parent_of(x, c("am", "cyl", "qsec"), "mpg")),
      len = 2
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "If network is not a dbn, cast an error",
  {
    expect_error(is_node_parent_of(mtcars, child = "mpg"))
  }
)
# Functional Requirement 5 ------------------------------------------

test_that(
  "If child has length not equal to 1, cast an error",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_error(
        is_node_parent_of(x, c("am", "cyl", "qsec"), c("mpg", "vs"))
    )
  }
)

test_that(
  "If child has length not equal to 1, cast an error",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_error(
      is_node_parent_of(x, c("am", "cyl", "qsec"), character(0))
    )
  }
)
