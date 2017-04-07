context("set_node_utilities")

x <- dbn(~ cyl + 
           mpg | cyl * qsec + 
           qsec + 
           am)

y <- dbn(list(xtabs( ~ cyl, data = mtcars),
              lm(mpg ~ factor(cyl) + qsec, data = mtcars)))

# set_node_parent ---------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "When add = TRUE, append the nodes to the parent_raw and parent attributes",
  {
    expect_equal(
      get_node_parent(set_node_parent(x, list(mpg = "am"))),
      list(cyl = character(0),
           mpg = c("cyl", "qsec", "am"),
           qsec = character(0),
           am = character(0))
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When add = FALSE replace the nodes in parent_raw and 
  parent attributes with the new nodes.",
  expect_equal(
    get_node_parent(set_node_parent(x, list(mpg = c("wt", "hp")), add = FALSE)),
    list(cyl = character(0),
         mpg = c("wt", "hp"),
         qsec = character(0),
         am = character(0))
  )
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "When force = FALSE, if a model object exists and new nodes are being 
  added that do not exist in the model, cast an error.",
  {
    expect_error(
      set_node_parent(y, list(mpg = c("qsec", "am")))
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "When force = TRUE, if a model object exists and new nodes are being 
  added that do not exist, drop the model and cast a warning.",
  {
    expect_warning(
      set_node_parent(y, list(mpg = c("qsec", "am")), force = TRUE)
    )
  }
)

test_that(
  "When force = TRUE, if a model object exists and new nodes are being 
  added that do not exist, drop the model and cast a warning.",
  {
    expect_equal(
      suppressWarnings(
        set_node_parent(y, list(mpg = c("qsec", "am")), force = TRUE)[["node_attr"]][["model"]][[2]]
      ),
      NULL
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "If any name in parent is not a node in network, 
  drop the element and cast a warning.",
  {
    expect_warning(
      set_node_parent(x, list(hp = c("am", "qsec")))
    )
  }
)

test_that(
  "If any name in parent is not a node in network, 
  drop the element and cast a warning.",
  {
    expect_identical(
      suppressWarnings(set_node_parent(x, list(hp = c("am", "qsec")))),
      x
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "Cast an error if network does not have class dbn",
  {
    expect_error(set_node_parent(mtcars, list(hp = c("am", "qsec"))))
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if parent is not a named list",
  {
    expect_error(set_node_parent(x, list(c("am", "qsec"))))
  }
)

test_that(
  "Cast an error if parent is not a named list",
  {
    expect_error(set_node_parent(x, list("am", "qsec")))
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if parent is not a named list",
  {
    expect_error(set_node_parent(x, list(mpg = c(1, 3))))
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if add is not logical(1)",
  {
    expect_error(set_node_parent(x, list(mpg = c("qsec", "hp")), add = "Yes"))
  }
)

test_that(
  "Cast an error if add is not logical(1)",
  {
    expect_error(set_node_parent(x, list(mpg = c("qsec", "hp")), add = c(TRUE, FALSE)))
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if add is not logical(1)",
  {
    expect_error(set_node_parent(x, 
                                 list(mpg = c("qsec", "hp")), 
                                 force = "Yes"))
  }
)

test_that(
  "Cast an error if add is not logical(1)",
  {
    expect_error(set_node_parent(x, 
                                 list(mpg = c("qsec", "hp")), 
                                 force = c(TRUE, FALSE)))
  }
)

# set_node_maxt -----------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the max_t attribute of the node",
  {
    expect_equal(
      suppressWarnings(set_node_maxt(x, mpg = 2, am = 3)[["node_attr"]][["max_t"]]),
      c(0, 2, 0, 3)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If the name of any element in ... does not match a node name in the
   network, drop the element and cast a warning",
  {
    expect_warning(
      set_node_maxt(x, am = 1, vs = 3),
      "The following are not valid nodes in the network and are ignored"
    )
  }
)

test_that(
  "If the name of any element in ... does not match a node name in the
   network, drop the element and cast a warning",
  {
    expect_equal(
      suppressWarnings(
        set_node_maxt(x, mpg = 1, vs = 3)[["node_attr"]][["max_t"]]
      ),
      c(0, 1, 0, 0)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_maxt(mtcars, cyl = 1))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if any element in ... is not numeric(1)",
  {
    expect_error(set_node_maxt(x, cyl = c(1, 2)))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "If any of the target nodes are not dynamic, cast an error if any of 
   them are parents of at least one other node.",
  {
    expect_error(
      set_node_maxt(network = x,
                    qsec = 5,
                    mpg = 5)
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "If any of the target nodes are not dynamic, cast an error if any of
   them are parents of at least one other node",
  {
    expect_warning(
      set_node_maxt(network = x,
                    mpg = 5)
    )
  }
)

# set_node_dynamic --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the is_dynamic attribute of the node",
  {
    expect_equal(
      set_node_dynamic(x, mpg = TRUE, am = TRUE)[["node_attr"]][["is_dynamic"]],
      c(FALSE, TRUE, FALSE, TRUE)
    )
  }
)

test_that(
  "Correctly reassigns the is_dynamic attribute of the node",
  {
    expect_equal(
      set_node_dynamic(x, mpg = TRUE, am = TRUE)[["node_attr"]][["is_dynamic"]],
      c(FALSE, TRUE, FALSE, TRUE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If the name of any element in ... does not match a node name in 
   network, drop the element and cast a warning",
  {
    expect_warning(
      set_node_dynamic(x, mpg = TRUE, vs = FALSE)[["node_attr"]][["is_dynamic"]]
    )
  }
)

test_that(
  "If the name of any element in ... does not match a node name in 
   network, drop the element and cast a warning",
  {
    expect_equal(
      suppressWarnings(
        set_node_dynamic(x, mpg = TRUE, vs = FALSE)[["node_attr"]][["is_dynamic"]]
      ),
      c(FALSE, TRUE, FALSE, FALSE)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_dynamic(mtcars, mpg = TRUE))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_dynamic(x, mpg = 1))
  }
)

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_dynamic(x, mpg = c(TRUE, FALSE)))
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if any of the target nodes have children",
  {
    expect_error(
      set_node_dynamic(network = x,
                       mpg = TRUE,
                       cyl = TRUE)
    )
  }
)

# set_node_decision --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the is_decision attribute of the node",
  {
    expect_equal(
      set_node_decision(x, mpg = TRUE, cyl = TRUE)[["node_attr"]][["is_decision"]],
      c(TRUE, TRUE, FALSE, FALSE)
    )
  }
)

test_that(
  "Correctly reassigns the is_decision attribute of the node",
  {
    expect_equal(
      set_node_decision(x, mpg = TRUE, qsec = TRUE)[["node_attr"]][["is_decision"]],
      c(FALSE, TRUE, TRUE, FALSE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If the name of any element in ... does not match a node name in 
  network, drop the element and cast a warning",
  {
    expect_warning(
      set_node_decision(x, mpg = TRUE, vs = FALSE)[["node_attr"]][["is_decision"]]
    )
  }
)

test_that(
  "If the name of any element in ... does not match a node name in 
  network, drop the element and cast a warning",
  {
    expect_equal(
      suppressWarnings(
        set_node_decision(x, mpg = TRUE, vs = FALSE)[["node_attr"]][["is_decision"]]
      ),
      c(FALSE, TRUE, FALSE, FALSE)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_decision(mtcars, mpg = TRUE))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_decision(x, mpg = 1))
  }
)

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_decision(x, mpg = c(TRUE, FALSE)))
  }
)

# set_node_deterministic --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the is_deterministic attribute of the node",
  {
    expect_equal(
      set_node_deterministic(x, mpg = TRUE, cyl = TRUE)[["node_attr"]][["is_deterministic"]],
      c(TRUE, TRUE, FALSE, FALSE)
    )
  }
)

test_that(
  "Correctly reassigns the is_deterministic attribute of the node",
  {
    expect_equal(
      set_node_deterministic(x, mpg = TRUE, qsec = TRUE)[["node_attr"]][["is_deterministic"]],
      c(FALSE, TRUE, TRUE, FALSE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If the name of any element in ... does not match a node name in 
  network, drop the element and cast a warning",
  {
    expect_warning(
      set_node_deterministic(x, mpg = TRUE, vs = FALSE)[["node_attr"]][["is_deterministic"]]
    )
  }
)

test_that(
  "If the name of any element in ... does not match a node name in 
  network, drop the element and cast a warning",
  {
    expect_equal(
      suppressWarnings(
        set_node_deterministic(x, mpg = TRUE, vs = FALSE)[["node_attr"]][["is_deterministic"]]
      ),
      c(FALSE, TRUE, FALSE, FALSE)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_deterministic(mtcars, mpg = TRUE))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_deterministic(x, mpg = 1))
  }
)

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_deterministic(x, mpg = c(TRUE, FALSE)))
  }
)

# set_node_utility --------------------------------------------------
# Functional Requirement 1 ------------------------------------------

test_that(
  "Correctly reassigns the is_utility attribute of the node",
  {
    expect_equal(
      set_node_utility(x, mpg = TRUE, am = TRUE)[["node_attr"]][["is_utility"]],
      c(FALSE, TRUE, FALSE, TRUE)
    )
  }
)

test_that(
  "Correctly reassigns the is_utility attribute of the node",
  {
    expect_equal(
      set_node_utility(x, mpg = TRUE, am = TRUE)[["node_attr"]][["is_utility"]],
      c(FALSE, TRUE, FALSE, TRUE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "If the name of any element in ... does not match a node name in 
  network, drop the element and cast a warning",
  {
    expect_warning(
      set_node_utility(x, mpg = TRUE, vs = FALSE)[["node_attr"]][["is_utility"]]
    )
  }
)

test_that(
  "If the name of any element in ... does not match a node name in 
  network, drop the element and cast a warning",
  {
    expect_equal(
      suppressWarnings(
        set_node_utility(x, mpg = TRUE, vs = FALSE)[["node_attr"]][["is_utility"]]
      ),
      c(FALSE, TRUE, FALSE, FALSE)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_utility(mtcars, mpg = TRUE))
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_utility(x, mpg = 1))
  }
)

test_that(
  "Cast an error if x is not a dbn",
  {
    expect_error(set_node_utility(x, mpg = c(TRUE, FALSE)))
  }
)
# Functional Requirement 5 ------------------------------------------

test_that(
  "Cast an error if any of the target nodes have children",
  {
    expect_error(
      set_node_utility(network = x,
                       mpg = TRUE,
                       cyl = TRUE)
    )
  }
)