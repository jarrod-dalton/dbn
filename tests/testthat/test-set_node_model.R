context("set_node_model")

x <- dbn(~ mpg | cyl * qsec +
           wt)

mpg <- lm(mpg ~ factor(cyl) + qsec, data = mtcars)
cyl <- xtabs(~ cyl, data = mtcars)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Replace the current node model with the specified model.",
  {
    expect_true(
      has_node_model(
        set_node_model(x, mpg = mpg),
        "mpg"
      )
    )
  }
)

test_that(
  "Replace the current node model with the specified model.",
  {
    expect_equal(
      has_node_model(
        set_node_model(x, mpg = mpg, cyl = cyl)),
        c(mpg = TRUE, wt = FALSE, cyl = TRUE, qsec = FALSE)
    )
  }
)

test_that(
  "Replace the current node model with the specified model.",
  {
    expect_equal(
        set_node_model(x, mpg = mpg)[["node_attr"]][["model"]][[1]],
        mpg
      )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "When add = TRUE append any parents in the model not currently in the node's 
   parents to the parent_raw and parent attributes of the node.",
  {
    expect_equal(
      sort(set_node_model(x, 
                     mpg = lm(mpg ~ factor(cyl) + qsec + wt, data = mtcars),
                     add = TRUE,
                     force = TRUE)[["node_attr"]][["parent"]][[1]]),
      sort(c("cyl", "qsec", "wt"))
    )
  }
)

test_that(
  "When add = TRUE append any parents in the model not currently in the node's 
   parents to the parent_raw and parent attributes of the node.",
  {
    expect_equal(
      sort(set_node_model(x, 
                     mpg = lm(mpg ~ factor(cyl) + qsec + wt, data = mtcars),
                     add = TRUE,
                     force = TRUE)[["node_attr"]][["parent_raw"]][[1]]),
      sort(c("cyl", "qsec", "wt"))
    )
  }
)


# Functional Requirement 3 ------------------------------------------

test_that(
  "When add = FALSE replace the nodes in parent_raw and parent with the 
   parents in the model.",
  {
    expect_equal(
      set_node_model(x, 
                     mpg = lm(mpg ~ wt, data = mtcars),
                     add = FALSE,
                     force = TRUE)[["node_attr"]][["parent"]][[1]],
      c("wt")
    )
  }
)

test_that(
  "When add = FALSE replace the nodes in parent_raw and parent with the 
   parents in the model.",
  {
    expect_equal(
      set_node_model(x, 
                     mpg = lm(mpg ~ wt, data = mtcars),
                     add = FALSE,
                     force = TRUE)[["node_attr"]][["parent_raw"]][[1]],
      c("wt")
    )
  }
)
# Functional Requirement 4 ------------------------------------------

test_that(
  "When force = FALSE, if parents in the model do not match the
   parents of the node, cast an error.",
  {
    expect_error(
      set_node_model(x,
                     mpg = lm(mpg ~ wt, data = mtcars)),
      "have parents not listed for the node"
    )
  }
)

test_that(
  "When force = FALSE, if parents in the model do not match the
   parents of the node, cast an error.",
  {
    expect_error(
      set_node_model(x,
                     mpg = lm(mpg ~ factor(cyl) + qsec + wt, data = mtcars)),
      "have parents not listed for the node"
    )
  }
)

# Functional Requirement 5 ------------------------------------------

# When force = TRUE, if there are new parents to be added, perform the 
# appropriate action based on the add argument.

# This requirement is redundant to requirements 2 and 3. The requirement
# is preserved for the purpose of making expected behavior explicit, but
# it is not tested here.

# Functional Requirement 6 ------------------------------------------

test_that(
  "When force = TRUE and add = TRUE, if the node has 
   parents that aren't in the model, cast an error. add = FALSE
   should be used to remove parents.",
  {
    expect_error(
      set_node_model(x, 
                     mpg = lm(mpg ~ wt, data = mtcars),
                     add = TRUE,
                     force = TRUE),
      "to force a refresh"
    )
  }
)

# Functional Requirement 7 ------------------------------------------

test_that(
  "Cast an error if network is not a dbn object",
  {
    expect_error(
      set_node_model(mtcars, mpg = lm(mpg ~ qsec + factor(cyl), data = mtcars))
    )
  }
)

# Functional Requirement 8 ------------------------------------------

test_that(
  "Cast an error if ... is unnamed",
  {
    expect_error(
      set_node_model(x, lm(mpg ~ factor(cyl) + qsec, data = mtcars))
    )
  }
)

test_that(
  "Cast an error if ... is unnamed",
  {
    expect_error(
      set_node_model(x, 
                     mpg = lm(mpg ~ factor(cyl) + qsec, data = mtcars),
                     xtabs( ~ cyl, data = mtcars))
    )
  }
)

# Functional Requirement 9 ------------------------------------------

test_that(
  "Cast an error if any model object in ... cannot be parsed by model_to_node",
  {
    n <- dbn(~ lh)
    expect_error(
      set_node_model(n, arims(lh, order = c(1, 0, 0)))
    )
  }
)

# Functional Requirement 10 -----------------------------------------

test_that(
  "Cast an error if add is not logical(1)",
  {
    expect_error(
      set_node_model(x, add = "true")
    )
  }
)

test_that(
  "Cast an error if add is not logical(1)",
  {
    expect_error(
      set_node_model(x, add = c(TRUE, FALSE))
    )
  }
)

# Functional Requirement 11 -----------------------------------------

test_that(
  "Cast an error if force is not logical(1)",
  {
    expect_error(
      set_node_model(x, force = "true")
    )
  }
)

test_that(
  "Cast an error if force is not logical(1)",
  {
    expect_error(
      set_node_model(x, force = c(TRUE, FALSE))
    )
  }
)

# Functional Requirement 12 -----------------------------------------

test_that(
  "Cast an error if the model has more than one response variable.",
  {
    expect_error(
      set_node_model(x, 
                     mpg = lm(mpg + am ~ factor(cyl) + qsec, data = mtcars)),
      "have more than one response variable"
    )
  }
)

# Functional Requirement 13 -----------------------------------------

test_that(
  "Cast an error if the response variable does not match the node name.",
  {
    expect_error(
      set_node_model(x,
                     qsec = lm(mpg + am ~ factor(cyl) + qsec, data = mtcars)),
      "models' response variable"
    )
  }
)