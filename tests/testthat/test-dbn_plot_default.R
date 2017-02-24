context("dbn_plot_default")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: Output displayed showing the current settings",
  {
    expect_output(
      dbn_plot_default()
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: Accept a named list where the names are the node types",
  {
    expect_silent(
      dbn_set_plot_default(
        list(generic = list(shape = "diamond"),
             dynamic = list(shape = "triangle"),
             utility = list(fillcolor = "purple"),
             decision = list(fillcolor = "green"))
      )
    )
  }
)

test_that(
  "FR2: Cast an error if `new_opt` has an invalid name",
  {
    expect_error(
      dbn_set_plot_default(
        list(some_name = list(shape = "diamond"))
      )
    )
  }
)

test_that(
  "FR2: Cast an error if `new_opt` is not named",
  {
    expect_error(
      dbn_set_plot_default(
        list(generic = list(shape = "diamond"),
             list(fillcolor = "green"))
      )
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR3: Cast an error if any of the lists in `new_opt` is unnamed",
  {
    expect_error(
      dbn_set_plot_default(
        list(generic = list("green"),
             dynamic = list(fillcolor = "green"))
      )
    )
  }
)

test_that(
  "FR3: Cast an error if any names in the lists in `new_opt` is not a valid GraphViz attribute",
  {
    expect_error(
      dbn_set_plot_default(
        list(generic = list(colour = "green"),
             dynamic = list(fillcolor = "green"))
      )
    )
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "FR4: The user may replace the existing defaults with just the list content",
  {
    new_opt <- list(generic = list(shape = "diamond"))
    dbn_set_plot_default(new_opt, replace = TRUE)
    expect_equal(
      new_opt,
      getOption("dbn_plot_node_default")
    )
  }
)

test_that(
  "FR4: Cast an error if `replace` is not logical",
  {
    expect_error(
      dbn_set_plot_default(list(generic = list(shape = "triangle")),
                           replace = 1)
    )
  }
)

# Functional Requirement 5 ------------------------------------------

test_that(
  "FR5: The user may choose to overwrite existing options with what is in `new_opt`` without altering existing options not in `new_opt`",
  {
    dbn_set_plot_default(
      list(generic = list(shape = "diamond")),
           replace = TRUE
    )
    new_opt <- list(generic = list(shape = "triangle"))
    dbn_set_plot_default(new_opt)
    expect_equal(
      getOption("dbn_plot_node_default"),
      new_opt
    )
  }
)

# Functional Requirement 6 ------------------------------------------

test_that(
  "FR5: The user may choose to overwrite existing options with what is in `new_opt`` without altering existing options not in `new_opt`",
  {
    dbn_set_plot_default(
      list(generic = list(shape = "diamond")),
      replace = TRUE
    )
    new_opt <- list(generic = list(shape = "triangle",
                                   color = "purple"))
    dbn_set_plot_default(new_opt)
    expect_equal(
      getOption("dbn_plot_node_default"),
      new_opt
    )
  }
)

dbn_restore_plot_default()