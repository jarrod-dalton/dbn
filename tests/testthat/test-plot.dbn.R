context("plot.dbn")

net <- dbn(~ Q1 | Age + 
             Q2 | Q1 + 
             Y | Q1 * Q2)

dynamic_net <-
  dbn(~ Age + 
        Sex + 
        Q1[t] | Q2[t-1] * Q1[t-1] * Age * Sex + 
        Q2[t] | Q1[t-1] * Age * Sex,
      max_t = 2)

# Functional Requirement 1 ------------------------------------------

test_that(
  "Cast an error when given an object that does not have class dbn",
  {
    expect_error(plot.dbn(mtcars))
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Provide the user with the option to collapse dynamic nodes into a single-node representation or expand dynamic nodes across all timepoints.",
  {
    plot(net, 
         expand = TRUE)
  }
)

test_that(
  "Provide the user with the option to collapse dynamic nodes into a single-node representation or expand dynamic nodes across all timepoints.",
  {
    plot(net, 
         expand = FALSE)
  }
)

test_that(
  "Provide the user with the option to collapse dynamic nodes into a single-node representation or expand dynamic nodes across all timepoints.",
  {
    expect_error(
      plot(net, 
           expand = 3)
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR 3: Accept a named vector of node customizations",
  {
    expect_silent({
      plot(net,
           custom_node = c(Age = "fillcolor=green",
                           Age = "style=filled"))
    })
  }
)

test_that(
  "FR 3: Cast error with unnamed vector of node customizations",
  {
    expect_error({
      plot(net,
           custom_node = c("fillcolor=green",
                           Age = "style=filled"))
    })
  }
)

test_that(
  "FR 3: Accept a named list of node customizations",
  {
    expect_silent({
      plot(net,
           custom_node = list(Age = "fillcolor=green",
                              Age = "style=filled"))
    })
  }
)

test_that(
  "FR 3: Cast error with unnamed listr of node customizations",
  {
    expect_error({
      plot(net,
           custom_node = list("fillcolor=green",
                              Age = "style=filled"))
    })
  }
)

# Functional Requirement 4 ------------------------------------------

test_that(
  "FR 4: Quietly ignore nodes in `custom_node` vector that do not exist in the network",
  {
    expect_silent({
      plot(net,
           custom_node = c(Age = "fillcolor=green",
                           Height = "style=filled"))
    })
  }
)

test_that(
  "FR 4: Quietly ignore nodes in `custom_node` list that do not exist in the network",
  {
    expect_silent({
      plot(net,
           custom_node = list(Age = "fillcolor=green",
                              Height = "style=filled"))
    })
  }
)

# Functional Requirement 5 ------------------------------------------

# This test does not determine if the output is correct.  It only states
# that the using the star notation didn't kill the function.
test_that(
  paste0("FR 5: Node customizations for dynamic nodes may use a `_*`` ",
         "appendix to apply changes to all of the nodes in the time stream."),
  {
    expect_silent(
      plot(dynamic_net, 
           expand = TRUE,
           custom_node = c("Q1_*" = "style = filled; color = green"))
    )
  }
)
# Functional Requirement 6 ------------------------------------------

test_that(
  "FR 4: Accept a vector of graph customizations",
  {
    expect_silent({
      plot(net,
           custom_general = c("node [shape=circle]"))
    })
  }
)

test_that("FR 4: Accept a list of graph customizations",
          {
            expect_silent({
              plot(net,
                   custom_general = list("node [shape=circle]"))
            })
          })

# Functional Requirement 7 ------------------------------------------

test_that(
  "FR 7: Accept a named vector of edge customizations",
  {
    expect_silent({
      plot(net,
           custom_edge = c("Age -> Q1" = "arrowhead=box"))
    })
  }
)

test_that(
  "FR 7: Cast error on unnamed vector of edge customizations",
  {
    expect_error({
      plot(net,
           custom_edge = c("arrowhead=box"))
    })
  }
)

test_that(
  "FR 7: Accept a named list of edge customizations",
  {
    expect_silent({
      plot(net,
           custom_edge = list("Age -> Q1" = "arrowhead=box"))
    })
  }
)

test_that(
  "FR 7: Cast error on unnamed list of edge customizations",
  {
    expect_error({
      plot(net,
           custom_edge = list("arrowhead=box"))
    })
  }
)

# Functional Requirement 8 ------------------------------------------
# This test does not determine if the output is correct.  It only states
# that the using the star notation didn't kill the function.
test_that(
  paste0("Edge customizations for dynamic edges may use a `_*`",
         "appendix to apply changes to all of the edges in the time stream."),
  {
    expect_silent(
      plot(dynamic_net,
           expand = TRUE,
           custom_edge = c("Q2_0 -> Q1_1" = "arrowhead = box; color = red"))
    )
  }
)
# Functional Requirement 9 ------------------------------------------

test_that(
  "FR 9: Quietly ignore nodes name in a `custom_edge` vector that do not exist in the network",
  {
    expect_silent({
      plot(net,
           custom_edge = c("Age -> Q1" = "arrowhead=box",
                           "Age -> Height" = "arrowhead=box"))
    })
  }
)

test_that(
  "FR 9: Quietly ignore nodes name in a `custom_edge` list that do not exist in the network",
  {
    expect_silent({
      plot(net,
           custom_edge = list("Age -> Q1" = "arrowhead=box",
                              "Age -> Height" = "arrowhead=box"))
    })
  }
)

# Functional Requirement 10 ------------------------------------------

test_that(
  "FR 8: White space will be ignored in the names of a custom_edge vector",
  {
    expect_silent({
      plot(net,
           custom_edge = c("  Age ->   Q1" = "arrowhead=box",
                           "Q1    -> Q2    " = "arrowhead=box"))
    })
  }
)

test_that(
  "FR 10: White space will be ignored in the names of a custom_edge vector",
  {
    expect_silent({
      plot(net,
           custom_edge = list("  Age ->   Q1" = "arrowhead=box",
                              "Q1    -> Q2    " = "arrowhead=box"))
    })
  }
)

# Functional Requirement 11 ------------------------------------------

test_that(
  "FR 9: Display GraphViz code to the console on request.",
  {
    expect_output({
      plot(net,
           display_code = TRUE)
    })
  }
)

test_that(
  "Cast error when `display_code` is not logical",
  {
    expect_error({
      plot(net,
           display_code = 1)
    })
  }
)
