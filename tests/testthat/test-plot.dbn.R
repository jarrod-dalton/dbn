context("plot.dbn")

net <- dbn(~ Q1 | Age + 
             Q2 | Q1 + 
             Y | Q1 * Q2) 

# Functional Requirement 1 ------------------------------------------

test_that("FR 1: Accept a named vector of node customizations",
          {
            expect_silent({
              plot(net,
                   custom_node = c(Age = "fillcolor=green",
                                   Age = "style=filled"))
            })
          })

test_that("FR 1: Cast error with unnamed vector of node customizations",
          {
            expect_error({
              plot(net,
                   custom_node = c("fillcolor=green",
                                   Age = "style=filled"))
            })
          })

test_that("FR 1: Accept a named list of node customizations",
          {
            expect_silent({
              plot(net,
                   custom_node = list(Age = "fillcolor=green",
                                      Age = "style=filled"))
            })
          })

test_that("FR 1: Cast error with unnamed listr of node customizations",
          {
            expect_error({
              plot(net,
                   custom_node = list("fillcolor=green",
                                      Age = "style=filled"))
            })
          })

# Functional Requirement 2 ------------------------------------------

test_that("FR 2: Quietly ignore nodes in `custom_node` vector that do not exist in the network",
          {
            expect_silent({
              plot(net,
                   custom_node = c(Age = "fillcolor=green",
                                   Height = "style=filled"))
            })
          })

test_that("FR 2: Quietly ignore nodes in `custom_node` list that do not exist in the network",
          {
            expect_silent({
              plot(net,
                   custom_node = list(Age = "fillcolor=green",
                                      Height = "style=filled"))
            })
          })

# Functional Requirement 3 ------------------------------------------

# Functional Requirement 4 ------------------------------------------

test_that("FR 4: Accept a vector of graph customizations",
          {
            expect_silent({
              plot(net,
                   custom_general = c("node [shape=circle]"))
            })
          })

test_that("FR 4: Accept a list of graph customizations",
          {
            expect_silent({
              plot(net,
                   custom_general = list("node [shape=circle]"))
            })
          })

# Functional Requirement 5 ------------------------------------------

test_that("FR 5: Accept a named vector of edge customizations",
          {
            expect_silent({
              plot(net,
                   custom_edge = c("Age -> Q1" = "arrowhead=box"))
            })
          })

test_that("FR 5: Cast error on unnamed vector of edge customizations",
          {
            expect_error({
              plot(net,
                   custom_edge = c("arrowhead=box"))
            })
          })

test_that("FR 5: Accept a named list of edge customizations",
          {
            expect_silent({
              plot(net,
                   custom_edge = list("Age -> Q1" = "arrowhead=box"))
            })
          })

test_that("FR 5: Cast error on unnamed list of edge customizations",
          {
            expect_error({
              plot(net,
                   custom_edge = list("arrowhead=box"))
            })
          })

# Functional Requirement 6 ------------------------------------------

# Functional Requirement 7 ------------------------------------------

test_that("FR 7: Quietly ignore nodes name in a `custom_edge` vector that do not exist in the network",
          {
            expect_silent({
              plot(net,
                   custom_edge = c("Age -> Q1" = "arrowhead=box",
                                   "Age -> Height" = "arrowhead=box"))
            })
          })

test_that("FR 7: Quietly ignore nodes name in a `custom_edge` list that do not exist in the network",
          {
            expect_silent({
              plot(net,
                   custom_edge = list("Age -> Q1" = "arrowhead=box",
                                      "Age -> Height" = "arrowhead=box"))
            })
          })

# Functional Requirement 8 ------------------------------------------

test_that("FR 8: White space will be ignored in the names of a custom_edge vector",
          {
            expect_silent({
              plot(net,
                   custom_edge = c("  Age ->   Q1" = "arrowhead=box",
                                   "Q1    -> Q2    " = "arrowhead=box"))
            })
          })

test_that("FR 8: White space will be ignored in the names of a custom_edge vector",
          {
            expect_silent({
              plot(net,
                   custom_edge = list("  Age ->   Q1" = "arrowhead=box",
                                      "Q1    -> Q2    " = "arrowhead=box"))
            })
          })

# Functional Requirement 9 ------------------------------------------

test_that("FR 9: Display GraphViz code to the console on request.",
          {
            expect_output({
              plot(net,
                   display_code = TRUE)
            })
          })

# Miscellaneous Tests -----------------------------------------------

test_that("Cast error when `display_code` is not logical",
          {
            expect_error({
              plot(net,
                   display_code = 1)
            })
          })