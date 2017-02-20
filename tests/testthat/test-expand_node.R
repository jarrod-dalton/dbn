context("expand_node")

test_that(
  "FR1: If dynamic = TRUE, returns a character vector where each element follows the format node_t",
  {
    expand_node(node = "a",
                time = 3,
                dynamic = TRUE) %>%
      expect_equal(sprintf("a_%s", 0:3))
  }
)

test_that(
  "FR2: If dynamic = FALSE, returns `node`",
  {
    expand_node(node = "a",
                time = 3,
                dynamic = FALSE) %>%
      expect_equal("a")
  }
)
