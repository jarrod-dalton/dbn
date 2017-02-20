context("get_dynamic_time")

test_that(
  "Returns an error if `node` has length > 1",
  {
    expect_error(
      get_dynamic_time(node = c("a_1", "a_2"))
    )
  }
)

test_that(
  "FR1: Return the integer suffix at the end of a node name.",
  {
    get_dynamic_time(node = "a_1") %>%
      expect_integerish(len = 1)
  }
)

test_that(
  "FR2: If the node is not dynamic, return `numeric(0)`",
  {
    get_dynamic_time(node = "a") %>%
      expect_integerish(len = 0)
  }
)
