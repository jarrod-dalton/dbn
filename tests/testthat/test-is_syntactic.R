context("is_syntactic.R")

# Functional Requirement --------------------------------------------

test_that(
  "Return a logical vector the same length as x",
  {
    x <- c("1", "x", ".2var", ".variable", "if", "ifetex")
    expect_logical(
      is_syntactic(x),
      len = length(x)
    )
  }
)

test_that(
  "Confirm the return values on",
  {
    x <- c("1", "x", ".2var", ".variable", "if", "ifetex")
    expect_equal(
      is_syntactic(x),
      c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE)
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "Cast an error if x is not a character vector",
  {
    expect_error(
      is_syntactic(1:10)
    )
  }
)