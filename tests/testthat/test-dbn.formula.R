context("dbn.R formula method")

# Functional Requirement 1 ------------------------------------------

test_that(
  "FR1: dynamic relationships may be defined",
  {
    expect_silent(
      dbn(~ a[t] | b[t - 1])
    )
  }
)

# Functional Requirement 2 ------------------------------------------

test_that(
  "FR2: Cast an error if an attempt is made to relate to a future event",
  {
    expect_error(
      dbn(~ a[t] | b[t+1])
    )
  }
)

# Functional Requirement 3 ------------------------------------------

test_that(
  "FR3: Cast an error if the relative time component is not integerish",
  {
    expect_error(
      dbn(~ a[t] | b[t + 1.5])
    )
  }
)

test_that(
  "FR3: Cast an error if the relative time component is negative",
  {
    expect_error(
      dbn(~ a[t] | b[t - -1])
    )
  }
)
# Functional Requirement 4 ------------------------------------------

test_that(
  "FR4: Return an object of class `dbn`",
  {
    expect_class(
      dbn(~ a[t] | b[t - 1]),
      "dbn"
    )
  }
)
