context("dbn.list")

# Functional Requirement 1 ------------------------------------------

test_that(
  "The list method converts existing model objects into a dbn structure.",
  {
    expect_class(
      dbn(list(xtabs( ~ cyl, data = mtcars),
               lm(mpg ~ factor(cyl) + qsec, data = mtcars))),
      classes = "dbn"
    )
  }
)