context("print.dbn")

test_that(
  "print method writes the correct formula",
  {
    x <- dbn(list(xtabs( ~ cyl, data = mtcars),
                  lm(mpg ~ factor(cyl) + qsec, data = mtcars)))
    expect_output(
      print(x),
      "A `dbn` object with 3 nodes\ncyl\nmpg [|] cyl [*] qsec\nqsec"
    )
  }
)