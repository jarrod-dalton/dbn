context("grep_extract")

test_that(
  "FR1: Returns the substring that matches the regular expression.",
  {
    grep_extract(x = "Hello World",
                 pattern = "Hello") %>%
      expect_equal("Hello")
  }
)

test_that(
  "FR1: Returns the substring that matches the regular expression (ignore.case).",
  {
    grep_extract(x = "Hello World",
                 pattern = "hello",
                 ignore.case = TRUE) %>%
      expect_equal("Hello")
  }
)

test_that(
  "FR1: Returns the substring that matches the regular expression (perl).",
  {
    grep_extract(x = "a[t-1]",
                 pattern = "(?=\\[).*?(?<=\\])",
                 perl = TRUE) %>%
      expect_equal("[t-1]")
  }
)

test_that(
  "FR2: If no match is generated, return NA",
  {
    grep_extract(x = "Hello World",
                 pattern = "(?=\\[).*?(?<=\\])",
                 perl = TRUE) %>%
      is.na() %>%
      expect_true()
  }
)