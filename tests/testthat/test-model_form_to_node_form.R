context("model_form_to_node_form")

fit1 <- lm(mpg ~ qsec + am, data = mtcars)
fit2 <- lm(mpg ~ 1, data = mtcars)
fit3 <- glm(am ~ mpg, data = mtcars, family = binomial)
fit4 <- glm(am ~ 1, data = mtcars, family = binomial)
fit5 <- xtabs(~ gear, data = mtcars)
fit6 <- xtabs(~ gear + vs, data = mtcars)
fit7 <- t.test(mtcars$mpg)

test_that("FR1: Returns a character string with the converted formula: lm with parent",
{
  expect_equal(object = model_form_to_node_form(fit1),
               expected = "mpg | qsec * am")
})

test_that("FR1: Returns a character string with the converted formula: lm with no parent",
{
  expect_equal(object = model_form_to_node_form(fit2),
               expected = "mpg")
})

test_that("FR1: Returns a character string with the converted formula: glm with parent",
{
  expect_equal(object = model_form_to_node_form(fit3),
                   expected = "am | mpg")
})

test_that("FR1: Returns a character string with the converted formula: glm with no parent",
{
  expect_equal(object = model_form_to_node_form(fit4),
                   expected = "am")
})

test_that("FR1: Returns a character string with the converted formula: xtabs with one variable",
{
  expect_equal(object = model_form_to_node_form(fit5),
                   expected = "gear")
})

test_that("FR2: Require that xtabs models only have one variable.",
{
  expect_error(x = model_form_to_node_form(fit6))
})

test_that(paste0("FR1: Should formula extraction fail, return an error ", 
                 "message with directions to file an issue on GitHub."),
{
  expect_error(model_form_to_node_form(fit7))
})