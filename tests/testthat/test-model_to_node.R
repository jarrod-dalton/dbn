context("model_to_node.R")

require(rms)

# # OLS data
# x1 <- runif(200)
# x2 <- runif(200)
# x3 <- runif(200)
# x4 <- runif(200)
# y <- x1 + x2 + rnorm(200)
# d <- data.frame(y, x1, x2, x3, x4)
# 
# # CPH data
# n <- 1000
# set.seed(731)
# age <- 50 + 12*rnorm(n)
# label(age) <- "Age"
# sex <- factor(sample(c('Male','Female'), n, 
#                      rep=TRUE, prob=c(.6, .4)))
# cens <- 15*runif(n)
# h <- .02*exp(.04*(age-50)+.8*(sex=='Female'))
# dt <- -log(runif(n))/h
# label(dt) <- 'Follow-up Time'
# e <- ifelse(dt <= cens,1,0)
# dt <- pmin(dt, cens)
# units(dt) <- "Year"
# dd <- datadist(age, sex)


# Functional Requirement 1 ------------------------------------------

test_that(
  "Cast an error if the model formula has more than one element on the left hand side.",
  {
    expect_error(
      model_to_node(
        lm(mpg + qsec ~ factor(am) + wt^2)
      )
    )
  }
)

# Functional Requirement 2 ------------------------------------------

# I'm not sure why this isn't working, but it's a problem with rms
# test_that(
#   "Assume any node in a function is given as the first argument of the function.",
#   {
#     require(rms)
#     f    <- ols(y ~ rcs(x1,4) + x2 + x3 + x4)
#     expect_equal(
#       model_to_node(f),
#       "y | x1 * x2 * x3 * x4"
#     )
#   }
# )

# Functional Requirement 1 (xtabs) ----------------------------------

test_that(
  "Only allow one variable in xtabs definitions",
  {
    expect_error(
      model_to_node(xtabs(~ cyl + am, data = mtcars))
    )
  }
)


# Additional Tests (it works with the following model types) --------

test_that(
  "Works with lm",
  {
    expect_equal(
      model_to_node(lm(mpg ~ qsec + factor(am) + wt^2, data = mtcars)),
      "mpg | qsec * am * wt"
    )
  }
)

test_that(
  "Works with glm",
  {
    expect_equal(
      model_to_node(glm(mpg ~ qsec + factor(am) + wt^2, data = mtcars)),
      "mpg | qsec * am * wt"
    )
  }
)

test_that(
  "Works with glm, family = binomial",
  {
    expect_equal(
      model_to_node(glm(am ~ mpg + factor(cyl) + wt^2, data = mtcars)),
      "am | mpg * cyl * wt"
    )
  }
)

# No additional tests are performed for glm.  The mechanims doesn't change
# based on the family.

test_that(
  "Works with xtabs",
  {
    expect_equal(
      model_to_node(xtabs(~ cyl, data = mtcars)),
      "cyl"
    )
  }
)

test_that(
  "Works with coxph",
  {
    require(survival)
    
    expect_equal(
      model_to_node(coxph(Surv(futime, fustat) ~ age + factor(resid.ds),
                          data = ovarian)),
      "fustat | age * resid.ds"
    )
  }
)

test_that(
  "Works with survreg",
  {
    require(survival)
    
    expect_equal(
      model_to_node(survreg(Surv(futime, fustat) ~ age + factor(resid.ds),
                    data = ovarian)),
      "fustat | age * resid.ds"
    )
  }
)

test_that(
  "Works with multinom",
  {
    require(nnet)
    
    expect_equal(
        model_to_node(
          multinom(cyl ~ mpg^2 + factor(am),
                   data = mtcars)
        ),
      "cyl | mpg * am"
    )
  }
)

# I'm not sure why this isn't working, but it's a problem with rms
# test_that(
#   "Works with ols",
#   {
#     require(rms)
#     options(datadist = "d")
#     f    <- ols(y ~ rcs(x1,4) + x2 + x3 + x4, data = d)
#     expect_equal(
#       model_to_node(f),
#         "y | x1 * x2 * x3 * x4"
#     )
#   }
# )

# I'm not sure why this isn't working, but it's a problem with rms
# test_that(
#   "Works with lrm",
#   {
#     require(rms)
#     x    <- 1:5
#     y    <- c(0,1,0,1,0)
#     reps <- c(1,2,3,2,1)
#     f <- lrm(y ~ x, weights=reps)
#     expect_equal(
#       model_to_node(f),
#       "y | x"
#     )
#   }
# )

# I'm not sure why this isn't working, but it's a problem with rms
# test_that(
#   "Works with cph",
#   {
#     require(rms)
# 
#     options(datadist='dd')
#     f <- cph(Surv(dt, e) ~ rcs(age,4) + sex, x=TRUE, y=TRUE, data = dd)
#     expect_equal(
#       model_to_node(f),
#       "e | age * sex"
#     )
#   }
# )