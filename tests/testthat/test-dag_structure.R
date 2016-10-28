context("dag_structure")

test_that("FC1: Return an error when not passed a formula",
{
  expect_error(dag_structure(mtcars))
})

test_that("FC1: Return an error when formula has a left hand side",
{
  expect_error(dag_structure(a ~ b))
})


test_obj <- dag_structure(~wells + 
                            pe | wells + 
                            d.dimer | pregnant * pe + 
                            angio | pe + 
                            treat | d.dimer * angio + 
                            death | pe * treat)

test_that("FC2: Return a matrix (adjacency_matrix)",
{
  expect_class(test_obj[["adjacency_matrix"]],
               classes = "matrix")
})

test_that("FC2: Return an n x n matrix where n is the number of distinct nodes",
{
  expect_true(
    { test_obj[["adjacency_matrix"]] %>%
      dim()  == 7 } %>% 
      all()
  )
})

test_that("FC3: Return a 0/1 matrix where 1 indicates the presence of an edge",
{
  expect_true(
    all(test_obj[["adjacency_matrix"]] %in% 0:1)
  )
})

test_that("FC4: Return a named list with one element per node.",
{
  expect_list(x = test_obj[["parent_list"]],
              len = 7)
})

test_that("FC4: Return a named list with one element per node.",
{
  expect_named(object = test_obj[["parent_list"]],
               expected = c("wells", "pe", "d.dimer", "angio", 
                            "treat", "death", "pregnant"))
})