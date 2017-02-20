context("dag_structure")

test_that(
  "FC1: Return an error when not passed a formula",
  {
    expect_error(dag_structure(mtcars))
  })

test_that(
  "FC2: Return an error when formula has a left hand side",
  {
    expect_error(dag_structure(a ~ b))
  })

test_that(
  "FC3: Return an error when if a dynamic dependency refers to a future time point",
  {
    expect_error(dag_structure(~ a[t] | b[t + 3]))
  }
)

test_that(
  "FC4: Return a list when successful",
  {
    dag_structure(~ wells + 
                    pe | wells + 
                    d.dimer | pregnant * pe + 
                    angio | pe + 
                    treat | d.dimer * angio + 
                    death | pe * treat) %>%
      expect_class(classes = "list")
  }
)


