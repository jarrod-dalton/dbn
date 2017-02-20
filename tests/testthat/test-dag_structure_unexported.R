context("dag_structure: Unexported Utilities")

# These are not particularly robust tests.  
# They only ensure that the objects returned are of the correct class.
# No tests have been written to ensure the values are correct, nor
# that the lengths are correct.  
# It is hoped that proper controls on the inputs will prevent such errors.
# Additional test may be required if controls on the inputs are not suitable
# to prevent such errors.

test_that(
  "dag_structure_get_node_str",
  {
    dag_structure_get_node_str(~ a[t] | b[t-1]) %>%
      expect_character()
  }
)

test_that(
  "dag_structure_get_future_dependency: no future dependency",
  {
    dag_structure_get_future_dependency("a[t] | b[t-1]") %>%
      expect_character(len = 0)
  }
)

test_that(
  "dag_structure_get_future_dependency: has a future dependency",
  {
    dag_structure_get_future_dependency("a[t] | b[t+1]") %>%
      expect_character(len = 1)
  }
)

test_that(
  "dag_structure_remove_temporal_suffix",
  {
    dag_structure_remove_temporal_suffix("a[t] | b[t-1]") %>%
      grepl(pattern = "t", 
            x = .) %>%
      expect_false()
  }
)

test_that(
  "dag_structure_get_node_and_parent",
  {
    dag_structure_get_node_and_parent("a[t] | b[t-1]") %>%
      expect_matrix(mode = "character",
                    nrows = 2,
                    ncols = 2)
  }
)

test_that(
  "dag_structure_get_parent_list: returns a list",
  {
    dag_structure_get_node_and_parent("a[t] | b[t-1]") %>%
      dag_structure_get_parent_list() %>%
      expect_class(classes = "list")
  }
)

test_that(
  "dag_structure_get_parent_list: each element is a character vector",
  {
    dag_structure_get_node_and_parent("a[t] | b[t-1]") %>%
      dag_structure_get_parent_list() %>%
      vapply(X = .,
             FUN = is.character,
             FUN.VALUE = logical(1)) %>%
      expect_logical()
  }
)
