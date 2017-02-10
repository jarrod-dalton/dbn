#' @name dag_structure2
#' @title Convert a formula into an adjacency matrix
#' 

dag_structure <- function(fm, coll = NULL){

# Argument Validations ----------------------------------------------

  #* `fm` is a formula
  #* if `fm` is not a formula, nothing else matters, so we 
  #* are skipping the collection and going straight to the error.
  checkmate::assertClass(x = fm,
                         classes = "formula")
  
  
  coll <- checkmate::makeAssertCollection()
  
  #* `fm` has no left hand side
  lhs <- all.vars(update(fm, . ~ 0))
  
  if (!all(lhs == ".")) 
  {
    coll$push("`fm` may not have a left hand side.")
  }
  
  #* `fm` has at least one variable in the right hand side
  #* While writing tests I discovered that it is pretty 
  #* hard to make a formula with no right hand side.  
  #* (R won't let you do it) I'm leaving the check in place, 
  #* but it will go untested
  nodes <- all.vars(update(fm, 0 ~ .))
  
  if (all(nodes == "."))
  {
    coll$push("`fm` must have a right hand side")
  }
  
  #* Dynamic dependencies refer only to previous time points.
  #* We are preventing things such as a | b[t + 1].
  #* We are also disallowing `/` and `*` from being between brackets.
  
  node_str <- dag_structure_get_node_str(fm) 
  future_dependency <- dag_structure_get_future_dependency(node_str)
  
  if (length(future_dependency))
  {
    coll$push(sprintf("Invalid (future) dependencies found:'%s'",
                      paste0(future_dependency, collapse = "', '")))
  }
  
  checkmate::reportAssertions(coll)
  
# Functional Code ---------------------------------------------------
  #* `node_str` was defined in the Argument Validations.
  node_str <- strsplit(node_str, "[+]")[[1]]
  node_str <- trimws(node_str)
  
  #* Separate nodes and parents into columns of a matrix.
  node_and_parent <- dag_structure_get_node_and_parent(node_str)
  
  #* Nodes with and without temporal tags
  root_node <- dag_structure_remove_temporal_suffix(node_and_parent[, 1])
  raw_node <- node_and_parent[, 1]
  
  #* Parents with and without temporal tags
  raw_parent <- 
    lapply(node_and_parent[, 2],
           function(x) unlist(strsplit(x, split = "[*]"))) %>%
    lapply(trimws) %>%
    stats::setNames(root_node) 
  
  root_parent <- 
    lapply(raw_parent,
           dag_structure_remove_temporal_suffix) %>%
    lapply(trimws) %>%
    stats::setNames(root_node) 
  
  #* Make a matrix of nodes (column 1) and parents (column 2)
  
  root_edge <- 
    dag_structure_remove_temporal_suffix(node_and_parent)

  raw_edge <- 
    node_and_parent
  
  #* Determine if the node is dynamic
  is_dynamic <- 
    vapply(raw_node,
           FUN = function(x) gsub("[|].+$", "", x),
           FUN.VALUE = character(1)) %>%
    grepl("(?=\\[).*?(?<=\\])", ., perl = TRUE)
  
  list(edge = root_edge, 
       raw_edge = raw_edge,
       root_node = root_node,
       raw_node = raw_node,
       root_parent = root_parent,
       raw_parent = raw_parent,
       is_dynamic = is_dynamic)
}
