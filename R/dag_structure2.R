#' @name dag_structure2
#' @title Convert a formula into an adjacency matrix
#' 

dag_structure2 <- function(fm, coll = NULL){

# Argument Validations ----------------------------------------------
  #******************************************************************
  #* 1. `fm` is a formula
  #* 2. `fm` has no left hand side
  #* 3. `fm` has at least one variable in the right hand side
  #******************************************************************

  #* 1. `fm` is a formula
  #*    if `fm` is not a formula, nothing else matters, so we 
  #*    are skipping the collection and going straight to the error.
  checkmate::assertClass(x = fm,
                         classes = "formula")
  
  
  coll <- checkmate::makeAssertCollection()
  
  #* 2. `fm` has no left hand side
  lhs <- all.vars(update(fm, . ~ 0))
  
  if (!all(lhs == ".")) 
  {
    coll$push("`fm` may not have a left hand side.")
  }
  
  #* 3. `fm` has at least one variable in the right hand side
  #*    While writing tests I discovered that it is pretty 
  #*    hard to make a formula with no right hand side.  
  #*    (R won't let you do it) I'm leaving the check in place, 
  #*    but it will go untested
  nodes <- all.vars(update(fm, 0 ~ .))
  
  if (all(nodes == "."))
  {
    coll$push("`fm` must have a right hand side")
  }
  
  #* 4. Dynamic dependencies refer only to previous time points.
  #*    We are preventing things such as a | b[t + 1].
  #*    We are also disallowing `/` and `*` from being between brackets.
  
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
  
  #* Get the node root names
  root <- gsub(pattern = "(?=\\[).*?(?<=\\])",
               replacement = "",
               x = node_str,
               perl = TRUE)
  
  #* Make a matrix of nodes (column 1) and parents (column 2)
  edge <- dag_structure_get_edge(root)

  #* Restructure the matrix into a list
  parent_list <- dag_structure_get_parent_list(edge)
  
  parent_list
}


#' @rdname dag_structure2
#' @param fm A formula object defining the network

dag_structure_get_node_str <- function(fm)
{
  node_str <- deparse(fm)
  node_str <- paste0(node_str, collapse = " ")
  node_str <- trimws(node_str)
  node_str <- sub(pattern = "^.*?[~]", 
                  replacement = "", 
                  x = node_str,
                  perl = TRUE)
}

#' @rdname dag_structure2
#' @param node_str A character vector of node names

dag_structure_get_future_dependency <- function(node_str)
{
  matched <- 
    gregexpr(pattern = "\\[[^]+]+[+,*,/][^]]+\\]",
             text = node_str,
             perl = TRUE) 
  
  future_dependency <- 
    regmatches(x = node_str,
               m = matched)[[1]]
}

#' @rdname dag_structure2
#' @param root A character vector of root nodes.  These are nodes that have 
#'   the \code{[t-1]} tags removed.

dag_structure_get_edge <- function(root)
{
  # Base methods are faster for (approx.) n <= 100
  if (length(root) <= 100)
  {
    edge <- strsplit(root, 
                     split= "[|]")
    edge <- do.call("rbind",
                    edge)
  }
  else
  {
    stringr::str_split_fixed(root,
                             pattern = "[|]",
                             n = 2)
  }
}

#' @rdname dag_structure2
#' @param edge A character matrix. The first column is a node. The 
#'   second column is character string of parents.

dag_structure_get_parent_list <- function(edge)
{
  parent_list <- strsplit(edge[, 2], split = "[*]")
  parent_list <- lapply(parent_list,
                        function(e) trimws(e[e != ""]))
  setNames(parent_list,
           trimws(edge[, 1]))
}
  

nodes <- 
  ~ a[t] | b[t-1] + 
    b[t] | a[t-1]

dag_structure2(nodes, coll = checkmate::makeAssertCollection())
