#' @name get_node_formula
#' @title Retrieve the node definition from the node formula
#' 
#' @description Retrieve the node definition from a \code{dbn} object node.
#' 
#' @param network An object of class \code{dbn}.
#' @param node A character vector of nodes for which the formula definitions 
#'   are to be obtained.
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return the raw node definition from the \code{node_attr} data
#'       frame for each node listed in \code{node}.
#'   \item If any \code{node} is not a node in \code{network}, cast a warning
#'       and drop the invalid nodes.
#'   \item If \code{node} is NULL, return all of the node formulae.
#'   \item If \code{network} is not a \code{dbn}, cast an error.
#' }
#'   
#' @export

get_node_formula <- function(network, node = NULL)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          class = "dbn",
                          add = coll)
  
  if (!is.null(node))
  {
    checkmate::assert_character(x = node,
                                add = coll)
  }
  

  checkmate::reportAssertions(coll)
  
  if (is.null(node))
  {
    node <- network[["node_attr"]][["node_name"]]
  }
  
  mapply(
    FUN = 
      function(n, p)
      {
        sep = if (length(p)) " | " else ""
        sprintf("%s%s%s",
                n, 
                sep,
                paste0(unlist(p), collapse = " * "))
      },
    n = network[["node_attr"]][["node_name_raw"]],
    p = network[["node_attr"]][["parent_raw"]]
  )
}