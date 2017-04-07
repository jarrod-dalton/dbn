#' @name get_node_utilities
#' @title Utilities to Get Node Attributes
#' 
#' @description Methods to quickly get attributes
#'   of a node.  These may all be done through subsetting the 
#'   \code{node_attr} attribute of the network object.  These functions
#'   perform the work in a standardized way, allowing less typing.
#'   
#' @param network An object of class \code{dbn}.
#' @param node A character vector of nodes for which the formula definitions 
#'   are to be obtained. If \code{NULL}, a logical for each node is returned.
#'   
#' @details 
#' \code{get_node_parent} returns a list giving the parents of the 
#'   requested nodes.
#'   
#' \code{get_node_dynamic} Get the dynamic attribute of the node.
#'   
#' \code{is_node_decision} Get the decision attribute of the node.
#' 
#' \code{is_node_utility} Get the utility attribute of the node.
#' 
#' \code{is_node_deterministic} Get the deterministic attribute of the node.
#'   
#' @section Functional Requirements:
#' \emph{get_node_parent}
#' 
#' \enumerate{
#'  \item Return a list of character vectors naming the parents of 
#'    each node in \code{node}.
#'  \item If \code{node = NULL}, \code{node} is assumed to be the 
#'     vector of all node names.
#'   \item If any \code{node} is not a node in \code{network}, cast a warning
#'       and drop the invalid nodes.
#'   \item If \code{network} is not a \code{dbn}, cast an error.  
#' }
#' 
#' \emph{get_node_maxt}
#' 
#' \enumerate{
#'  \item Return a numeric vector giving the value of \code{max_t} for 
#'     each node in \code{node}.
#'  \item If \code{node = NULL}, \code{node} is assumed to be the 
#'     vector of all node names.
#'   \item If any \code{node} is not a node in \code{network}, cast a warning
#'       and drop the invalid nodes.
#'   \item If \code{network} is not a \code{dbn}, cast an error.  
#' }
#' 
#' Requirements for \code{get_node_dynamic}, \code{get_node_decision},
#' \code{get_node_utility} and \code{get_node_deterministic} are given 
#' in \code{\link{is_node_utilities}}.  The \code{get} functions are 
#' aliases of the \code{is} functions.
#' 
#' @seealso \code{\link{set_node_utilities}}, \code{\link{is_node_utilities}}
#' 
#' @export

get_node_parent <- function(network, node = NULL)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          classes = "dbn")
  
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
  
  if (any(!node %in% network[["node_attr"]][["node_name"]]))
  {
    node_not_found <- 
      node[!node %in% network[["node_attr"]][["node_name"]]]
    
    warning("The following are not nodes in `network` and are ignored: ", 
            paste0(node_not_found, collapse = ", "))
    
    node <- node[!node %in% node_not_found]
  }
  
  index <- match(node, network[["node_attr"]][["node_name"]])
  
  stats::setNames(network[["node_attr"]][["parent"]][index],
                  node)
}

#' @rdname get_node_utilities
#' @export 

get_node_maxt <- function(network, node = NULL)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          classes = "dbn")
  
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
  
  if (any(!node %in% network[["node_attr"]][["node_name"]]))
  {
    node_not_found <- 
      node[!node %in% network[["node_attr"]][["node_name"]]]
    
    warning("The following are not nodes in `network` and are ignored: ", 
            paste0(node_not_found, collapse = ", "))
    
    node <- node[!node %in% node_not_found]
  }
  
  index <- match(node,
                 network[["node_attr"]][["node_name"]])
  
  stats::setNames(network[["node_attr"]][["max_t"]][index],
                  node)
}


