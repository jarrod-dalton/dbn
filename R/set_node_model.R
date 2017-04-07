#' @name set_node_model
#' @title Set and Retrieve Models for \code{dbn} Objects
#' 
#' @description Access and replace models associated with nodes in a \code{dbn}
#'   network.  This provides a more convenient interface with which to 
#'   make updates to the network.
#'   
#' @param network An object of class \code{dbn}
#' @param node A character vector naming nodes in the network.
#' 
#' @section Functional Requirements:
#' \emph{has_node_model}
#' 
#' \enumerate{
#'   \item If \code{node = NULL} or assume all nodes in the network
#'   \item If any \code{node} is not a node in \code{network}, cast a warning
#'       and drop the invalid nodes.
#'   \item Return a logical vector the length of \code{node} where \code{TRUE}
#'     indicates if the node has a model in the network.
#'   \item Cast an error if \code{network} is not a \code{dbn} object.
#'   \item Cast an error if \code{node} is not a character vector.
#' }
#'
#' @export

has_node_model <- function(network, node = NULL)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          classes = "dbn",
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
  
  invalid_node <- !is_node(network = network,
                           node = node)
  
  if (any(invalid_node))
  {
    warning("The following nodes are not nodes in `network` and are ignored: ",
            paste0(node[invalid_node], collapse = ", "))
    node <- node[!invalid_node]
  }
  
  index <- match(node,
                 network[["node_attr"]][["node_name"]])
  
  has_model <- 
    !vapply(X = network[["node_attr"]][["model"]][index],
            FUN = is.null,
            FUN.VALUE = logical(1))
  
  stats::setNames(has_model,
                  node)
}