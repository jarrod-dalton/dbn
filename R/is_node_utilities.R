#' @name is_node_utilities
#' @title Utilities to identify node attributes
#' 
#' @description Methods to quickly get logical indications of the attributes
#'   of a node.  These may all be done through subsetting the 
#'   \code{node_attr} attribute of the network object.  These functions
#'   perform the work in a standardized way, allowing less typing.
#'   
#' @param network An object of class \code{dbn}.
#' @param node A character vector of nodes for which the formula definitions 
#'   are to be obtained. If \code{NULL}, a logical for each node is returned.
#' @param child A \code{character(1)} vector used in \code{is_node_parent_of}. 
#'   This is the node for which \code{node} are being tested as parents.
#'   
#' @details 
#' \code{is_node_parent} returns a logical stating if the node is a parent of
#'   at least one other node.
#'   
#' \code{is_node_parent_of} returns a logical stating if node is a parent of 
#'   at least one element of \code{child}
#'   
#' \code{is_node_dynamic} Is the node a dynamic node. \code{get_node_dynamic}
#' is an alias.
#'   
#' \code{is_node_decision} Is the node a decision node. \code{get_node_decision}
#' is an alias.
#' 
#' \code{is_node_utility} Is the node a utility node. \code{get_node_utility}
#' is an alias.
#' 
#' \code{is_node_deterministic} Is the node a deterministic node.  
#' \code{get_node_deterministic} is an alias.
#' 
#' \code{is_node_something} is an unexported generalized form from which 
#' \code{is_node_dynamic}, \code{is_node_decision}, \code{is_node_utility},
#' \code{is_node_deterministic} are derived.  This is done to minimize the
#' code base that needs to be maintained. 
#'   
#' @section Functional Requirements:
#' \emph{is_node}
#' 
#' \enumerate{
#'  \item Return a named logical vector the length of \code{node} indicating
#'    if each element in node is a valid node name in \code{network}.
#'  \item Cast an error if \code{network} is not a \code{dbn} object.
#'  \item Cast an error if \code{node} is not a character vector.
#' }
#' 
#' \emph{is_node_parent}
#' 
#' \enumerate{
#'  \item Return a named logical vector the length of \code{node} indicating
#'    if each element is a parent of another node in \code{network}
#'  \item If \code{node = NULL}, \code{node} is assumed to be the 
#'     vector of all node names.
#'   \item If any \code{node} is not a node in \code{network}, cast a warning
#'       and drop the invalid nodes.
#'   \item If \code{network} is not a \code{dbn}, cast an error.
#' }
#' 
#' \emph{is_node_parent_of}
#' 
#' \enumerate{
#'  \item Return a named logical vector the length of \code{node} indicating
#'    if each element is a parent of another node in \code{network}
#'  \item If \code{node = NULL}, \code{node} is assumed to be the 
#'     vector of all node names.
#'   \item If any \code{node} is not a node in \code{network}, cast a warning
#'       and drop the invalid nodes.
#'   \item If \code{network} is not a \code{dbn}, cast an error.
#'   \item Cast an error if \code{child} is not a \code{character(1)}
#'   \item Cast an error if \code{child} is not a node in \code{network}
#' }
#' 
#' \emph{is_node_dynamic, is_node_decision, is_utility, is_deterministic}
#' 
#' \enumerate{
#'   \item Each method returns a logical vector (except for \code{get_node_parent}).
#'   \item If \code{node = NULL}, \code{node} is assumed to be the 
#'     vector of all node names.
#'   \item If any \code{node} is not a node in \code{network}, cast a warning
#'       and drop the invalid nodes.
#'   \item If \code{network} is not a \code{dbn}, cast an error.
#' }
#' 
#' @seealso \code{\link{set_node_utilities}} \code{\link{get_node_utilities}}
#' 
#' @export

is_node <- function(network, node = NULL)
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
  
  stats::setNames(node %in% network[["node_attr"]][["node_name"]],
                  node)
}

#' @rdname is_node_utilities
#' @export

is_node_parent <- function(network, node = NULL)
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
  
  stats::setNames(
    node %in% unlist(network[["node_attr"]][["parent"]]),
    node
  )
}

#' @rdname is_node_utilities
#' @export

is_node_parent_of <- function(network, node = NULL, child)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          classes = "dbn")
  
  if (!is.null(node))
  {
    checkmate::assert_character(x = node,
                                add = coll)
  }
  
  checkmate::assert_character(x = child,
                              len = 1,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = child,
                           choices = network[["node_attr"]][["node_name"]],
                           add = coll)
  
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
  
  child_index <- match(child, network[["node_attr"]][["node_name"]])
  child_parent <- unlist(network[["node_attr"]][["parent"]][child_index])
  
  stats::setNames(node %in% child_parent,
                  node)
}

#' @rdname is_node_utilities
#' @export

is_node_decision <- function(network, node = NULL)
{
  is_node_something(network, node, "is_decision")
}

#' @rdname is_node_utilities
#' @export

is_node_deterministic <- function(network, node = NULL)
{
  is_node_something(network = network, 
                    node = node, 
                    attribute = "is_deterministic")
}

#' @rdname is_node_utilities
#' @export

is_node_dynamic <- function(network, node = NULL)
{
  is_node_something(network, node, "is_dynamic")
}

#' @rdname is_node_utilities
#' @export

is_node_utility <- function(network, node = NULL)
{
  is_node_something(network, node, "is_utility")
}

# Unexported -------------------------------------------------------- 

# is_node_something is an unexported generalized form 
# from which is_node_dynamic, is_node_decision, is_node_utility, 
# is_node_deterministic are derived. 
# This is done to minimize the code base that needs to be maintained.

is_node_something <- function(network, node = NULL, attribute)
{
  # attribute names the column for comparison
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          classes = "dbn")
  
  if (!is.null(node))
  {
    checkmate::assert_character(x = node,
                                add = coll)
  }
  
  attribute <- 
    checkmate::matchArg(x = attribute,
                        choices = c("is_dynamic", "is_decision", 
                                    "is_utility", "is_deterministic"),
                        add = coll)
  
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
  
  stats::setNames(network[["node_attr"]][[attribute]][index],
                  node)
}

#' @rdname get_node_utilities 
#' @export 

get_node_decision <- is_node_decision

#' @rdname get_node_utilities
#' @export 

get_node_deterministic <- is_node_deterministic

#' @rdname get_node_utilities
#' @export

get_node_dynamic <- is_node_dynamic

#' @rdname get_node_utilities 
#' @export 

get_node_utility <- is_node_utility
