#' @name is_node_something
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
#' \code{get_node_parent} returns a list of character vectors. Each element
#'   of the list is the parents for the corresponding value of \code{node}.
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
#' \enumerate{
#'   \item Each method returns a logical vector (except for \code{get_node_parent}).
#'   \item If \code{node = NULL}, \code{node} is assumed to be the 
#'     vector of all node names.
#'   \item If any \code{node} is not a node in \code{network}, cast a warning
#'       and drop the invalid nodes.
#'   \item If \code{network} is not a \code{dbn}, cast an error.
#'   \item For \code{is_node_parent_of}, if \code{child} has length not 
#'       equal to 1, cast an error.
#'   \item For \code{get_node_parent}, returns a list of character vectors.
#' }
#' 
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

#' @rdname is_node_something
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

#' @rdname is_node_something
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

#' @rdname is_node_something
#' @export

is_node_dynamic <- function(network, node = NULL)
{
  is_node_something(network, node, "is_dynamic")
}

#' @rdname is_node_something
#' @export

get_node_dynamic <- is_node_dynamic

#' @rdname is_node_something
#' @export

is_node_decision <- function(network, node = NULL)
{
  is_node_something(network, node, "is_decision")
}

#' @rdname is_node_something 
#' @export 

get_node_decision <- is_node_decision

#' @rdname is_node_something
#' @export

is_node_utility <- function(network, node = NULL)
{
  is_node_something(network, node, "is_utility")
}

#' @rdname is_node_something 
#' @export 

get_node_utility <- is_node_utility


#' @rdname is_node_something
#' @export

is_node_deterministic <- function(network, node = NULL)
{
  is_node_something(network = network, 
                    node = node, 
                    attribute = "is_deterministic")
}

#' @rdname is_node_something
#' @export 

get_node_deterministic <- is_node_deterministic


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
