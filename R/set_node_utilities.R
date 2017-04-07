#' @name set_node_utilities
#' @title Set Node Attributes
#' 
#' @description The functions assist in setting node attributes after a 
#'   network has been initialized.  
#'   
#' @param network An object of class \code{dbn}
#' @param node A named list of character vectors. The name of each element 
#'   corresponds to a node to which the vector of parent names is added or 
#'   replaced. Dynamic style names (\code{parent[t-1]}) are acceptable so 
#'   long as the node is dynamic. 
#' @param add \code{logical(1)}. If \code{TRUE}, the node adopts all unique
#'   parents available between the existing parents and the new parents.
#'   If \code{FALSE}, the old parents are removed and new parents put in 
#'   their place. 
#' @param force \code{logical(1)}. If \code{FALSE} and the node has a model 
#'   object, the new parents will not already exist in the model and 
#'   \code{set_node} will cast an error.  If \code{force = TRUE}, the model
#'   attribute will be dropped and a warning cast.
#' @param ... named arguments where the name corresponds to a node name in 
#'   \code{network} and the new value of the attribute. Use a 
#'   \code{numeric(1)} for \code{set_node_maxt} and a \code{logical(1)}
#'   for the others.
#' 
#' 
#' @section Functional Requirements:
#' \emph{set_node_parent}
#' 
#' \enumerate{
#'  \item When \code{add = TRUE} append the nodes to the \code{parent_raw} and 
#'    \code{parent} attributes of the node.
#'  \item When \code{add = FALSE} replace the nodes in \code{parent_raw} and 
#'    \code{parent} attributes with the new nodes.
#'  \item When \code{force = FALSE}, if a model object exists and new nodes 
#'    are being added that do not exist in the model, cast an error.
#'  \item When \code{force = TRUE}, if a model object exists and new nodes
#'    are being added that do not exist, drop the model and cast a warning.
#'  \item If any name in \code{parent} is not a node in \code{network}, 
#'    drop the element and cast a warning.
#'  \item Cast an error if \code{network} does not have class \code{dbn}
#'  \item Cast an error if \code{parent} is not a named list.
#'  \item Cast an error if \code{parent} has any non-character vector elements.
#'  \item Cast an error if \code{add} is not \code{logical(1)}
#'  \item Cast an error if \code{force} is not \code{logical(1)}
#' }
#' 
#' \emph{set_node_maxt}
#'
#' \enumerate{
#'  \item Correctly reassigns the \code{max_t} attribute of the node.
#'  \item If the name of any element in ... does not match a node name 
#'    in the network, drop the element and cast a warning.
#'  \item Cast an error if x is not a dbn.
#'  \item Cast an error if any element in ... is not \code{numeric(1)}
#'  \item If any of the target nodes are not dynamic, cast an error if 
#'    any of them are parents of at least one other node.
#'  \item If any of the target nodes are not dynamic, and none of them are 
#'    parents of any other node, cast a warning naming any nodes that 
#'    are converted to dynamic.
#' }
#' 
#' \emph{set_node_dynamic, set_node_utility}
#' \enumerate{
#'  \item Correctly reassigns the \code{is_dynamic} attribute of the node.
#'   \item If the name of any element in \code{...} does not match
#'      a node name in the network, drop the element and cast a warning.
#'   \item Cast an error if \code{x} is not a \code{dbn}.
#'   \item Cast an error if any element in \code{...} is not \code{logical(1)}
#'   \item Cast an error if any of the target nodes have children.
#' }
#' 
#' \emph{set_node_decision, set_node_deterministic}
#' \enumerate{
#'   \item Correctly reassigns the \code{is_dynamic} attribute of the node.
#'   \item If the name of any element in \code{...} does not match
#'      a node name in the network, drop the element and cast a warning.
#'   \item Cast an error if \code{x} is not a \code{dbn}.
#'   \item Cast an error if any element in \code{...} is not \code{logical(1)}
#' }
#' 
#' @author Jarrod Dalton and Benjamin Nutter
#' 
#' @seealso \code{\link{get_node_utilities}}, \code{\link{is_node_utilities}}
#' 
#' @export

set_node_parent <- function(network, node, add = TRUE, force = FALSE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          classes = "dbn",
                          add = coll)
  
  checkmate::assert_list(x = node,
                         names = "named",
                         types = "character",
                         add = coll)
  
  checkmate::assert_logical(x = add,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = force,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  not_node <- names(node)[!names(node) %in% network[["node_attr"]][["node_name"]]]
  
  if (length(not_node))
  {
    warning("The following are not nodes in `network`: ",
            paste0(not_node, collapse = ", "))
    
    node <- node[!names(node) %in% not_node]
  }
  
  index <- match(names(node), 
                 network[["node_attr"]][["node_name"]])
  
  # When a node has a model, discrepant parents must either be forced
  # or an error cast notifying the user of the discrepancy
  if (any(has_node_model(network = network,
                         node = names(node))))
  {
    same_parent <- 
      mapply(function(new, old){ all(new %in% old) },
             new = node,
             old = network[["node_attr"]][["parent_raw"]][index],
             SIMPLIFY = FALSE)
    same_parent <- unlist(same_parent)

    if (any(!same_parent & force))
    {
      warning("New parents being forced and models dropped for: ",
              paste0(names(node)[!same_parent], collapse = ", "))
      
      network[["node_attr"]][["model"]][index[!same_parent]] <- 
        lapply(index[!same_parent], 
               function(i) NULL)
    }
    else if (any(!same_parent & !force))
    {
      stop("Cannot add new parents to an exisiting model. ",
           "Consider using force = TRUE or revising the parents added to: ",
           paste0(names(node)[!same_parent], collapse = ", "))
    }
  }
  
  if (add)
  {
    network[["node_attr"]][["parent_raw"]][index] <- 
      mapply(function(old, new) unique(c(old, new)),
             old = network[["node_attr"]][["parent_raw"]][index],
             new = node,
             SIMPLIFY = FALSE)
    
    network[["node_attr"]][["parent"]][index] <- 
      mapply(function(old, new) unique(c(old, new)),
             old = network[["node_attr"]][["parent_raw"]][index],
             new = dag_structure_remove_temporal_suffix(node),
             SIMPLIFY = FALSE)
  }
  else
  {
    network[["node_attr"]][["parent_raw"]][index] <- node
    network[["node_attr"]][["parent"]][index] <- 
      lapply(node, 
             dag_structure_remove_temporal_suffix)
  }
  
  network
}

#' @rdname set_node_utilities
#' @export

set_node_maxt <- function(network, ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          classes = "dbn",
                          add = coll)
  
  something <- list(...)
  
  checkmate::assert_list(x = something,
                         types = "integerish",
                         add = coll)
  
  dots_integerish <- 
    vapply(something,
           FUN = checkmate::test_integerish,
           FUN.VALUE = logical(1),
           len = 1,
           lower = 0)
  
  if (any(!dots_integerish))
  {
    coll$push("All elements to ... must be integerish with length 1 and at least 0")
  }
  
  dynamic_node <- 
    suppressWarnings(
      is_node_dynamic(network = network,
                      node = names(something))
    )
  parent_node <- 
    suppressWarnings(
      is_node_parent(network = network,
                     node = names(something)) 
    )
              
  
  if (any(!dynamic_node & parent_node))
  {
    coll$push(sprintf("The following nodes have children and can not be dynamic: %s",
                      paste0(names(parent_node)[!dynamic_node & parent_node], collapse = ", ")))
  }
  
  checkmate::reportAssertions(coll)
  
  if (any(!dynamic_node & !parent_node))
  {
    warning("The following nodes have been converted to dynamic nodes: ",
            paste0(names(parent_node[!dynamic_node & !parent_node]), 
                   collapse = " "))
    index <- match(names(parent_node),
                   network[["node_attr"]][["node_name"]])
    
    network[["node_attr"]][["is_dynamic"]][index] <- TRUE
  }
  
  something <- unlist(something)
  
  not_in_network <-  
    names(something)[!names(something) %in% network[["node_attr"]][["node_name"]]]
  
  if (length(not_in_network))
  {
    warning("The following are not valid nodes in the network and are ignored: ",
            paste0(not_in_network, collapse = ", "))
    something <- something[!names(something) %in% not_in_network]
  }
  
  index <- match(names(something), 
                 network[["node_attr"]][["node_name"]])
  
  network[["node_attr"]][["max_t"]][index] <- something
  
  network
}

#' @rdname set_node_utilities
#' @export

set_node_dynamic <- function(network, ...)
{
  set_node_something(network =  network,
                     ...,
                     attribute = "is_dynamic")
}

#' @rdname set_node_utilities
#' @export

set_node_decision <- function(network, ...)
{
  set_node_something(network =  network,
                     ...,
                     attribute = "is_decision")
}

#' @rdname set_node_utilities
#' @export

set_node_utility <- function(network, ...)
{
  set_node_something(network =  network,
                     ...,
                     attribute = "is_utility")
}

#' @rdname set_node_utilities
#' @export

set_node_deterministic <- function(network, ...)
{
  set_node_something(network =  network,
                     ...,
                     attribute = "is_deterministic")
}

set_node_something <- function(network, ..., 
                               attribute = c("is_dynamic", "is_decision", 
                                             "is_utility", "is_deterministic"))
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          classes = "dbn",
                          add = coll)
  
  something <- list(...)
  
  length_something <- 
    vapply(something,
           FUN = checkmate::test_logical,
           FUN.VALUE = logical(1),
           len = 1)
  
  if (any(length_something != 1))
  {
    coll$push("All elements in `...` must be a logical(1)")
  }
  
  attribute <- 
    checkmate::matchArg(x = attribute,
                        choices = c("is_dynamic", "is_decision", 
                                    "is_utility", "is_deterministic"),
                        add = coll)
  
  if (attribute %in% c("is_dynamic", "is_utility"))
  {
    parent_node <- 
      suppressWarnings(
        is_node_parent(network = network,
                       node = names(list(...)))
      )
    
    if (any(parent_node))
    {
      coll$push(sprintf("The following nodes have children and can not be %s: %s",
                        sub("^is_", "", attribute),
                       paste0(names(parent_node)[parent_node], collapse = ", ")))
    }
  }
  
  checkmate::reportAssertions(coll)
  
  something <- unlist(something)
  
  not_in_network <-  
    names(something)[!names(something) %in% network[["node_attr"]][["node_name"]]]
  
  if (length(not_in_network))
  {
    warning("The following are not valid nodes in the network and are ignored: ",
            paste0(not_in_network, collapse = ", "))
    something <- something[!names(something) %in% not_in_network]
  }
  
  index <- match(names(something), 
                 network[["node_attr"]][["node_name"]])
  
  network[["node_attr"]][[attribute]][index] <- something
  
  network
}