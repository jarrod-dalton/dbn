#' @name set_node_model
#' @title Set and Retrieve Models for \code{dbn} Objects
#' 
#' @description Access and replace models associated with nodes in a \code{dbn}
#'   network.  This provides a more convenient interface with which to 
#'   make updates to the network.
#'   
#' @param network An object of class \code{dbn}
#' @param node A character vector naming nodes in the network.
#' @param add \code{logical(1)}. If \code{TRUE}, the node adopts all unique
#'   parents available between the existing parents and the new parents.
#'   If \code{FALSE}, the old parents are removed and new parents put in 
#'   their place. 
#' @param force \code{logical(1)}. If \code{FALSE} and the node has a model 
#'   object, the new parents will not already exist in the model and 
#'   \code{set_node} will cast an error.  If \code{force = TRUE}, the model
#'   attribute will be dropped and a warning cast.
#' @param ... named arguments where the name corresponds to a node name in 
#'   \code{network} and the value is the model to assign to the node. 
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
#' \emph{set_node_model}
#' 
#' \enumerate{
#'   \item Replace the current node model with the specified model.
#'   \item When \code{add = TRUE} append any parents in the model not 
#'     currently in the node's parents to the \code{parent_raw} and \code{parent}
#'     attributes of the node.
#'   \item When \code{add = FALSE} replace the nodes in \code{parent_raw}
#'     and \code{parent} with the parents in the model.
#'   \item When \code{force = FALSE}, if parents in the model do not match the
#'     parents of the node, cast an error.
#'   \item When \code{force = TRUE}, if there are new parents to be added, 
#'     perform the appropriate action based on the \code{add} argument.
#'   \item When \code{force = TRUE} and \code{add = TRUE}, if the node has 
#'     parents that aren't in the model, cast an error. \code{add = FALSE}
#'     should be used to remove parents.
#'   \item Cast an error if \code{network} is not a \code{dbn} object.
#'   \item Cast an error if \code{...} is unnamed.
#'   \item Cast an error if any model object in \code{...} cannot be parsed
#'     by \code{model_to_node}
#'   \item Cast an error if \code{add} is not a \code{logical(1)}.
#'   \item Cast an error if \code{force} is not a \code{logical(1)}.
#'   \item Cast an error if the model has more than one response variable.
#'   \item Cast an error if the response variable does not match the node name.
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

#' @rdname set_node_model
#' @export

set_node_model <- function(network, ..., add = FALSE, force = FALSE)
{
  model_list <- list(...)
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = network,
                          classes = "dbn",
                          add = coll)
  
  checkmate::assert_list(x = model_list,
                         names = "named",
                         add = coll,
                         .var.name = "...")
  
  checkmate::assert_logical(x = add,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = force,
                            len = 1, 
                            add = coll)
  
  node_form <- 
    lapply(model_list,
           function(x) tryCatch(model_to_node(x),
                                error = function(cond) FALSE))
  
  # each element in node_form will be a character if the model can 
  # be processed by model_to_node.  If any value is logical, it 
  # failed, and we want to cast an error.
  
  failed_model_to_node <- vapply(X = node_form, 
                                 FUN = is.logical, 
                                 FUN.VALUE = logical(1))
  if (any(failed_model_to_node))
  {
    coll$push(
      sprintf("The following models could not be processed by `model_to_node`: %s",
              paste0(names(model_list)[failed_model_to_node], collapse = ", "))
    )
  }
  
  more_than_one_response <- vapply(X = node_form,
                                   FUN = function(x) isTRUE(grepl("[*].+[|]", x)),
                                   FUN.VALUE = logical(1))
  
  if (any(more_than_one_response))
  {
    coll$push(
      sprintf("The following models have more than one response variable: %s",
              paste0(names(model_list)[more_than_one_response], collapse = ", "))
    )
  }
  
  mismatch_response <- vapply(X = node_form,
                              FUN = function(x) trimws(gsub(pattern = "[|].+$",
                                                            replacement = "",
                                                            x = x)),
                              FUN.VALUE = character(1))
  mismatch_response <- mismatch_response != names(model_list)
  
  if (any(mismatch_response))
  {
    coll$push(
      sprintf("The following models' response variable does not match the given node name: %s",
              paste0(names(model_list)[mismatch_response], collapse = ", "))
    )
  }
  
  checkmate::reportAssertions(coll)
  
  node <- trimws(sub(pattern = "[|].+$", 
                     replacement = "",
                     x = node_form))
  
  parent <- trimws(sub(pattern = "^.+[|]",
                       replacement = "",
                       x = node_form))
  parent <- mapply(function(n, p) {if (n == p) character(0) else p},
                   n = node,
                   p = parent,
                   SIMPLIFY = FALSE)
  parent <- lapply(parent,
                   function(x) trimws(unlist(strsplit(x, "[*]"))))
  
  parent_raw <- 
    mapply(
      function(x, node)
      {
        if (is_node_dynamic(network, node)) paste0(x, "[t - 1]")
        else x
      },
      x = parent,
      node = names(parent),
      SIMPLIFY = FALSE
    )
  
  index <- match(node,
                 network[["node_attr"]][["node_name"]])
  
  if (!force)
  {
    new_parent <- 
      mapply(function(existing, new){new[!new %in% existing]},
             network[["node_attr"]][["parent"]][index],
             parent,
             SIMPLIFY = FALSE) 
    
    has_new_parent <- vapply(X = new_parent, 
                             FUN = length, 
                             FUN.VALUE = numeric(1))
    
    if (any(as.logical(has_new_parent)))
    {
      coll$push(
        sprintf("The following models have parents not listed for the node. Use `force = TRUE` to add them: %s",
                paste0(names(model_list)[has_new_parent], collapse = ", "))
      )
      checkmate::reportAssertions(coll)
    }
  }
  else
  {
    if (add)
    {
      exist_not_in_new <- mapply(function(existing, new){existing[!existing %in% new]},
                                 network[["node_attr"]][["parent"]][index],
                                 parent[index],
                                 SIMPLIFY = FALSE) 
      
      has_exist_not_in_new <- vapply(X = exist_not_in_new, 
                                     FUN = length, 
                                     FUN.VALUE = numeric(1))
      
      if (any(as.logical(has_exist_not_in_new)))
      {
        coll$push(sprintf("In the following models, fewer parents are named than exist in the node. Use `add = FALSE` to force a refresh of the parents: %s",
                         paste0(names(model_list)[as.logical(has_exist_not_in_new)], 
                                collapse = ", ")))
        
        checkmate::reportAssertions(coll)
      }

      new_not_in_exist <- mapply(function(existing, new){new[!new %in% existing]},
                                 network[["node_attr"]][["parent"]][index],
                                 parent[index],
                                 SIMPLIFY = FALSE) 
      
      new_not_in_exist_raw <- 
        mapply(function(x, node) 
               {
                 if (is_node_dynamic(network, node)) paste0(x, "[t - 1]")
                 else x
               },
               x = new_not_in_exist,
               node = names(new_not_in_exist))
      
      has_new_not_in_exist <- vapply(X = new_not_in_exist, 
                                     FUN = length, 
                                     FUN.VALUE = numeric(1))
      
      network[["node_attr"]][["parent"]][index] <- 
        mapply(function(new, exist) unique(c(new, exist)),
               new = new_not_in_exist,
               exist = network[["node_attr"]][["parent"]][index],
               SIMPLIFY = FALSE)
      
      network[["node_attr"]][["parent_raw"]][index] <- 
        mapply(function(new, exist) unique(c(new, exist)),
               new = new_not_in_exist_raw,
               exist = network[["node_attr"]][["parent_raw"]][index],
               SIMPLIFY = FALSE)
      
      
    }
    else
    {
      network[["node_attr"]][["parent"]][index] <- parent[index]
      network[["node_attr"]][["parent_raw"]][index] <- parent_raw[index]
    }
  }
  
  network[["node_attr"]][["model"]][index] <- model_list
  
  network
  # list(node, parent)
}