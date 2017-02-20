#' @name dag_structure
#' @title Convert a formula into an adjacency matrix
#' 
#' @description Builds the core structure of the \code{dbn} object.
#' 
#' @param fm A formula object with no left hand side. See Details in 
#' \code{\link{dbn}}.
#' @param coll An optional \code{assertCollection} object for returning
#'   validation errors.
#'   
#' @return When successful, a list is returned with the following elements
#' \itemize{
#'  \item{\code{edge} }{A two column matrix. The first column lists the 
#'    nodes in the network. The second column shows the parents for the
#'    respective nodes. The parents are separated by a \code{*}}
#'  \item{\code{raw_edge} }{Similar to \code{edge}, but the nodes and parents
#'    also display the time dependency structure.}
#'  \item{\code{root_node} }{A vector naming the nodes in the network.}
#'  \item{\code{raw_node} }{Similar to \code{node}, but the nodes are 
#'    displayed with the \code{[t]} suffix if they are dynamic.}
#'  \item{\code{root_parent} }{A list of vectors, one for each node, naming the 
#'    parents of the node.}
#'  \item{\code{raw_parent} }{A list of vectors, one for each node, naming the
#'    parents of the node, but with the time dependency suffix.}
#'  \item{\code{is_dynamic} }{A logical vector indicating if each node is 
#'    dynamic.}
#' }
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return an error when \code{fm} is not a formula.
#'   \item Return an error when \code{fm} has a left hand side.
#'   \item Return an error if a dynamic dependency refers to a 
#'     future time point (such as \code{t + 1}).
#'   \item Return a list when successful.
#' }
#'

dag_structure <- function(fm, coll = NULL){

# Argument Validations ----------------------------------------------

  #* `fm` is a formula
  #* if `fm` is not a formula, nothing else matters, so we 
  #* are skipping the collection and going straight to the error.
  checkmate::assertClass(x = fm,
                         classes = "formula")
  
  if (is.null(coll))
  {
    coll <- checkmate::makeAssertCollection()
  }
  
  #* `fm` has no left hand side
  lhs <- all.vars(stats::update(fm, . ~ 0))
  
  if (!all(lhs == ".")) 
  {
    coll$push("`fm` may not have a left hand side.")
  }
  
  #* `fm` has at least one variable in the right hand side
  #* While writing tests I discovered that it is pretty 
  #* hard to make a formula with no right hand side.  
  #* (R won't let you do it) I'm leaving the check in place, 
  #* but it will go untested
  nodes <- all.vars(stats::update(fm, 0 ~ .))
  
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

utils::globalVariables(c("."))