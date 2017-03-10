#' @name model_to_node
#' @title Convert a Model to a Node Description
#' 
#' @description Write a node definition to be placed in a network formula. 
#'   This is used when creating networks from a list of models.
#'   
#' @param m A model object
#' @param ... Additional arguments to pass to other methods
#' 
#' @section Functional Requirements:
#' Default Method
#' \enumerate{
#'   \item Cast an error if the model formula has more than one element on 
#'         the left hand side.
#'   \item Assume any node in a function is given as the first argument
#'         of the function unless the function is \code{Surv}.
#' }
#' 
#' Additional Requirement for the \code{xtabs} method
#' \enumerate{
#'   \item Only allow one variable in \code{xtabs} definitions 
#' }

model_to_node <- function(m, ...)
{
  UseMethod("model_to_node")
}

#' @rdname model_to_node

model_to_node.default <- function(m, ...)
{
  form <- stats::terms(m)
  form <- deparse(form)
  form <- paste0(form, collapse = " ")
  
  side <- unlist(strsplit(form, "~"))
  
  node <- model_to_node_deparse_side(side[[1]])
  
  parent <- model_to_node_deparse_side(side[[2]])
  
  sprintf("%s | %s",
          node, 
          paste0(parent, collapse = " * "))
}

#' @rdname model_to_node

model_to_node.xtabs <- function(m, ...)
{
  node <- attr(m, "call")[["formula"]]
  node <- deparse(node)
  node <- unlist(strsplit(node, "~"))[[2]]
  node <- model_to_node_deparse_side(node)
  
  if (grepl("[*]", node))
  {
    stop("`xtabs` models may only have one independent variable in dbn")
  }
  
  node
}
