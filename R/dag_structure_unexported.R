#' @name dag_structure_unexported
#' @aliases dag_structure_remove_temporal_suffix
#' @aliases dag_structure_get_node_and_parent
#' @aliases dag_structure_get_node_str
#' @aliases dag_structure_get_future_dependency
#' @aliases dag_structure_get_parent_list
#' 
#' @title Unexported Utilities for \code{dag_structure}
#' 
#' @description Utilities used by \code{dag_structure} to generate the 
#'   core structure of the \code{dbn} object.
#' 
#' @details These utilities are used to support the \code{dag_structure} 
#'   function.  They are not intended to be exposed to the user and 
#'   may not contain the same level of argument checks, or functional 
#'   requirements.
#'   
#' \code{dag_structure_remove_temporal_suffix} helps to identify the root 
#' node name of a dynamic node definition.  For example, \code{a[t - 1]}
#' becomes \code{a}.
#' 
#' \code{dag_structure_get_node_and_parent} returns a character matrix 
#' that lists the nodes in the network in the first column and the parents
#' of those nodes in the second column.
#' 
#' \code{dag_structure_get_node_str} converts the node formula into a 
#' character vector.
#' 
#' \code{dag_structure_get_future_dependency} looks for and returns 
#' nodes that are defined with a future dependence, such as \code{a[t + 1]}.
#' Future dependencies are not permitted.
#' 
#' \code{dag_structure_get_parent_list} divides the parents in the second 
#' column of the node and parents matrix into individual node identifiers.
#'   
#' @author Benjamin Nutter

#' @param node_str A character vector of node definitions. It is assumed
#'   that these come from the formula definition.


#' @param fm A formula object defining the network

dag_structure_get_node_str <- function(fm)
{
  node_str <- deparse(fm)
  node_str <- paste0(node_str, collapse = " ")
  node_str <- trimws(node_str)
  sub(pattern = "^.*?[~]", 
      replacement = "", 
      x = node_str,
      perl = TRUE)
}

#' @rdname dag_structure_unexported

dag_structure_get_future_dependency <- function(node_str)
{
  matched <- 
    gregexpr(pattern = "\\[[^]+]+[+,*,/][^]]+\\]",
             text = node_str,
             perl = TRUE) 
  
  regmatches(x = node_str,
             m = matched)[[1]]
}

#' @rdname dag_structure_unexported

dag_structure_remove_temporal_suffix <- function(node_str)
{
  gsub(pattern = "(?=\\[).*?(?<=\\])",
       replacement = "",
       x = node_str,
       perl = TRUE)
}

#' @rdname dag_structure_unexported

dag_structure_get_node_and_parent <- function(node_str)
{
  #* Separate the nodes into one column of a matrix 
  #* and the parents into the second.
  node_and_parent <- strsplit(x = node_str, 
                              split = "[|]")
  node_and_parent <-
    lapply(node_and_parent,
           function(x)
           {
             if (length(x) == 1) c(x, "")
             else x
           })
  node_and_parent <- do.call("rbind", node_and_parent)
  node_and_parent <- trimws(node_and_parent)
  
  #* identify implicit parents
  #* These are parents that are not given their own node definition 
  #* in the formula.
  implicit_parent <- 
    lapply(
      node_and_parent[, 2],
      function(x) unlist(strsplit(x, split = "[*]"))
    ) 
  implicit_parent <- lapply(X = implicit_parent,
                            FUN = trimws) 
  implicit_parent <- unlist(implicit_parent)
  implicit_parent <- unique(implicit_parent)
  implicit_parent <- implicit_parent[!is.na(implicit_parent)]
  implicit_parent <- sub(pattern = "(?=\\[).*?(?<=\\])",
                         replacement = "[t]",
                         x = implicit_parent,
                         perl = TRUE)
  
  implicit_parent <- 
    implicit_parent[!implicit_parent %in% node_and_parent[, 1]]
  
  #* Return the matrix of nodes and parents.
  rbind(node_and_parent,
        cbind(implicit_parent,
              rep("", length(implicit_parent))))
}

#' @rdname dag_structure_unexported
#' @param edge A character matrix. The first column is a node. The 
#'   second column is character string of parents.

dag_structure_get_parent_list <- function(edge)
{
  parent_list <- strsplit(edge[, 2], split = "[*]")
  parent_list <- lapply(parent_list,
                        function(e) trimws(e[e != ""]))
  stats::setNames(parent_list,
           trimws(edge[, 1]))
}