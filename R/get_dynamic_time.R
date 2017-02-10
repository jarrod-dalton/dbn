#' @name get_dynamic_time
#' @title Get the Time Suffix from a Dynamic Node
#' 
#' @description Returns the integer suffix appended to a dynamic node.
#' 
#' @param node \code{character(1)} giving a node name.
#' 
#' @author Benjamin Nutter
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return the integer suffix at the end of a node name.
#'   \item If the node is not dynamic, return \code{NA}.
#' }

get_dynamic_time <- function(node)
{
  m <- regexpr(pattern = "_\\d{1,10}$",
               text =  node)
  node <- regmatches(x = node,
                     m = m)
  node <- sub(pattern = "_",
              replacement = "",
              x = node)
  as.numeric(node)
}