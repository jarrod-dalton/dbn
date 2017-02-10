#' @name expand_node
#' @title Convert Root Node to Temporal Node
#' 
#' @description Appends the \code{_n} suffix to the root node. Presumably,
#'   this is of interest in plotting the expanded network.
#'   
#' @param node \code{character(1)} naming a root node
#' @param time \code{numeric(1)} giving \code{max_t} value for the node
#' @param dynamic \code{logical(1)} indicating if the node is a dynamic node
#' 
#' @author Benjamin Nutter
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item If \code{dynamic = TRUE}, returns a character vector where
#'     each element follows the format \code{"node_t"}.
#'   \item If \code{dynamic = FALSE}, returns \code{node}.
#' }

expand_node <- function(node, time, dynamic)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = node,
                              len = 1,
                              add = coll)
  
  checkmate::assert_numeric(x = time,
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = dynamic,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (dynamic)
  {
    sprintf("%s_%s", node, 0:time)
  }
  else 
  {
    node
  }
}
