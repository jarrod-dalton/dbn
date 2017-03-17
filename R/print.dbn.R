#' @name print.dbn
#' @title Print a \code{dbn} Object
#' 
#' @description Prints the node formulae of a \code{dbn} network.
#' 
#' @param x A \code{dbn} object.
#' @param ... Additional arguments to pass to other methods. Currently ignored.
#' 
#' @export

print.dbn <- function(x, ...)
{
  cat("A `dbn` object with",
      nrow(x[["node_attr"]]),
      "nodes\n")
  cat(get_node_formula(x), 
      sep = "\n")
}