#' @name model_to_node_unexported
#' @title Unexported Utilities for \code{model_to_node}
#' 
#' @description Support functions for \code{model_to_node}. These assist with
#' parsing the formula object from a model into a node definition to include 
#' in \code{dbn}.
#' 
#' @param side A left or right hand side of a formula object.  The object 
#'   should be deparsed and split on \code{~} before passing into 
#'   \code{mode_to_node_deparse_side}.
#'   
#' @seealso \code{\link{model_to_node}}

model_to_node_deparse_side <- function(side)
{
  side <- unlist(strsplit(side, 
                          "([*]|[+]|[-]|[/]|[:]|[\\^])"))
  
  in_func <- grepl(pattern = "[(]",
                   x = side)
  
  replace_pattern <- vector("character", 
                            length= length(side))
  replace_pattern[grepl(pattern = "[(]",
                        x = side)] <- ",.+$"
  replace_pattern[grepl("Surv[(]",
                        x = side)] <- ".+,"
  
  side[in_func] <- 
    grep_extract(x = side[in_func],
                 pattern = "(?<=[(])(.*?)(?=[)])",
                 perl = TRUE)
  
  has_comma <- grepl(pattern = ",", 
                     x = side)
  
  side[has_comma] <- 
    sub(pattern = if (!any(has_comma)) "" else replace_pattern[has_comma],
        replacement = "",
        x = side[has_comma])
  side <- trimws(side)
  side <- side[is_syntactic(side)]
  paste0(side, collapse = " * ")
}