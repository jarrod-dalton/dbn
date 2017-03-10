#' @name is_syntactic
#' @title Check for Syntactically Valid R Names
#' 
#' @description Determines if the elements of a character string are 
#'   syntactically valid names.
#'   
#' @param x a character vector.
#' 
#' @return A logical vector with the same length as \code{x}
#' 
#' @author Hadley Wickham
#' 
#' @source \url{http://r.789695.n4.nabble.com/Syntactically-valid-names-td3636819.html}
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Return a logical vector the same length as \code{x}
#'   \item Cast an error if \code{x} is not a character vector
#' }
#' 
#' @export

is_syntactic <- function(x)
{
  checkmate::assert_character(x = x)
  
  x == make.names(x)
}