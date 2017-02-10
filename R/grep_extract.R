#' @name grep_extract
#' @title Extract a Character String Using a Regular Expression
#' 
#' @describe Extracts a substring from a character string based on a 
#'   regular expression.  This is similar to \code{stringr::str_extract}, 
#'   but relies only on base \code{R} methods.
#'   
#' @param x Character vector.
#' @param pattern \code{character(1)}, a regular expression to apply to 
#'   \code{x}
#' @param invert \code{logical(1)}, if \code{TRUE}, extract the non-matched 
#'   substrings.
#' @param ... Additional arguments to pass to \code{\link{gregexpr}}.
#' 
#' @author Benjamin Nutter
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Returns the substring that matches the regular expression.
#'   \item If no match is generated, return \code{NA}
#' }

grep_extract <- function(x, pattern, invert = FALSE, ...)
{
  mtch <- gregexpr(pattern = pattern,
                   text = x,
                   ...)
  mtch <- regmatches(x = x,
                     m = mtch,
                     invert = invert)
  vapply(X = mtch,
         FUN = function(x) if (length(x)) x else NA_character_,
         FUN.VALUE = character(1))
}