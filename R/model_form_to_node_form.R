#' @name model_form_to_node_form
#' @title Convert a Model Object Formula to a Node Formula
#' 
#' @description A utility for \code{dbn.list} to take the model
#'   formula of an object and convert it to a format that can be used 
#'   in creating a \code{dbn} object. Different objects may require
#'   different methods to get the formula, so these are written as 
#'   methods.
#'   
#' @param fit An object with a \code{call} element
#' @param ... Arguments to pass to other methods.  Currently ignored.
#' 
#' @return 
#' Returns a character string with the converted formula
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Returns a character string with a converted formula
#'   \item Require that \code{xtabs} models only have one variable.
#'   \item Should formula extraction fail, return an error message with
#'         directions to file an issue on GitHub.
#' }
#' 
#' @author Benjamin Nutter
#'
#' @export

model_form_to_node_form <- function(fit, ...)
{
  UseMethod("model_form_to_node_form")
}

#' @rdname model_form_to_node_form
#' @export

model_form_to_node_form.default <- function(fit, ...)
{
  fm <- tryCatch(formula(fit),
                 error = function(cond) sprintf("ERROR: %s", cond))
  
  if (is.character(fm))
  {
    coll <- checkmate::makeAssertCollection()
    coll$push(paste0("An error occured while extracting the network formula. ",
                     "It seems this model object is not supported. ",
                     "If you feel this model object should be supported, ",
                     "please submit an issue on GitHub ",
                     "(https://github.com/nutterb/dbn/issues) ", 
                     "and include this error. '",
                     fm, "'"))
    checkmate::reportAssertions(coll)
  }
  
  lhs <- all.vars(update(fm, . ~ 0))
  rhs <- all.vars(update(fm, 0 ~ .))
  
  #* Node has parents
  if (length(rhs))
  {
    sprintf("%s | %s",
            lhs,
            paste0(rhs, collapse = " * "))
  }
  #* Node has no parents
  else
  {
    lhs
  }
}

#' @rdname model_form_to_node_form
#' @export

model_form_to_node_form.xtabs <- function(fit, ...)
{
  response <- formula(attr(fit, "call")) %>%
    update(., 0 ~ .) %>%
    all.vars()
  
  if (length(response) > 1)
  {
    coll <- checkmate::makeAssertCollection()
    coll$push("`dbn` only accepts `xtabs` models with one variable.")
    checkmate::reportAssertions(coll)
  }
  
  response
}