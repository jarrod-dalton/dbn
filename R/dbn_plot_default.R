#' @name dbn_plot_default
#' @title Show and Modify \code{dbn} Default Settings for Plots
#' 
#' @description Display the current defaults used for plotting a dynamic 
#'   belief network.  Modifications may also be made.  
#'   
#' @section Functional Requirements:
#' \enumerate{
#'   \item The non-assignment method returns a printed string for each node
#'         type showing the plotting parameters.
#'   \item The assignment method accepts a named list where the name is one
#'         of \code{decision}, \code{deterministic}, \code{dynamic}, 
#'         \code{generic} or \code{utility}
#'   \item Each of the named lists is a named list. The names of these lists
#'         may be a valid GraphViz attribute.
#'   \item The user may choose to replace all contents strictly with the 
#'         contents of \code{new_opt}.
#'   \item The user may choose to overwrite existing options with what is
#'         in \code{new_opt} without altering existing options not in 
#'         \code{new_opt}.
#'   \item Items in \code{new_opt} that aren't listed in existing options are 
#'         added.
#' }
#' 
#' @source 
#' GraphViz attributes \url{http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#attributes}
#' 
#' @examples 
#' dbn_plot_default()
#' 
#' dbn_set_plot_default(
#'   list(generic = list(shape = 'triangle'),
#'        dynamic = list(fillcolor = 'purple'))
#' )
#' 
#' dbn_plot_default()
#' 
#' dbn_restore_plot_default()
#' 
#' @export

dbn_plot_default <- function()
{
  .opt <- getOption("dbn_plot_node_default")
  
  .opt <- 
    lapply(.opt,
         FUN = function(opt){sprintf("%s = %s", names(opt), opt)})
  
  .opt <- lapply(.opt, paste0, collapse = "; ")
  
  .opt <- unlist(.opt)
  
  cat(sprintf("%s\n%s\n\n",
          names(.opt),
          .opt),
      sep = "")
}

#' @rdname dbn_plot_default
#' @param new_opt A list of named lists giving the new values to apply as 
#'   defaults.  See Details
#' @param replace A \code{logical(1)}. If \code{TRUE}, the entire contents of 
#'   the current defaults are obliterated and replaced with what is in
#'   \code{new_opt}.  if \code{FALSE}, options that exist in the current 
#'   settings and \code{new_opt} are replaced with the \code{new_opt value}.
#'   Options that only exist in \code{new_opt} are added.  Options that
#'   only exist in the current options are not touched.
#'   
#' @details The list given to \code{new_opt} may contain lists that are 
#'   named \code{generic}, \code{dynamic}, \code{decision}, 
#'   \code{deterministic}, or \code{utility}. 
#'   
#' Each of those lists may have any number of named elements where the 
#' name is the GraphViz attribute name (see References). 
#' 
#' @export

dbn_set_plot_default <- function(new_opt, replace = FALSE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_list(x = new_opt,
                         names = "named",
                         add = coll)
  
  checkmate::assert_subset(x = names(new_opt),
                           choices = c("generic", "decision", "deterministic",
                                       "dynamic", "utility"),
                           add = coll)
  
  sublist_named <- 
    vapply(X = new_opt,
           FUN = checkmate::test_list,
           FUN.VALUE = logical(1),
           names = "named")
  
  if (!all(sublist_named))
  {
    coll$push("Each list in `new_opt` must be a named list")
  }
  
  sublist_invalid_name <- 
    lapply(X = lapply(new_opt, names),
           FUN = function(x, choices) x[!x %in% choices],
           choices = c("color",     "fillcolor", "fontcolor", "alpha",
                       "shape",     "style",     "sides",     "peripheries",
                       "fixedsize", "height",    "width",     "distortion",
                       "penwidth",  "x",         "y",        "tooltip", 
                       "fontname",  "fontsize",  "icon"))
  
  if (any(as.logical(vapply(sublist_invalid_name, length, numeric(1)))))
  {
    printed_nicely <- 
      sprintf("%s- %s",
              names(sublist_invalid_name),
              vapply(sublist_invalid_name,
                     paste0, 
                     character(1),
                     collapse = ", "))

    coll$push(sprintf("The following are not valid GraphViz attributes:  %s",
                      paste0(printed_nicely, collapse = ";  ")))
  }
  
  checkmate::assert_logical(x = replace,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Do a total replace of the values with the list given
  if (replace)
  {
    options(dbn_plot_node_default = new_opt)
    return(invisible())
  }

  # Replace existing value and add values not currently in defaults
  set_opt <- getOption("dbn_plot_node_default")
  
  for (node_type in seq_along(new_opt))
  {
    set_opt[[node_type]][names(new_opt[[node_type]])] <- 
      new_opt[[node_type]]
  }
  
  options(dbn_plot_node_default = set_opt)
  
  return(invisible())
}

#' @rdname dbn_plot_default
#' @export

dbn_restore_plot_default <- function()
{
  options(
    dbn_plot_node_default = 
      list(generic = list(shape = "ellipse"),
           dynamic = list(shape = "circle",
                          style = "filled",
                          fillcolor = "gray70"),
           decision = list(shape = "rect",
                           style = "filled",
                           fillcolor = "#6BAED6"),
           deterministic = list(shape = "ellipse",
                                color = "gray70",
                                fontcolor = "gray70"),
           utility = list(shape = "diamond",
                          style = "filled",
                          fillcolor = "#FFFFB2"))
  )
}
