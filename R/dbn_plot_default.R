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
#' }
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