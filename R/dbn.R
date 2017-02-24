#' @name dbn
#' @title Dynamic Belief Network
#' 
#' @description Construct a Dynamic Belief Network.  DBNs contain information 
#'   regarding how nodes and variables relate to each other. Relationships 
#'   may be defined over time with dynamic variables, and may also contain 
#'   static variables that do not change over time.
#'   
#' @param nodes Either a \code{formula} or a \code{list} of models. See 
#'   Details.
#' @param max_t \code{numeric(1)}, integerish. Determines the maximum 
#'   time points to extend the model. The initial time point is assumed to
#'   be \code{t = 0}.
#' @param ... Additional arguments to be passed to other methods. 
#'   Current ignored.
#'   
#' @details The formula object may not have a left hand side.  The right
#' hand side is built with a syntax where a node is identified on the 
#' left of a \code{|} (pipe) and its parents on the right.  If a node has 
#' multiple parents, they are separated by a \code{*} (asterisk).  Thus
#' \code{q | a * b} identifies the node \code{q} with parents. Add additional
#'  nodes with the \code{+} operator (e.g. \code{q | a * b + a | c * d}). 
#'  There is no limit to how often a node may appear in the parents.  Any nodes
#'  that are listed in the parents but not as a specific node will still be
#'  identified as a node, but with no parents.
#'   
#' @section Functional Requirements (General):
#' \enumerate{
#'  \item{A generic that has \code{formula} and \code{list} methods}
#'  \item{\code{dbn} objects consist of, at a minimum, a network formula
#'        and a \code{tbl_df} object providing 
#'        node attributes.}
#'  \item{The \code{tbl_df} object contains, at a minimum, columns for 
#'        the node names; the parents; flags for dynamic, decision, utility, and 
#'        deterministic nodes; and a model object.}
#'  \item{Provides an argument that accepts an integerish value indicating 
#'        the number of time periods over which the network may be observed.}
#' }   
#'   
#' @export

dbn <- function(nodes, max_t = 0, ...)
{
  UseMethod("dbn")
}

#' @rdname dbn
#' @section Functional Requirements (\code{formula} method):
#' \enumerate{
#'  \item{Dynamic relationships may be defined relative to time \code{t} 
#'        using syntax such as \code{t - 1}.}
#'  \item{Dynamic relationships may not be defined to depend on future
#'        events, i.e. \code{t + 1}.}
#'  \item{Dynamic relationship time designations must be integerish with
#'        where \code{t} is non-negative.}
#'  \item{\code{dbn} returns an object of class \code{dbn}.}
#' }   
#' @export

dbn.formula <- function(nodes, max_t = 0, ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = max_t,
                               min = 0,
                               len = 1,
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  dag_str <- dag_structure(nodes)
  
  node_attr <- 
    tibble::data_frame(
      node_name = dag_str[["root_node"]],
      parent = lapply(dag_str[["root_parent"]], identity),
      is_dynamic = dag_str[["is_dynamic"]],
      max_t = max_t,
      is_decision = FALSE,
      is_utility = FALSE,
      is_deterministic = FALSE,
      model = lapply(dag_str[["root_node"]], function(x) NULL),
      node_name_raw = dag_str[["raw_node"]],
      parent_raw = lapply(dag_str[["raw_parent"]], identity)
    )
  
  structure(list(network = nodes,
                 node_attr = node_attr),
            class = "dbn")
}

#' @rdname dbn
#' @section Functional Requirements (\code{formula} method):
#' \enumerate{
#'  \item{The list method converts existing model objects into a 
#'        \code{dbn} structure.}
#' }   
#' @export

dbn.list <- function(nodes, ...)
{
  message("This method is not yet implemented")
}