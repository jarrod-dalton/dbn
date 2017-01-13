#' @name dbn
#' @title Dynamic Bayesian Network
#' 
#' @description Generate a Dynamic Bayesian Network.  This section needs more 
#'   detail as well.  As the crux of the package, perhaps a brief introduction
#'   to the topic and its application.  Maybe wait until the first 
#'   vignette is written, and copy content from there?
#'   
#' @param nodes Either a formula that defines the network (see Details) 
#'   or a list of model objects.
#' @param ... additional arguments for other methods. currently ignored.
#' 
#' @details 
#' The formula is used to identify nodes and their parentage.  A node 
#' definition may be of the format \code{~ [node] | [parent1] * [parent2]}. 
#' Mutliple nodes may be defined by adding them with the \code{+} operator 
#' (\code{~ node1 | parent1 * parent2 + node2 | parent3}).
#' 
#' It is permissible to include a node only as a parent; it will still be 
#' identified as a distinct node in the network.  Each node that has at least 
#' one parent will need to have its own entry in the formula.
#' 
#' @return 
#' Returns a \code{dbn} object with attributes to be described:
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item Accept a one sided formula for the network specification or a 
#'     list of model objects.
#'   \item Return an object of class \code{dbn}
#'   \item The \code{dbn} object has as elements the original \code{nodes}
#'     formula; The adjacency matrix for the network; a table of each node's
#'     attributes.
#'   \item The table of node attributes includes the following columns:
#'     \code{node_name} (character(1)), \code{parents} (character vector),
#'     \code{is_decision} (logical), \code{is_utility} (logical),
#'     \code{is_temporal} (logical), \code{model} (an object to assist in 
#'     simulation of posterior)
#' }
#' 
#' @author Benjamin Nutter
#' 
#' @export

dbn <- function(nodes, ...)
{
  UseMethod("dbn")
}

#' @rdname dbn
#' @export

dbn.formula <- function(nodes, ...)
{
  dag_str <- dag_structure(nodes)
  
  node_attr <- 
    tibble::data_frame(node_name = names(dag_str[["parent_list"]])) %>%
    dplyr::mutate(parent = lapply(dag_str[["parent_list"]], identity),
                  is_dynamic = FALSE,
                  n_dynamic = 1,
                  self_depend = FALSE,
                  is_decision = FALSE,
                  is_utility = FALSE,
                  is_deterministic = FALSE,
                  model = lapply(dag_str[["parent_list"]], function(x) NULL))
  
  structure(list(network = nodes,
                 adjacency_matrix = dag_str[["adjacency_matrix"]],
                 node_attr = node_attr),
            class = "dbn")
}

#' @rdname dbn
#' @export

dbn.list <- function(nodes, ...)
{
  #* convert the model formulae to a dbn formula
  #* Pass this formula to `dbn.formula`
  form <- 
    lapply(nodes,
           model_form_to_node_form) 
  
  #* Identify the response variable in each model
  response_var <- sub(pattern = " [|].+$", 
                      replacement = "",
                      x = form)
  
  network <- 
    form %>%
    paste0(., collapse = " + ") %>%
    sprintf("~ %s", .) %>%
    as.formula() %>%
    dbn.formula()
  
  #* match the name of the response variable to the 
  #* row of its node name in the node attributes table
  attr_row <- match(response_var,
                    network[["node_attr"]][["node_name"]])
  
  #* Insert the models
  network[["node_attr"]][["model"]][attr_row] <- 
    nodes

  network
}