#' @name dag_structure
#' @title Convert a formula into an adjacency matrix
#' 
#' @description Building the network object requires an adjacency matrix to
#'   identify relationships, and an indication of inheritance to identify 
#'   which nodes inherit from which parents.  \code{dag_adj_matrix} creates
#'   the adjacency  matrix, and \code{dag_parentage} identifies the parents 
#'   for each node.  
#'   
#' @param fm A formula object with no left side. See details for formula 
#'   structure.
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
#' These functions are not strictly necessary, as the tasks they 
#' perform could be done by the \code{gRbase} package. However, the 
#' \code{gRbase} package depends on a couple of Bioconductor packages, and 
#' I've had too many headaches with the testing and checking of packages 
#' to include \code{RBGL}, so I'm just avoiding that headache.  I'm sure 
#' I'll come to regret that eventually, but since I don't need to full 
#' functionality of \code{gRbase}, I'll take my chances.
#' 
#' @return 
#' \code{dag_adj_matrix} returns an n x n matrix where n is the number of 
#' distinct nodes identified in the formula.  A 0 entry indicates no edge
#' exists between the nodes.  A 1 entry indicates an edge exists.
#' 
#' \code{dag_parentage} returns a named list with one element for each 
#' distinct node identified in the formula.  Each element is a character
#' vector of the parents for that node.  If a node as no elements, the 
#' entry is \code{character(0)}.
#' 
#' @author Benjamin Nutter
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item Require a one-sided formula with no left side.
#'  \item Return an n x n matrix where n is the number of distinct nodes
#'  \item Return a 0/1 matrix where 1 indicates the presence of an edge
#'  \item Return a named list with one element per node. Each element gives
#'        the parents of that node
#' }
#' 

dag_structure <- function(fm)
{
  #******************************************************************
  #* Argument Validation
  #* 1. `fm` is a formula
  #* 2. `fm` has no left hand side
  #* 3. `fm` has at least one variable in the right hand side
  #******************************************************************
  
  #* 1. `fm` is a formula
  #*    if `fm` is not a formula, nothing else matters, so we 
  #*    are skipping the collection and going straight to the error.
  checkmate::assertClass(x = fm,
                         classes = "formula")
  
  coll <- checkmate::makeAssertCollection()
  
  #* 2. `fm` has no left hand side
  lhs <- all.vars(update(fm, . ~ 0))
  
  if (!all(lhs == ".")) 
  {
      coll$push("`fm` may not have a left hand side.")
  }
  
  #* 3. `fm` has at least one variable in the right hand side
  #*    While writing tests I discovered that it is pretty 
  #*    hard to make a formula with no right hand side.  
  #*    (R won't let you do it) I'm leaving the check in place, 
  #*    but it will go untested
  nodes <- all.vars(update(fm, 0 ~ .))
  
  if (all(nodes == "."))
  {
    coll$push("`fm` must have a right hand side")
  }
  
  checkmate::reportAssertions(coll)
  
  #******************************************************************
  #* Function Body
  #******************************************************************
  
  #* Parent List
  
  #* Make a matrix of nodes (column 1) and parents (column 2)
  edge <- 
    as.character(fm)[-1] %>%
    stringr::str_split(pattern = "[+]") %>%
    unlist() %>%
    stringr::str_split_fixed(pattern = "[|]",
                             n = 2)
  
  #* Restructure the matrix into a list
  parent_list <- 
    edge[, 2] %>%
    stringr::str_split(pattern = "[*]") %>%
    lapply(function(e) trimws(e[e != ""])) %>%
    stats::setNames(trimws(edge[, 1]))
  
  #* Find any parents not listed.
  #* This occurs when a parent is defined after the |, but does not 
  #* have its own distinct definition.
  #* It needs to be added to the parent list
  
  missing_from_parent <- setdiff(nodes, names(parent_list))
  
  if (length(missing_from_parent))
  {
    parent_list <- lapply(missing_from_parent,
           function(x) character(0)) %>%
      stats::setNames(missing_from_parent) %>%
      c(parent_list, .)
  }

  #* Construct the matrix with all zeros
  #* the `nodes` variable was defined during the argument validations
  adjacency_matrix <- matrix(rep(0, length(nodes)^2),
                             nrow = length(nodes),
                             dimnames = list(nodes, nodes))
  
  #* Insert the edges where identified.
  for (i in seq_along(parent_list))
  {
    if (length(parent_list[[i]]))
    {
      adjacency_matrix[names(parent_list)[i], parent_list[[i]]] <- 1
    }
  }
  
  list(parent_list = parent_list,
       adjacency_matrix = adjacency_matrix)
}

utils::globalVariables(".")