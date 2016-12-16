#' @name plot.dbn
#' @title Plot Dynamic Bayesian Network Objects
#' 
#' @description Prepares a plot of a Dynamic Bayesian Network Object using
#'   the \code{DiagrammeR} package.
#'   
#' @param x A \code{dbn} object.
#' @param custom_node A named \code{vector} of customizations to assign to nodes.
#' @param custom_edge A named \code{vector} of customizations to assign to edges.
#' @param custom_general A \code{vector} of customizations to assign to the 
#'   graph. 
#' @param ... Additional arguments to pass to other methods
#' 
#' @section Functional Requirements:
#' \enumerate{
#'  \item display passage of time over dynamic variables horizontally across the plot
#'  \item display dependencies vertically on the plot
#'  \item display static variables above the dynamic variables
#' }
#' 
#' @export

plot.dbn <- function(x, 
                     custom_node = character(0), 
                     custom_edge = character(0), 
                     custom_general = character(0), ...)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = custom_node,
                              names = "named",
                              add = coll)
  
  checkmate::assert_character(x = custom_edge,
                              names = "named",
                              add = coll)
  
  checkmate::assert_character(x = custom_general,
                              add = coll)
  
  Node <- expand_dynamic(Node = x[["node_attr"]])
  
  ranks <- get_rank_string(Node = x[["node_attr"]])
  
  nodes <- get_node_string(node_name = Node[["node_name"]], 
                           custom_node = custom_node)
  
  edges <- write_edge(Node = Node)
  
  graph_code <- 
    sprintf(
      paste0(
        "digraph{\n",
        "  /* GENERAL GRAPH SETTINGS */\n",
        "  %s\n\n",
        "  /* NODE DEFINITIONS */\n",
        "  %s\n\n",
        "  /* RANK DEFINITIONS */\n",
        "  %s\n\n",
        "  /* EDGE DEFINITIONS */\n",
        "  %s\n",
        "}"),
      paste0(custom_general,           # GENERAL GRAPH SETTINGS
             collapse = "\n  "),
      paste0(nodes,                    # NODE DEFINITIONS
             collapse = "\n  "),
      paste0(ranks,                    # RANK DEFINITIONS
             collapse = "\n  "),
      paste0(edges,                    # EDGE DEFINITIONS
             collapse = "\n  ")
    )

  DiagrammeR::grViz(diagram = graph_code, 
                    engine = "neato")
}




# UNEXPORTED FUNCTIONS ----------------------------------------------

#' @name plot.dbn_unexported
#' @title Unexported Utilities for \code{plot.dbn}
#' 
#' @description Documentation for utilities that support the 
#'   \code{plot.dbn} method.
#'   
#' @param Node The node attibutes data frame.  
#' 
#' @details \code{plot.dbn} will call \code{write_edge}, which in turn calls
#'   \code{get_shared_parent} and \code{get_edge_text}.  The shared 
#'   parents of two nodes are needed in order to determine when the 
#'   \code{"[constraint=false]"} tag should be added to an edge.
#'   This tag helps keep the dynamic nodes in proper alignment.
#' 
#' @author Benjamin Nutter
#' 
#' @section Functional Requirements:
#' \enumerate{
#'   \item \code{write_edge} returns one character string for each edge 
#'     in the network.
#'   \item \code{get_shared_parent} returns a vector naming the parents shared
#'     between two nodes.
#'   \item \code{get_edge_text} returns a character string defining a single 
#'     edge.  
#'   \item \code{get_edge_text} When drawing an edge between two nodes, if 
#'     the parent is a shared parent, the \code{"[constraint=false]"} tag 
#'     is added to the edge.
#' }
#' 

expand_dynamic <- function(Node)
{
  Node <-
    #* repeat dynamic rows 
    #* See http://stackoverflow.com/questions/11121385/repeat-rows-of-a-data-frame
    Node[rep(x = seq_len(nrow(Node)), 
             times = Node[["n_dynamic"]]), ] %>%
    dplyr::group_by(node_name) %>%
    dplyr::mutate(dynamic_index = seq_along(node_name)) %>%
    dplyr::ungroup()
  
  for (i in 1:nrow(Node))
  {
    index <- Node[["dynamic_index"]][i]
    parent <- 
      Node[["parent"]][i] %>%
      unlist
    
    this_index <- dplyr::filter(Node, 
                                dynamic_index == index & 
                                  is_dynamic)
    
    parent[parent %in% this_index[["node_name"]]] <- sprintf("%s_%s",
                                                             parent,
                                                             index)
    
    if (Node[["self_depend"]][i] & index > 1)
      parent <- c(parent, 
                  sprintf("%s_%s", 
                          Node[["node_name"]][i],
                          index - 1))
    
    Node[["parent"]][i] <- list(parent)
  }
  
  dplyr::filter(Node, self_depend)
  
  Node %>%
    dplyr::mutate(node_name = sprintf("%s_%s", 
                                      node_name,
                                      dynamic_index))
}

#' @rdname plot.dbn_unexported

get_rank_string <- function(Node)
{
  Node <- dplyr::filter(Node, self_depend)
  
  mapply(FUN = function(node, n) paste0(node, "_", 1:n),
         node = Node[["node_name"]],
         n = Node[["n_dynamic"]],
         SIMPLIFY = FALSE) %>%
    lapply(FUN = paste0,
           collapse = "; ") %>%
    vapply(FUN = function(x) sprintf("{rank=same; %s}", x),
           FUN.VALUE = character(1))
}

#' @rdname plot.dbn_unexported
#' @param node_name the \code{Node$node_name} vector.
#' @param custom_node The \code{custom_node} argument from \code{plot.dbn}

get_node_string <- function(node_name, custom_node)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = node_name,
                              add = coll)
  
  checkmate::assert_character(x = custom_node,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (length(custom_node))
  {
    custom_node <- get_star_node(node_name = node_name, 
                                 custom_node = custom_node)
    
    index <- match(x = names(custom_node), 
                   table = node_name) 
    
    node_name[index] <- 
      sprintf("%s [%s]",
              node_name[index],
              custom_node)
  }
  
  node_name
}

#' @rdname plot.dbn_unexported

get_star_node <- function(node_name, custom_node)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = node_name,
                              add = coll)
  
  checkmate::assert_character(x = custom_node,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  #* Do any custom node entries have a * (as in, Y_*)
  has_star <- grepl(pattern = "[*]$", 
                    x = names(custom_node))
  
  #* If no stars, return custom_node unchanged.
  if (!any(has_star)) return(custom_node)
  
  #* Flag for identifying starred custom_node elements
  star_node <- custom_node[has_star] %>%
    stats::setNames(sub("[*]$", "", names(.)))
  
  #* Match the names of the starred elements to node names
  star_match <-
    lapply(X = names(star_node),
           FUN = function(patt, name) name[grep(patt, name)],
           node_name)
  
  #* Expand the star definitions to include all of nodes of the same base
  star_custom <-
    mapply(
      FUN = 
        function(custom, name)
        {
          rep(x = custom, 
              times = length(name)) %>%
            stats::setNames(name)
        },
      custom = star_node,
      name = star_match,
      USE.NAMES = FALSE,
      SIMPLIFY = FALSE
    ) %>%
    do.call("c", .)
  
  #* Make the complete custom nodes vector
  custom_node <- c(custom_node[!has_star], star_custom) 
  
  #* Consolidate duplicated node names
  vapply(
    X = unique(names(custom_node)),
    FUN = 
      function(x, custom_node)
      {
        paste0(custom_node[names(custom_node) %in% x], collapse = ",")
      },
    custom_node = custom_node, 
    FUN.VALUE = character(1)
  )
}

#' @rdname plot.dbn_unexported

write_edge <- function(Node)
{
  checkmate::assert_class(x = Node,
                          classes = "data.frame")
  
  node_map <- 
    mapply(FUN = expand.grid,
           node = Node[["node_name"]],
           parent = Node[["parent"]],
           MoreArgs = list(stringsAsFactors = FALSE),
           SIMPLIFY = FALSE) %>%
    dplyr::bind_rows()
  
  shared_parent <-
    mapply(FUN = get_shared_parent,
           node = node_map[["node"]],
           parent = node_map[["parent"]],
           MoreArgs = list(Node = Node),
           SIMPLIFY = FALSE) %>%
    dplyr::bind_rows() 
  
  mapply(FUN = get_edge_text,
         parent = shared_parent[["parent"]],
         node = shared_parent[["node"]],
         MoreArgs = list(shared_parent = shared_parent),
         SIMPLIFY = FALSE)
}

#' @rdname plot.dbn_unexported
#' @param node \code{character(1)} giving a node in the network.
#' @param parent \code{character(1)} giving a parent of \code{node}

get_shared_parent <- function(node, parent, Node)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = node,
                              len = 1,
                              add = coll)
  
  checkmate::assert_character(x = parent,
                              len = 1,
                              add = coll)
  
  checkmate::assert_class(x = Node,
                          classes = "data.frame",
                          add = coll)
  
  checkmate::reportAssertions(coll)
  
  shared <- 
    intersect(
      Node[["parent"]][Node[["node_name"]] == node] %>%
        unlist(),
      Node[["parent"]][Node[["node_name"]] == parent] %>%
        unlist()
    )
  
  tibble::data_frame(parent = parent,
                     node = node,
                     shared_parent_set = list(shared))
}

#' @rdname plot.dbn_unexported
#' @param shared_parent The \code{tbl_df} object
#'   returned by \code{get_shared_parent}

get_edge_text <- function(parent, node, shared_parent)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = node,
                              len = 1,
                              add = coll)
  
  checkmate::assert_character(x = parent,
                              len = 1,
                              add = coll)
  
  checkmate::assert_class(x = shared_parent,
                          classes = "data.frame",
                          add = coll)
  
  checkmate::reportAssertions(coll)
  
  shared_root <- 
    sub("_\\d{1,3}$", "", node) == sub("_\\d{1,3}$", "", parent)
  
  shared_parent <- 
    shared_parent[shared_parent[["node"]] == node, ] %$%
    shared_parent_set %>%
    do.call("c", .)
  
  constraint <- 
    if (parent %in% shared_parent | shared_root)
    {
      "[constraint=false]"
    }
  else
  {
    ""
  }
  
  sprintf("%s -> %s %s",
          parent,
          node,
          constraint)
}

# GLOBAL VARIABLE DECLARATIONS --------------------------------------

utils::globalVariables(
  c("node_name", "dynamic_index", "is_dynamic", "self_depend", 
    "shared_parent_set")
)
