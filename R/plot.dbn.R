#' @name plot.dbn
#' @title Plot Dynamic Bayesian Network Objects
#' 
#' @description Prepares a plot of a Dynamic Bayesian Network Object using
#'   the \code{DiagrammeR} package.
#'   
#' @param x A \code{dbn} object.
#' @param custom_node A named \code{vector} of customizations to assign to nodes.
#'   Named nodes that do not exist in the network are ignored.
#' @param custom_edge A named \code{vector} of customizations to assign to edges.
#'   Named edges that do not exist in the network are ignored.
#' @param custom_general A \code{vector} of customizations to assign to the 
#'   graph. 
#' @param ... Additional arguments to pass to other methods
#' @param display_code A \code{logical(1)}. Useful for debugging, when 
#'   \code{TRUE}, the GraphViz code is printed to the console.
#' 
#' @details 
#' \code{plot.dbn} produces plots using GraphViz via the \code{DiagrammeR}
#' package.  
#' 
#' @section Customizing Nodes:
#' Node attributes are customized by passing a named character vector (or 
#' named list) in the \code{custom_node} argument.  Any node definitions 
#' are applied to the node named in the argument.  For example, to change the 
#' shape of a node \code{abc} to a square, one would use \code{custom_node = 
#' c(abc = "shape=square")}.  Multiple customizations may be applied applied
#' within the same string, or in separate elements of the vector.  In other
#' words \code{c(abc = "fillcolor=green", abc = "style=filled")} is the same
#' thing as \code{c(abc = "fillcolor=green,style=filled")}.
#' 
#' A list of node attributes is available at the \code{DiagrammeR} website.\cr
#' \url{http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#node-attributes}\cr
#' \url{http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#node-shapes}\cr
#' \url{http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#colors}
#' 
#' At times, it may be desirable to apply settings to all of the iterations of
#' a dynamic variable.  For instance, if \code{X} is a variable that is 
#' observed over three time periods (\code{X_1, X_2, X_3}) and we want all of 
#' these nodes to appear in green on the plot, we may use 
#' \code{custom_node = c("X_*" = "fillcolor=green,style=filled")} in place of
#' naming all three nodes with the same value.  
#' 
#' @section Customizing Edges:
#' Edges (or arrows between nodes) are customized by passing a named character
#' vector (or named list) in the \code{custom_edge} argument where the name
#' of the element corresponds to the edge.  To pass a customization to the edge
#' between nodes \code{A} and \code{B}, the vector would look like 
#' \code{c("A -> B" = "arrowhead=none")}. White space in the edge name will be
#' ignored.
#' 
#' A list of edge attributes is available at the \code{DiagrammeR} website.\cr
#' \url{http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#edge-attributes}\cr
#' \url{http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html#arrow-shapes}
#' 
#' At times, it may be desirable to apply settings to all of the iterations of
#' a dynamic edge.  For instance, if \code{X -> Y} is an edge that is 
#' observed over three time periods (\code{X_1 -> Y_1, X_2 -> Y_2, X_3 -> Y_3}) 
#' and we want all of these edges to appear in with a box arrowhead on the plot, 
#' we may use 
#' \code{custom_node = c("X_* -> Y_*" = "arrowhead=box")} in place of
#' naming all three edges with the same value.  
#' 
#' @section Customizing General Settings:
#' Settings for nodes and edges may be set globally by passing a character
#' vector (or list) to \code{custom_general}.  For example, 
#' \code{custom_general = c("node[shape=circle]", "edge[arrowhead=box]")}
#' will force all nodes to be circles, and all edges to have a box on the 
#' arrow head.  Use the links it the previous sections to explore the 
#' available node and edge attributes.  
#' 
#' @section Using Color in Network Plots:
#' Colors may be specified as either hexidecimal values or any of the values
#' listed in \code{\link[grDevices]{colors}}. Note that 
#' \code{\link[grDevices]{rgb}} returns hexidecimal values, and is suitable 
#' for use in determining colors for use in network plots.
#' 
#' @section Functional Requirements:
#' Subjective Requirements (requirements for which formal tests will not be 
#' written)
#' \itemize{
#'  \item display passage of time over dynamic variables horizontally 
#'        across the plot
#'  \item display dependencies vertically on the plot
#'  \item display static variables above the dynamic variables
#' }
#' 
#' Objective Requirements
#' 
#' \enumerate{
#'   \item Accept a named vector or list of node customizations.
#'   \item Quietly ignore nodes named in \code{custom_node} that do 
#'     not exist in the network.
#'   \item Node customizations for dynamic nodes may use a \code{_*}
#'         appendix to apply changes to all of the nodes in the time stream.
#'   \item Accept a vector or list of graph customizations
#'   \item Accept a named vector or list of edge customizations.
#'   \item Edge dustomizations for dynamic edges may use a \code{_*}
#'         appendix to apply changes to all of the edges in the time stream.
#'   \item Quietly ignore nodes named in \code{custom_edge} that do 
#'     not exist in the network.
#'   \item White space will be ignored in the names of \code{custom_edge}
#'   \item Display GraphViz code to the console on request.
#' }
#' 
#' 
#' @export

plot.dbn <- function(x, 
                     custom_node = character(0), 
                     custom_edge = character(0), 
                     custom_general = character(0), ...,
                     display_code = FALSE)
{
# Argument Validations --------------------------------------------
  
  # The plotting method likes to deal with the customizations 
  # strings as vectors.  If lists were provided, the list is 
  # coerced to a vector after performing the check.
  # This behavior will be unobserved by the user.
  coll <- checkmate::makeAssertCollection()
  
  if (is.list(custom_node))
  {
    checkmate::assert_list(x = custom_node,
                           types = "character",
                           names = "named",
                           add = coll)
    custom_node <- unlist(custom_node)
  }
  else
  {
    checkmate::assert_character(x = custom_node,
                                names = "named",
                                add = coll)
  }
  
  if (is.list(custom_edge))
  {
    checkmate::assert_list(x = custom_edge,
                           types = "character",
                           names = "named",
                           add = coll)
    custom_edge <- unlist(custom_edge)
  }
  else
  {
    checkmate::assert_character(x = custom_edge,
                                names = "named",
                                add = coll)
  }
  
  if (is.list(custom_general))
  {
    checkmate::assert_list(x = custom_general,
                           types = "character",
                           add = coll)
    custom_general <- unlist(custom_general)
  }
  else
  {
    checkmate::assert_character(x = custom_general,
                                add = coll)
  }

  checkmate::assert_logical(x = display_code,
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
# Functional Code ---------------------------------------------------
  Node <- expand_dynamic(Node = x[["node_attr"]])
  
  ranks <- get_rank_string(Node = x[["node_attr"]])
  
  nodes <- get_node_string(node_name = Node[["node_name"]], 
                           custom_node = custom_node)

  edges <- write_edge(Node = Node,
                      custom_edge = custom_edge)
  
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
  
  if (display_code)
  {
    cat(graph_code)
  }

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
    dplyr::mutate(dynamic_index = ifelse(test = is_dynamic, 
                                         yes = seq_along(node_name),
                                         no = NA)) %>%
    dplyr::ungroup()
  
  for (i in 1:nrow(Node))
  {
    if (!is.na(Node[["dynamic_index"]][i]))
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
  }
  
  Node %>%
    dplyr::mutate(node_name = ifelse(test = is.na(dynamic_index),
                                     yes = node_name,
                                     no = sprintf("%s_%s", 
                                                  node_name,
                                                  dynamic_index)))
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
    
    custom_node <- custom_node[!is.na(index)]
    index <- index[!is.na(index)]
    
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
  if (!any(has_star)) 
  {
    return(custom_node)
  }
  
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

sanitize_custom_edge <- function(custom_edge, custom_edge_name, Node)
{
  # To preserve the correct lengths, this needs to be used on
  # one value at a time.  Ideally, this is passed through 
  # an apply function.  Adding the `len = 1` ensure that 
  # failures occur if not used in a manner that preserves the
  # correct ordering of edges.
  names(custom_edge) <- custom_edge_name
  
  checkmate::assert_character(x = custom_edge,
                              names = "named",
                              max.len = 1)
  
  # Remove white space from the name, and make sure the 
  # edge is defined with a single space on each side of the arrow
  names(custom_edge) <- 
    gsub(pattern = "\\s",
         replacement = "",
         x = names(custom_edge)) %>%
    sub(pattern = "(-[>])",
        replacement = " \\1 ",
        x = .)
  
  # The node from which the edge is drawn.  
  # Sent through `get_star_node` to expand dynamic nodes
  
  edge_from <- sub(pattern = " .+$", 
                   replacement = "",
                   x = names(custom_edge)) %>%
    stats::setNames(nm = .) %>%
    get_star_node(node_name = Node[["node_name"]], 
                  custom_node = .)
  
  # The node to which the edge is drawn.
  # Set through `get_star_node` to expand dynamic nodes
  edge_to <- sub(pattern = "^.+ ", 
                 replacement = "",
                 x = names(custom_edge)) %>%
    stats::setNames(nm = .) %>%
    get_star_node(node_name = Node[["node_name"]], 
                  custom_node = .)
  
  # Get the star-expanded edge listing
  edge_expand <- 
    sprintf("%s -> %s", 
            edge_from,
            edge_to) %>%
    stats::setNames(nm = sprintf("%s -> %s",
                                 names(edge_from),
                                 names(edge_to)))
  
  # Get the star-expanded custom edges
  custom_edge <- 
    custom_edge[edge_expand] %>%
    stats::setNames(names(edge_expand))

  # Combine elements with the same name 
  distinct_edge <- unique(names(custom_edge))
  
  lapply(distinct_edge,
         function(nm, custom_edge) paste0(custom_edge[nm],
                                          collapse = ","),
         custom_edge = custom_edge) %>%
    stats::setNames(distinct_edge)
}

#' @rdname plot.dbn_unexported

write_edge <- function(Node, custom_edge = custom_edge)
{
  checkmate::assert_class(x = Node,
                          classes = "data.frame")
  
  if (length(custom_edge))
  {
    custom_edge <- 
      mapply(sanitize_custom_edge,
             custom_edge = custom_edge,
             custom_edge_name = names(custom_edge),
             MoreArgs = list(Node = Node),
             SIMPLIFY = FALSE,
             USE.NAMES = FALSE) %>%
      unlist()
  }

  # make a data frame of all of the edges
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
         MoreArgs = list(shared_parent = shared_parent,
                         custom_edge = custom_edge),
         SIMPLIFY = FALSE)
}

#' @rdname plot.dbn_unexported
#' @param node \code{character(1)} giving a node in the network.
#' @param parent \code{character(1)} giving a parent of \code{node}

get_shared_parent <- function(node, parent, Node)
{
# Argument validations ----------------------------------------------
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

get_edge_text <- function(parent, node, shared_parent, custom_edge)
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
  
  checkmate::assert_character(x = custom_edge,
                                add = coll)
  
  checkmate::reportAssertions(coll)
  
  # The regular expression strips out the appendix.
  shared_root <- 
    sub("_\\d{1,3}$", "", node) == sub("_\\d{1,3}$", "", parent)
  
  # Create a vector of shared parents for the node.
  shared_parent <- 
    shared_parent[shared_parent[["node"]] == node, ] %$%
    shared_parent_set %>%
    do.call("c", .)
  
  edge_def <- 
    c(
      # Constraint string
      if (parent %in% shared_parent | shared_root)
        "constraint=false"
      else NULL,
      
      # Custom Edge definitions
      if (!is.na(custom_edge[sprintf("%s -> %s", parent, node)]))
        custom_edge[sprintf("%s -> %s", parent, node)]
      else
        NULL
    ) %>%
    paste(collapse = ",")
  
  # Edge code
  sprintf("%s -> %s %s",
          parent,
          node,
          if (edge_def =="") edge_def 
          else sprintf("[%s]", edge_def))
}

# GLOBAL VARIABLE DECLARATIONS --------------------------------------

utils::globalVariables(
  c("node_name", "dynamic_index", "is_dynamic", "self_depend", 
    "shared_parent_set")
)
