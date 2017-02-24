#' @name plot_dbn_unexported
#' @title Unexported Utilities for \code{plot.dbn}
#' 
#' @description Utilities to support the \code{plot.dbn} method.  These 
#' are not intended to be exposed to the user and may not receive the 
#' same rigor in argument checking or functional requirements.
#' 
#' @param custom_node The \code{custom_node} argument from the \code{plot.dbn}
#'   call.
#' @param custom_edge The \code{custom_edge} argument from the \code{plot.dbn}
#'   call.
#' @param custom_edge_name The names of the \code{custom_edge} argument 
#'   from the \code{plot.dbn} call.
#' @param expand \code{logical(1)}, passed from \code{plot.dbn}.
#' @param network A network object, passed from \code{plot.dbn}.
#' @param Node The \code{Node} object generated in \code{plot.dbn}. This 
#'   holds the expanded node structure.
#' @param node \code{character(1)} naming a node in the network.  This should
#'   be one of the values in \code{network$node_attr$node_name}.
#' @param node_attr, the \code{network$node_attr} object.
#' @param node_name Character vector of node names.
#' @param parent \code{character(1)} giving a parent of \code{node}.
#' @param shared_parent The \code{tbl_df} object
#'   returned by \code{get_shared_parent}.
#' 
#' @details 
#' \code{plot_dbn_expand_network} Expands the network to relate each node with
#'   all of its parents.  One relationship is displayed in each line of the
#'   data frame returned.
#' 
#' \code{plot_dbn_expand_dynamic_network} is a subroutine of 
#'   \code{plot_dbn_expand_network}, which performs the same expansion, and
#'   also expands dynamic nodes.
#' 
#' \code{plot_dbn_get_node_string} returns a character vector of all of the 
#'   nodes in the network plot.
#' 
#' \code{plot_dbn_get_star_node} makes adjustments to the \code{custom_node}
#'   argument to include all of the dynamic nodes when the star
#'   notation is employed.
#' 
#' \code{plot_dbn_plot_dbn_get_subgraph} returns a character vector with 
#'   the subgraph code for the plot.  A Subgraph is made for each dynamic 
#'   node, allowing the changes in time to be displayed in a column.
#' 
#' \code{plot_dbn_sanitize_custom_edge} formats edge definitions to 
#'   match the GraphViz requirements.  This is especially important when
#'   using dynamic nodes and the star notation.
#' 
#' \code{plot_dbn_write_edge} Creates the edge definitions for the plot
#' 
#' \code{plot_dbn_get_shared_parent} Identifies nodes with a shared parent.
#'   Honestly, I forget why this was important.  I might remember some day.
#' 
#' \code{plot_dbn_get_edge_text} generates the actual edge text to pass
#'   to GraphViz.
#' 
#' @author Benjamin Nutter

plot_dbn_expand_network <- function(network, expand = FALSE)
{
  checkmate::assert_class(x = network,
                          classes = "dbn")
  
  checkmate::assert_logical(x = expand,
                            len = 1)
  
  if (!expand)
  {
    mapply(
      function(node, parent, ...) expand.grid(node = node, 
                                              parent = if (!length(parent)) NA else parent, 
                                              ...),
      network[["node_attr"]][["node_name"]],
      network[["node_attr"]][["parent"]],
      MoreArgs = list(stringsAsFactors = FALSE),
      SIMPLIFY = FALSE
    ) %>%
      do.call("rbind", .)
  }
  else
  {
    lapply(network[["node_attr"]][["node_name"]], 
           plot_dbn_expand_dynamic_network,
           network[["node_attr"]]) %>%
      dplyr::bind_rows()
  }
}

#' @rdname plot_dbn_unexported
 
plot_dbn_expand_dynamic_network <- function(node, node_attr)
{
  checkmate::assert_character(x = node,
                              len = 1)
  checkmate::assert_class(x = node_attr,
                          classes = "data.frame")
  
  node_is_dynamic <- node_attr[node_attr[["node_name"]] == node, "is_dynamic"]
  
  parent <- unlist(node_attr[node_attr[["node_name"]] == node, "parent"])
  if (!length(parent)) parent <- NA
  
  if (!node_is_dynamic)
  {
    return(
      expand.grid(node = node,
                  parent = parent,
                  stringsAsFactors = FALSE)
    )
  }
  
  parent_raw <- unlist(node_attr[node_attr[["node_name"]] == node, "parent_raw"])
  if (!length(parent_raw)) parent_raw <- NA
  
  parent_raw_depend <- grep_extract(x = parent_raw, 
                                    pattern = "(?=\\[).*?(?<=\\])",
                                    perl = TRUE)
  parent_raw_depend <- gsub(pattern = "[[:punct:],[:alpha:]]",
                            replacement = "",
                            x = parent_raw_depend)
  parent_raw_depend <- as.numeric(parent_raw_depend)
  parent_raw_depend <- stats::setNames(parent_raw_depend,
                                       parent)
  
  time <- node_attr[node_attr[["node_name"]] == node, "max_t"]
  
  parent_is_dynamic <- 
    grepl(pattern = "(?=\\[).*?(?<=\\])",
          x = parent_raw,
          perl = TRUE)
  
  expanded_node <- 
    mapply(
      expand_node,
      time = time,
      dynamic = node_is_dynamic,
      MoreArgs = list(node = node),
      SIMPLIFY = TRUE
    )
  
  expanded_parent <- 
    mapply(
      expand_node,
      node = parent,
      time = time,
      dynamic = parent_is_dynamic,
      SIMPLIFY = FALSE
    )
  
  expanded_node_and_parent <- 
    lapply(expanded_parent,
           function(p, n, rn) expand.grid(node = n,
                                          parent = p,
                                          stringsAsFactors = FALSE),
           n = expanded_node) 
  expanded_node_and_parent <- dplyr::bind_rows(expanded_node_and_parent)
  expanded_node_and_parent <- 
    tidyr::separate(data = expanded_node_and_parent,
                    col = node,
                    into = c("node_root", "node_time"),
                    remove = FALSE,
                    fill = "right") 
  expanded_node_and_parent <- 
    tidyr::separate(data = expanded_node_and_parent,
                    col = parent,
                    into = c("parent_root", "parent_time"),
                    remove = FALSE,
                    fill = "right") 
  expanded_node_and_parent <- 
    dplyr::mutate(
      .data = expanded_node_and_parent,
      parent_raw_depend = parent_raw_depend[expanded_node_and_parent$parent_root],
      node_diff = as.numeric(node_time) - as.numeric(parent_time),
      node_time = ifelse(test = is.na(parent_raw_depend),
                         yes = node_time,
                         no = as.numeric(node_time) - parent_raw_depend),
      node_diff = ifelse(test = is.na(node_diff),
                         yes = -1,
                         no = node_diff)
    )
  
  #* This probably seems strange.  To get the desired parent nodes, we 
  #* need to find those rows that meet the following conditions:
  #* IF `parent_raw_depend` IS NOT NA
  #*    `node_diff == parent_raw_depend`, which means the difference between
  #*    time points is equal to the difference defined in the network.
  #* IF `parent_raw_depend` IS NA
  #*    `node_time == 0`, essentially, the parent is not dynamic, and for
  #*    simplicity of display, will only be connected to the initial timepoint
  expanded_node_and_parent <- 
    dplyr::filter(.data = expanded_node_and_parent,
                  ifelse(test = is.na(parent_raw_depend),
                         yes = node_time == 0,
                         no = node_diff == parent_raw_depend))
    dplyr::select(.data = expanded_node_and_parent,
                  node, parent)
}

#' @rdname plot_dbn_unexported

plot_dbn_get_node_string <- function(node_name, custom_node, node_attr)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = node_name,
                              add = coll)
  
  checkmate::assert_character(x = custom_node,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  node_name <- unique(node_name)
  
  node_type <- plot_dbn_get_node_type(node_name, node_attr)
  
  if (length(custom_node))
  {
    custom_node <- plot_dbn_get_star_node(node_name = node_name, 
                                          custom_node = custom_node)
    
    index <- match(x = names(custom_node), 
                   table = node_name) 
    
    custom_node <- custom_node[!is.na(index)]
    index <- index[!is.na(index)]
    
    node_name[index] <- 
      sprintf("'%s' [%s; %s]",
              node_name[index],
              vapply(node_type[index],
                     plot_dbn_get_node_default,
                     character(1)),
              custom_node)
    node_name[-index] <- 
      sprintf("'%s' [%s]",
              node_name[-index],
              vapply(node_type[-index],
                     plot_dbn_get_node_default,
                     character(1)))
  }
  else
  {
    node_name <- sprintf("'%s' [%s]",
                        node_name,
                        vapply(node_type,
                               plot_dbn_get_node_default,
                               character(1)))
  }
  
  node_name
}

#' @rdname plot_dbn_unexported

plot_dbn_get_node_type <- function(node_name, node_attr)
{
  suffix <- grep_extract(node_name, 
                         pattern = "_\\d{1,10}$")
  
  root_node <- 
    mapply(sub,
           pattern = paste0(suffix, "$"), 
           x = node_name,
           MoreArgs = list(replacement = ""),
           SIMPLIFY = FALSE)
  
  dynamic <- node_attr[["node_name"]][node_attr[["is_dynamic"]]]
  decision <- node_attr[["node_name"]][node_attr[["is_decision"]]]
  utility <- node_attr[["node_name"]][node_attr[["is_utility"]]]
  deterministic <- node_attr[["node_name"]][node_attr[["is_deterministic"]]]
  
  ifelse(
    test = root_node %in% dynamic,
    yes = "dynamic",
    no = ifelse(
      test = root_node %in% decision,
      yes = "decision",
      no = ifelse(
        test = root_node %in% utility,
        yes = "utility",
        no = ifelse(
          test = root_node %in% deterministic,
          yes = "deterministic",
          no = "generic")
        )
      )
    )

}

#' @rdname plot_dbn_unexported

plot_dbn_get_node_default <- function(node_type)
{
  checkmate::assert_character(x = node_type,
                              len = 1)
  def <- getOption("dbn_plot_node_default")[[node_type]]
  paste0(
    sprintf("%s = '%s'",
            names(def),
            def),
    collapse = "; "
  )
}

#' @rdname plot_dbn_unexported

plot_dbn_get_star_node <- function(node_name, custom_node)
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

#' @rdname plot_dbn_unexported

plot_dbn_get_subgraph <- function(network)
{
  subgraph_node <- 
    mapply(
      function(node, time, dynamic)
      {
        if (dynamic)
        {
          sprintf("'%s_%s'",
                  node,
                  0:time)
        }
      },
      node = network[["node_attr"]][["node_name"]],
      time = network[["node_attr"]][["max_t"]],
      dynamic = network[["node_attr"]][["is_dynamic"]],
      SIMPLIFY = FALSE
    ) 
  
  subgraph_node <- 
    subgraph_node[!vapply(X = subgraph_node,
                          FUN = is.null,
                          FUN.VALUE = logical(1))]
  
  subgraph_edge <- 
    lapply(X = subgraph_node,
           function(x)
           {
             sprintf("%s -> %s",
                     utils::head(x, 
                                 n = length(x) - 1),
                     x[-1])
           }) %>%
    vapply(FUN = paste0,
           collapse = "\n    ",
           FUN.VALUE = character(1))
  
  
  vapply(X = subgraph_node,
         FUN = paste0,
         collapse = "\n    ",
         FUN.VALUE = character(1)) %>%
    sprintf("subgraph %s_node\n  {\n    %s\n    edge [style = 'invis']\n    %s\n  }",
            names(.),
            .,
            subgraph_edge)
}

#' @rdname plot_dbn_unexported

plot_dbn_sanitize_custom_edge <- function(custom_edge, custom_edge_name, Node)
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
  # Sent through `plot_dbn_get_star_node` to expand dynamic nodes
  
  edge_from <- sub(pattern = " .+$", 
                   replacement = "",
                   x = names(custom_edge)) %>%
    stats::setNames(nm = .) %>%
    plot_dbn_get_star_node(node_name = Node[["node"]], 
                           custom_node = .)
  
  # The node to which the edge is drawn.
  # Set through `plot_dbn_get_star_node` to expand dynamic nodes
  edge_to <- sub(pattern = "^.+ ", 
                 replacement = "",
                 x = names(custom_edge)) %>%
    stats::setNames(nm = .) %>%
    plot_dbn_get_star_node(node_name = Node[["node"]], 
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

#' @rdname plot_dbn_unexported

plot_dbn_write_edge <- function(Node, custom_edge = custom_edge)
{
  checkmate::assert_class(x = Node,
                          classes = "data.frame")
  
  if (length(custom_edge))
  {
    custom_edge <- 
      mapply(plot_dbn_sanitize_custom_edge,
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
           node = Node[["node"]],
           parent = Node[["parent"]],
           MoreArgs = list(stringsAsFactors = FALSE),
           SIMPLIFY = FALSE) %>%
    dplyr::bind_rows()
  
  shared_parent <-
    mapply(FUN = plot_dbn_get_shared_parent,
           node = node_map[["node"]],
           parent = node_map[["parent"]],
           MoreArgs = list(Node = Node),
           SIMPLIFY = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::filter(!is.na(parent))
  
  mapply(FUN = plot_dbn_get_edge_text,
         parent = shared_parent[["parent"]],
         node = shared_parent[["node"]],
         MoreArgs = list(shared_parent = shared_parent,
                         custom_edge = custom_edge),
         SIMPLIFY = FALSE)
}

#' @rdname plot_dbn_unexported

plot_dbn_get_shared_parent <- function(node, parent, Node)
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
      Node[["parent"]][Node[["node"]] == node] %>%
        unlist(),
      Node[["parent"]][Node[["node"]] == parent] %>%
        unlist()
    )
  
  tibble::data_frame(parent = parent,
                     node = node,
                     shared_parent_set = list(shared))
}

#' @rdname plot_dbn_unexported

plot_dbn_get_edge_text <- function(parent, node, shared_parent, custom_edge)
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
  sprintf("'%s' -> '%s' %s",
          parent,
          node,
          if (edge_def =="") edge_def 
          else sprintf("[%s]", edge_def))
}

utils::globalVariables(
  c(".",       "node_diff",    "node_time", 
    "parent",  "parent_time",  "shared_parent_set")
)