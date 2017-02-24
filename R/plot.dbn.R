#' @name plot.dbn
#' @title Plot Dynamic Bayesian Network Objects
#' 
#' @description Prepares a plot of a Dynamic Bayesian Network Object using
#'   the \code{DiagrammeR} package.
#'   
#' @param x A \code{dbn} object.
#' @param expand \code{logical(1)}. When \code{TRUE}, the plot is expanded
#'   to display dynamic nodes over time.  Otherwise, only the root node 
#'   dependencies are shown.
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
#' @seealso 
#' \code{\link{dbn_plot_deafult}}, \code{\link[DiagrammeR]{grViz}}
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
#'  \item display passage of time over dynamic variables vertically down 
#'        the plot
#'  \item display dependencies vertically on the plot
#'  \item display static variables above the dynamic variables
#' }
#' 
#' Objective Requirements
#' 
#' \enumerate{
#'   \item Cast an error when given an object that does not have class \code{dbn}.
#'   \item Provide the user with the option to collapse dynamic nodes into 
#'     a single-node representation or expand dynamic nodes across all 
#'     timepoints.
#'   \item Accept a named vector or list of node customizations.
#'   \item Quietly ignore nodes named in \code{custom_node} that do 
#'     not exist in the network.
#'   \item Node customizations for dynamic nodes may use a \code{_*}
#'         appendix to apply changes to all of the nodes in the time stream.
#'   \item Accept a vector or list of graph customizations
#'   \item Accept a named vector or list of edge customizations.
#'   \item Edge customizations for dynamic edges may use a \code{_*}
#'         appendix to apply changes to all of the edges in the time stream.
#'   \item Quietly ignore nodes named in \code{custom_edge} that do 
#'     not exist in the network.
#'   \item White space will be ignored in the names of \code{custom_edge}
#'   \item Display GraphViz code to the console on request.
#' }
#' 
#' @examples 
#' ################################
#' ### Static Network Examples ###
#' ################################
#' 
#' static <- 
#'  dbn(~ wells + 
#'        pe | wells + 
#'        d.dimer | pregnant*pe + 
#'        angio | pe + 
#'        treat | d.dimer*angio + 
#'        death | pe*treat)
#'        
#' plot(static)
#' 
#' # Changing a node attribute for a single node using a character vector 
#' plot(static,
#'      custom_node = c("d.dimer" = "color = purple"))
#'      
#' # Change node attributes for multiple nodes using a list
#' plot(static,
#'      custom_node = list(d.dimer = "color = purple",
#'                         treat = "style = filled; fillcolor = green",
#'                         pe = "shape = diamond; label = 'Pulmonary Embolism'"))
#'
#' # Change edge attributes for a single edge using a list
#' plot(static,
#'      custom_edge = list("wells -> pe" = "color = red; arrowhead = box"))
#'  
#' # Change edge attribues for multiple edges using a character vector
#' plot(static,
#'      custom_edge = list("wells -> pe" = "color = red",
#'                         "d.dimer -> treat" = "arrowhead = tee; color = purple"))
#' 
#' # Change settings for all nodes and/or edges
#' plot(static,
#'      custom_general = c("node [shape = diamond; style = filled; color = yellow]",
#'                         "edge [arrowhead = box; color = purple]"))
#'                         
#' ################################
#' ### Dynamic Network Examples ###
#' ################################ 
#' 
#' dynamic_net <-
#'   dbn(~ Age + 
#'         Sex + 
#'         Q1[t] | Q2[t-1] * Q1[t-1] * Age * Sex + 
#'         Q2[t] | Q1[t-1] * Age * Sex,
#'        max_t = 2)
#'        
#' # Compact representation
#' plot(dynamic_net)
#' 
#' # Expanded representation
#' plot(dynamic_net,
#'      expand = TRUE)
#'      
#' # Customizing Nodes in the Expanded Representation
#' plot(dynamic_net,
#'      expand = TRUE,
#'      custom_node = c("Q1_0" = "style = filled; fillcolor = purple",
#'                      "Q1_2" = "style = filled; fillcolor = green",
#'                      "Q2_1" = "style = filled; fillcolor = yellow"))
#'                      
#' # Customizing all nodes in a dynamic stream
#' plot(dynamic_net,
#'      expand = TRUE, 
#'      custom_node = c("Q1_*" = "style = filled; fillcolor = purple",
#'                      "Q2_*" = "style = filled; fillcolor = yellow"))
#'                      
#' # Customizing specific edges in a dynamic stream
#' plot(dynamic_net,
#'      expand = TRUE,
#'      custom_edge = c("Q1_0 -> Q1_1" = "arrowhead = inv; color = purple",
#'                      "Q2_0 -> Q1_1" = "arrowhead = box; color = green"))
#'                      
#' # Customizing all edges in a dynamic stream
#' plot(dynamic_net,
#'      expand = TRUE,
#'      custom_edge = c("Q2_* -> Q1_*" = "arrowhead = box; color = purple"))
#'      
#' # NOTE: General settings may be applied to all nodes, and then overridden 
#' #       for specific nodes.  The same applies to edges
#' plot(dynamic_net,
#'      expand = TRUE,
#'      custom_general = "node [shape = diamond; style = filled; fillcolor = green]",
#'      custom_node = c("Q2_1" = "shape = rect; fillcolor = purple"))
#' @export

plot.dbn <- function(x, 
                     expand = FALSE,
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
  
  checkmate::assert_logical(x = expand,
                            len = 1,
                            add = coll)
  
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
  
  Node <- plot_dbn_expand_network(network = x,
                                  expand = expand)
  
  nodes <- plot_dbn_get_node_string(node_name = Node[["node"]],
                                    custom_node = custom_node,
                                    node_attr = x[["node_attr"]])
  
  subgraph <- 
    if (expand) 
    {
      plot_dbn_get_subgraph(network = x)
    }
    else
    {
      ""
    }
  
  edges <- plot_dbn_write_edge(Node = Node,
                               custom_edge = custom_edge)
  
  graph_code <- 
    sprintf(
      paste0(
        "digraph{\n",
        "  /* GENERAL GRAPH SETTINGS */\n",
        "  %s\n\n",
        "  /* NODE DEFINITIONS */\n",
        "  %s\n\n",
        "  /* SUBGRAPH DEFINITIONS */\n",
        "  %s\n\n",
        "  /* RANK DEFINITIONS */\n",
        "  %s\n\n",
        "  /* EDGE DEFINITIONS */\n",
        "  edge [style = '']\n",
        "  %s\n",
        "}"),
      paste0(custom_general,           # GENERAL GRAPH SETTINGS
             collapse = "\n  "),
      paste0(unique(nodes),   # NODE DEFINITIONS
             collapse = "\n  "),
      paste0(subgraph,
             collapse = "\n  "),
      "", #paste0(ranks,                    # RANK DEFINITIONS
          #   collapse = "\n  "),
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

