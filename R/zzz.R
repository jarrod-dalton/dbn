.onLoad <- function(libname,pkgname)
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

.onUnload <- function(libPath)
{
  options(dbn_plot_node_default=NULL)
}