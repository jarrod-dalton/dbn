.onLoad <- function(libname,pkgname)
{
  dbn_restore_plot_default()
}

.onUnload <- function(libPath)
{
  options(dbn_plot_node_default=NULL)
}