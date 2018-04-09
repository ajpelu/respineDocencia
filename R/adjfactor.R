#' Adjacency Factor
#'
#' Compute adjacency factor. See fig. 2 of Zamora et al.
#'
#' @param adj the value of adjacency of a natural forest to a pine plantantion
#'
#' @param v the value of adding to avoid zero results (from 0 to 1)
#'
#' Utilizamos la equation derivada de los datos de la figura 2 de Zamora et al.
#' Calculamos la adjacencia entre un parche de nf y la pine plantation. Con ello
#' obtemos la seed limitation que tendra la pine plantation en funcion de la
#' adjacencia. Cuanto mas adjacency menos seed limitation
#' Luego hacemos el inverso y estandarizamos entre 0 y 1.
#' Finalmente para que el minimo no sea 0, sumamos la cantidad de v



adjfactor <- function(adj, v){
  seed_limitation <- 0.736658946 -0.004037077 * adj
  # inverse seed limitation
  sli <- 1/seed_limitation
  # min and max of inverse seed limitation (adj=0 and adj=100)
  sl0 <- 1/(0.736658946 -0.004037077 * 0)
  sl100 <- 1/(0.736658946 -0.004037077 * 100)

  # standardize inverse seed limitation
  sli_std <- ((sli - sl0) / (sl100 - sl0)) + v
  return(sli_std)
}





