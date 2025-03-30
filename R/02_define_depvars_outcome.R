

#' Define dep vars
#'
#' Create a column with dependent variables
#'
#' @param obj object returned by bootstrap_data
#' @param p_in entry p-value used to determine variable order
#' @param p_out removal p-value used to determine variable order
#'
#' @returns list
#' @export
#'
#' @examples
#' define_indepvars(obj)
define_indepvars <- function(obj,
                             p_in = 0.50,
                             p_out = 0.60){

  #run stepwise
  obj$stepwise_process <- fuzzySim::stepwise(data = obj$original_data,
                     sp.col = which(obj$outcome == names(obj$original_data)),
                     var.cols = which(names(obj$original_data)
                                      %in% obj$indep_vars),
                     p.in = p_in,
                     p.out = p_out,
                     direction = "forward",
                     trace = 0
                     )

  #set ordered dep vars
  obj$ordered_indep_vars <- setdiff(names(obj$stepwise_process$coefficients),
                                    "(Intercept)")
  obj$ordered_indep_vars <- obj$ordered_indep_vars[
    1:min(obj$n_maximum_dim, length(obj$ordered_indep_vars))]

  #add vars to bootstrap data
  obj$bootstrap_data <- obj$bootstrap_data %>%
    filter(n_indeps <= obj$n_maximum_dim) %>%
    mutate(indep_vars = purrr::map(.x = n_indeps,
                                     .f = ~obj$ordered_indep_vars[1:.x]))

  return(obj)


}
