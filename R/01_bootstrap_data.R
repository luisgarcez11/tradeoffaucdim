
#' Bootstrap data
#'
#' Create a tibble with a number of bootstrap samples
#'
#' @param data a dataframe to be analysed
#' @param outcome a string representing the outcome variable
#' @param indep_vars a vector of strings to be considered
#' @param n_samples number of bootstrap samples
#' @param n_maximum_dim maximum number of variables to be considered
#'
#' @returns list
#' @export
#'
#' @examples
#' bootstrap_data()
bootstrap_data <- function(data =  bananaquality[sample(1:nrow(bananaquality),
                                                        replace = FALSE,
                                                        size = 200),],
                           outcome = "Quality",
                           indep_vars = setdiff(names(bananaquality), outcome),
                           n_samples = 50,
                           n_maximum_dim = 5){

  #check outcome variable
  if(!outcome %in% names(data)){
    stop("outcome variable not in data")
  }

  #check outcome variable
  if(!all(unlist(data[,outcome]) %in% c(0,1))){
    stop("outcome variable not in data")
  }

  #dummify data
  if(any(sapply(data,is.factor)|sapply(data,is.character) )){
    data <- fastDummies::dummy_cols(data,
                                    remove_most_frequent_dummy = TRUE,
                                    remove_selected_columns = TRUE)
  }

  #Bootstrap data
  b_data_final <- tibble::tibble()
  for(i in 1:n_maximum_dim){
    bdata <- rsample::bootstraps(data,
                                 times = n_samples) %>%
      dplyr::mutate(n_indeps = i)
    b_data_final <- b_data_final %>%
      dplyr::bind_rows(bdata)
  }

  #independent vars
  indep_vars <- setdiff(names(data), outcome)


  #set obj
  obj = list("bootstrap_data" = b_data_final,
             "original_data" = data,
             "outcome" = outcome,
             "indep_vars" = indep_vars,
             "n_maximum_dim" = n_maximum_dim)

  return(obj)

}
