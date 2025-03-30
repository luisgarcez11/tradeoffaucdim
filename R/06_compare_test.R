#' Comapare test
#'
#' @param obj object returned by plot_curve
#'
#' @returns list
#' @export
#'
#' @examples
#' compare_test(obj)
compare_test <- function(obj){

  #test results
  if(length(obj$models)== 2){
    obj$test <- list()

    #compute p_values
    obj$test$perf_ttest <- tibble::tibble(
      n_indeps = 1:obj$n_maximum_dim) %>%
      mutate(t_test = purrr::map(.x = n_indeps,
                                 .f = function(.x){
                                   t.test(x = obj$bootstrap_data$diff_auc[
                                   obj$bootstrap_data$n_indeps == .x
                                   ],
                                   mu = 0
                                 )}
                                 )) %>%
      mutate(p_value = purrr::map_dbl(.x = t_test,
                                       .f = function(.x) {
                                         .x$p.value})) %>%
      mutate(y = 10) %>%
      mutate(weight = 1)

    #add global p_values
    # Convert p-values to Z-scores using inverse normal CDF
    z_scores <- qnorm(1 - obj$test$perf_ttest$p_value)
    weights <- obj$test$perf_ttest$weight

    # Compute weighted Z-score
    weighted_z <- sum(weights * z_scores) / sqrt(sum(weights^2))

    # Compute combined p-value
    obj$test$perf_ttest <- obj$test$perf_ttest %>%
      mutate(global_p_value = 1 - pnorm(weighted_z))

    #add plot with differences
    obj$test$plot_auc_diff <- ggplot2::ggplot(
      data = obj$bootstrap_data[,c("diff_auc", "n_indeps")] ,
      mapping = aes(x = diff_auc)
    )+
      geom_histogram() +
      facet_wrap(~n_indeps, scales = "free") +
      theme_bw()+
      geom_label(data = obj$test$perf_ttest %>%
                   mutate(x = 0),
                aes(x, y,
                    label = round(p_value, 5)), size = 3,
                position = "identity") +
      geom_vline(mapping = aes(xintercept = 0), colour = "red",
                 linetype = 2) +
      xlab("AUC Differences") +
      ylab("Count")


    #compute p_values for time differences
    obj$test$time_ttest <- tibble::tibble(
      n_indeps = 1:obj$n_maximum_dim) %>%
      mutate(t_test = purrr::map(.x = n_indeps,
                                 .f = function(.x){
                                   t.test(obj$bootstrap_data$diff_time[
                                     obj$bootstrap_data$n_indeps == .x
                                   ], mu = 0)}
      )) %>%
      mutate(p_value = purrr::map_dbl(.x = t_test,
                                      .f = function(.x) {
                                        .x$p.value})) %>%
      mutate(x = max(obj$bootstrap_data$diff_time),
             y = 10) %>%
      mutate(weight = 1)

    #add global p_values
    # Convert p-values to Z-scores using inverse normal CDF
    z_scores <- qnorm(1 - obj$test$time_ttest$p_value)
    weights <- obj$test$time_ttest$weight

    # Compute weighted Z-score
    weighted_z <- sum(weights * z_scores) / sqrt(sum(weights^2))

    # Compute combined p-value
    obj$test$time_ttest <- obj$test$time_ttest %>%
      mutate(global_p_value = 1 - pnorm(weighted_z))


  return(obj)}

  if(length(obj$models)> 2){

    #prepare data for ANOVA
    data = obj$bootstrap_data %>%
      tidyr::pivot_longer(cols = c(starts_with(obj$perf_measure)),
                          names_to = c("measure", "model"),
                          names_sep = "_",
                          values_to = obj$perf_measure)

    #repeated measures ANOVA
    obj$test$perf_anova <- ez::ezANOVA(data =  data,
                dv = auc,
                wid = id,
                within = model
                )

    #prepare data for ANOVA
    data = obj$bootstrap_data %>%
      tidyr::pivot_longer(cols = c(starts_with("time")),
                          names_to = c("measure", "model"),
                          names_sep = "_",
                          values_to = "time")

    #repeated measures ANOVA for time
    obj$test$time_anova <- ez::ezANOVA(data =  data,
                                       dv = time,
                                       wid = id,
                                       within = model
    )

  }

  if(length(obj$models) == 1){
    return(obj)
  }



}
