

#' Apply Model
#'
#' Apply model and create column with fit
#'
#' @param obj object returned from 02_define_depvars_outcome
#' @param models models to be analyzed
#' @param test_partition_prop test proportion
#' @param perf_measure performance measure
#'
#' @returns list
#' @export
#'
#' @examples
#' apply_model(obj2)
apply_model <- function(obj,
                        models = c("SL.glm", "SL.svm"),
                        test_partition_prop = 0.2,
                        perf_measure = "auc"){

  splits <- indep_vars <- train_test_data <- fit <- model_fit <- newY <-
    performance <- time <- X <- Y <- newX <- . <-  NULL

  obj$models <- models
  obj$test_partition_prop <- test_partition_prop
  obj$perf_measure = perf_measure
  #check if package is installed
  # if (stringr::str_remove(model, "SL.") %in% rownames(installed.packages())) {
  #   print(paste(package_name, "is installed."))

  if(!is.null(obj$models)){

    #store train and outcome data
    obj$bootstrap_data <- obj$bootstrap_data %>%
      dplyr::mutate(train_test_data = purrr::map2(.x = splits,
                                     .y = indep_vars,
                                     .f = function(.x, .y){
                                       trainIndex <- caret::createDataPartition(
                                         y = unlist(
                                           as.data.frame(.x)[obj$outcome]),
                                         p = 1-test_partition_prop,
                                         list = FALSE,
                                         times = 1)
                                       df = as.data.frame(.x)
                                       list(
                                         X = df[trainIndex,][.y] ,
                                         newX = df[-trainIndex,][.y],
                                         Y =  df[trainIndex, obj$outcome],
                                         newY =  df[-trainIndex, obj$outcome]
                                         )}
                                     )) %>%
      tidyr::unnest_wider(train_test_data) %>%
      select(-splits)


    #iterate over models to store AUC
    for(model in obj$models){

      rename_obj <- function(name){paste0(name, "_", model )}

      obj$bootstrap_data <- obj$bootstrap_data %>%
        dplyr::mutate(fit = purrr::pmap(.[,c("X", "newX", "Y")] ,
                                       .f = function(X, newX, Y){
                                         obj = list()
                                         st_time <- proc.time()
                                         model_fit = SuperLearner::SuperLearner(
                                           Y = as.numeric(unlist(Y)),
                                           X = X,
                                           newX = newX,
                                           SL.library = model)
                                         obj$time <-  (proc.time()-st_time)
                                         obj$model_fit <- model_fit
                                         return(obj)
                                         },
                                       .progress = paste("fitting model",
                                                         model))) %>%
        dplyr::mutate(model_fit = purrr::map(.x = fit, .f = ~.x[["model_fit"]])) %>%
        dplyr::mutate(time = purrr::map_dbl(.x = fit, .f = ~.x[["time"]][[1]])) %>%
        dplyr::select(-c(fit)) %>%
        dplyr::mutate(performance = purrr::map2_dbl(.x = model_fit,
                                     .y = newY,
                                     .f = function(.x, .y) {
                                       pred_rocr = ROCR::prediction(
                                         .x$SL.predict,
                                         .y)
                                       measure = ROCR::performance(
                                         pred_rocr,
                                         measure = perf_measure,
                                         x.measure = "cutoff")@y.values[[1]]
                                       return(measure)})) %>%
        dplyr::rename_at(dplyr::vars(performance, time), rename_obj)

    }
    }

  #compute performance differences
  if(length(obj$models) == 2){
    #calculate AUC differences
    obj$bootstrap_data <- obj$bootstrap_data %>%
      dplyr::mutate(diff_performance = obj$bootstrap_data[[paste0("performance_",
                                                           obj$models[2])]] -
               obj$bootstrap_data[[paste0("performance_",
                                          obj$models[1])]])%>%
      dplyr::mutate(diff_time =  obj$bootstrap_data[[paste0("time_",
                                                     obj$models[2])]] -
               obj$bootstrap_data[[paste0("time_",
                                          obj$models[1])]])}

  #save space
  obj$bootstrap_data <- obj$bootstrap_data %>%
    dplyr::select(-c(model_fit, X, Y,
              newX, newY))%>%
    dplyr::rename_all(~stringr::str_replace_all(.,
                                         pattern = "performance",
                                         replacement = obj$perf_measure))

  return(obj)




}
