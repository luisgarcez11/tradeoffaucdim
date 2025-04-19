
model = "SL.glm"
model2 = "SL.svm"

library(tradeoffaucdim,
        lib.loc = "C:/Users/luis_/OneDrive - Universidade de Lisboa/Luís Garcez/00_packages/")


hey = tradeoffaucdim::wrapper_aucdim(data = tradeoffaucdim::bananaquality[1:200,],
                               outcome = "Quality",
                               indep_vars = setdiff(names(bananaquality), "Quality"),
                               n_samples = 30,
                               n_maximum_dim = 5,
                               model = "SL.glm")
save(hey, file = "hey.Rdata")


bananaquality_sample <- tradeoffaucdim::bananaquality[sample(1:nrow(bananaquality), size = 50),]
usethis::use_data(bananaquality_sample)
obj1 <- bootstrap_data(data = bananaquality_sample,
                       outcome = "Quality",
                       indep_vars = setdiff(names(bananaquality), "Quality"),
                       n_samples = 30,
                       n_maximum_dim = 3)

usethis::use_data(obj1, overwrite = TRUE)
obj2 <- define_indepvars(obj1)
usethis::use_data(obj2, overwrite = TRUE)
obj3 <- apply_model(obj2)
usethis::use_data(obj3, overwrite = TRUE)
obj4 <- summary_stats(obj3)
usethis::use_data(obj4, overwrite = TRUE)
obj5 <- plot_curve(obj4)
usethis::use_data(obj5, overwrite = TRUE)
obj6 <- compare_test(obj5)
usethis::use_data(obj6, overwrite = TRUE)


