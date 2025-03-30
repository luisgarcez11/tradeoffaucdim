
model = "SL.glm"
model2 = "SL.svm"

library(tradeoffaucdim,
        lib.loc = "C:/Users/luis_/OneDrive - Universidade de Lisboa/Luís Garcez/00_packages/")


hey = tradeoffaucdim::wrapper_aucdim(data = tradeoffaucdim::bananaquality,
                               outcome = "Quality",
                               dep_vars = setdiff(names(bananaquality), "Quality"),
                               n_samples = 100,
                               n_maximum_dim = 5,
                               model = "SL.glm",
                               model2 = "SL.svm")
save(hey, file = "hey.Rdata")
