evaluate <- function(original, gold_std, model) {
  acc_misspelled <- sum(model[original != gold_std] == gold_std[original != gold_std]) / sum(original != gold_std)
  over_corr <- sum(model[original == gold_std] != gold_std[original == gold_std]) / sum(original == gold_std)
  accuracy <- sum(model == gold_std) / length(model)
  return(c(AccMiss = acc_misspelled, OverCorr = over_corr, Accuracy = accuracy))
}
