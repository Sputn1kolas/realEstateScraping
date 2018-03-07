Logistic regession has the ability to tell which indivudal predictors are correct, however when testing the whole model there is not a set type of hypothesis testing. What could be done is to cross validate, and check the results 
create_subset       <- function(dataset, pct) {
  # Set the seed of R's random number generator
  set.seed(111) 
  
  # splits the rows avalible into a training and a sample set
  train.index  <-  sample(row.names(dataset), pct*nrow(dataset))
  test.index   <-  setdiff(row.names(dataset), train.index) 
  
  # selects the rows selected above
  final           <-   list()
  final$train     <-   dataset[train.index,] 
  final$test      <-   dataset[test.index,]  # selects the rows from testing
  
  final
}
cross_validate_sets <- function(dataset, model, response) {
  pred = predict(model, newdata = dataset$test)
  confusionMatrix(data = pred, dataset$test[[response]])
}


BREAST_CANCER_sets <- create_subset(BREAST_CANCER, 0.6)
BC_GLM             <- glm(factor(class) ~ ., BREAST_CANCER_sets$train,  family="binomial")
pred               <- predict(BC_GLM, newdata = BREAST_CANCER_sets$test, type ='response' )

# what are these prediction values? ????
confusionMatrix(pred, BREAST_CANCER_sets$test$class)
summary(BC_GLM)
