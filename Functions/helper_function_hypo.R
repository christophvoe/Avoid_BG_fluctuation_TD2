cv_clas_hypo <- function(dataset,k,classifier,seed=123, thres= 0.5) {
  
  stopifnot(is.data.frame(dataset))    # dataset must be data frame
  stopifnot(is.integer(as.integer(k))) # k must be convertible to int
  stopifnot(is.integer(as.integer(seed))) 
  stopifnot(is.integer(as.integer(thres))) 
  
  set.seed(seed)
  
  # first, add a selection column to the dataset
  select_vec <- rep(1:k, length.out = nrow(dataset))
  data_split <- dataset %>% mutate(folds = sample(select_vec))
  
  # initialise an output vector of k mse values, which we 
  # will fill by using a _for loop_ going over each fold
  
  sens <- rep(0,k)
  spec <- rep(0,k)
  
  # start the for loop
  for (i in 1:k) {
    # split the data in train and validation set
    data_train <- data_split %>% filter(folds != i)
    data_valid <- data_split %>% filter(folds == i)
    
    if(classifier == "xgb"){
      # calculate the model on this data
      train_x <- model.matrix(Night_class_hypo_out ~ .,
                              data = data_train,)[,-15]
      train_y <- as.numeric(data_train$Night_class_hypo_out)
      xgboost_train <- xgboost(data = train_x,
                               label = train_y, 
                               max.depth = 10,
                               eta = 1,
                               nthread = 4,
                               nrounds = 4,
                               objective = "binary:logistic",
                               verbose = 2)
      
      xgb_test <- predict(xgboost_train, newdata = model.matrix(Night_class_hypo_out ~ .,
                                                                data = data_valid,)[,-15]) %>%
        factor(x = ifelse(. < as.numeric(thres), 1, 2), levels = c(1,2), labels = c("No_Hyper", "Hyper"))
      
      conf <- table(true = data_valid$Night_class_hypo_out,predicted = xgb_test)
    }
    
    if(classifier == "lr"){
      model <- glm(Night_class_hypo_out ~ ., family=binomial, data= data_train)
      pred_prob <- predict(model, type = "response", newdata = data_valid)
      pred_lr <- ifelse(pred_prob < as.numeric(thres), 0,1)
      
      conf <- table(true = data_valid$Night_class_hypo_out, predicted = pred_lr)
    }
    
    
    sens[i] <- as.numeric(confusion(conf)[1])
    spec[i] <- as.numeric(confusion(conf)[2])
  }
  
  
  return(list(sensitivity=mean(sens), specificity= mean(spec)))
}