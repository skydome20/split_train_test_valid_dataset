
# 4.split a dataset into (train, test, valid) subsets (returning formats is a 'list') ####
split.data <- function(data, seed=set.seed(123), train=0, test=0, valid=0){
  # check 
  if( as.numeric(train+valid+test) > 1 ){
    return (cat("The sum of parameters is not equal to 1!"))
  }
  
  test_size <- floor(test * nrow(data))
  #set the seed to make partition reproductible
  seed
  test_ind <- sample(seq_len(nrow(data)), size = test_size )
  d_test <- data[test_ind, ]
  d_train <-  data[-test_ind, ]    
  d_valid <- NULL
  
  # if exists the validation dataset
  if(!(train+test== 1)){
    valid_size <- floor((valid/(train+test)) * nrow(d_train))
    seed
    valid_ind <- sample(seq_len(nrow(d_train)), size = valid_size )
    d_valid <- d_train[valid_ind, ]
    d_train <- d_train[-valid_ind, ]
  }
  
  list("train"=d_train, "test"=d_test, "valid"=d_valid)
}