## quant_bands - function 
### vec: numeric vector 
### splits: numeric vector with you custom splits
### nquantiles: if splits are not defined, create n groups with the same amount of observations
### max_splits: if splits are defined but there are too many groups, you need to set this parameter
###             to higher values

quant_bands <- function(vec,splits=F,nquantiles=10,max_splits=150){
  if(is.numeric(vec)==F){
    warning('Insert a numeric vector')
    stop()
  }
  
  
  ## Do we have customized splits
  if(is.logical(splits) & splits!=F){
    
    if(length(splits)>max_splits){
      warning('Number of splits are too high, check it and change max_splits')
      stop()
    }
  

  } else {
    probs <- seq(0,1,1/nquantiles)
    splits <- quantile(vec,probs = probs,na.rm = T)[-c(1,length(probs))]
  }
  
  min_aux <- min(vec,na.rm = T) -0.0001 ## selecting the minimum of the vector 
  vec_out <- rep(NA,length(vec)) ## output
  
  ## input the quantiles on vec_out
  for( i in 1:length(splits)){
    vec_out[ vec > min_aux & vec <= splits[i] ] = i
    min_aux <- splits[i]
  }
  
  ## Now, where the vector is greater then 
  vec_out[ vec > min_aux ] = i+1
  
  ## Selecting output
  out <- list()
  out$cat_vet = vec_out
  out$splits = splits
  return(out)
}


quant_bands(iris$Sepal.Length)
