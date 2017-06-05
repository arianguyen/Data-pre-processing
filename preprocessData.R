library(caret)

preprocessData <- function(df,imputationMethod="medianImpute",correlationCutoff=0.9,cutoffQuantile=0.99,
                           removeNZV=T,impute=T,removeHighlyCorrelatedVars=T,removeLinearDependency=T,capOutlier=T){
  
  # select numeric variables
  num_vars <- names(sapply(df,is.numeric)[sapply(df,is.numeric)])
  num_data <- df[,num_vars]
  
  all_vars_processed <- num_data
  
  # remove near zero variance variables
  if(removeNZV){
    nzv <- nearZeroVar(num_data)
    nzv_vars <- names(num_data[,nzv])
    if(length(nzv_vars)>0)
    {
      all_vars_processed <- num_data[,-which(names(num_data)%in%nzv_vars)]
    } else {
      all_vars_processed <- num_data
    }
  }
  
  # impute missing values
  if(impute)
  {
    preProc <- preProcess(all_vars_processed,method=imputationMethod)
    all_vars_processed <- predict(preProc,all_vars_processed)
  }
  
  # remove highly correlated variables
  if(removeHighlyCorrelatedVars)
  {
    descrCor <- cor(all_vars_processed)
    highlyCorDescr <- findCorrelation(descrCor,cutoff=correlationCutoff)
    highlyCor_vars_to_rm <- names(all_vars_processed)[,highlyCorDescr]
    if(length(highlyCor_vars_to_rm)>0)
    {
      all_vars_processed <- all_vars_processed[,-highlyCorDescr]
    }
  }
    
  # remove linear dependency
  if(removeLinearDependency)
  {
    linearCombo <- findLinearCombos(all_vars_processed)
    if(length(linearCombo)>0)
    {
      all_vars_processed <- all_vars_processed[,-linearCombo$remove]
    }
  }
  
  # cap outliers
  if(capOutlier)
  {
    for (i in 1:ncol(all_vars_processed))
    {
      cappedValue <- quantile(all_vars_processed[,i],cutoffQuantile)
      all_vars_processed[,i] <- ifelse(all_vars_processed[,i]>cappedValue,cappedValue,all_vars_processed[,i])
    }
  }
  
  return(all_vars_processed)
    
}