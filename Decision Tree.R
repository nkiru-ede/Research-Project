library(readr)
library(ROSE)
getData <- function(){
  data <- read_csv("employeedata.csv")
  data_balanced_over <- ovun.sample(Attrition ~ ., data = data, method = "over",N = 2466)$data
  dataDT <<- data_balanced_over
  return (dataDT)
  
}


classfeatureFitness <- function(string,xx,yy) {
  
  inc <- which(string == 1)  #'inc' includes those features/variables for which 'string' contains 1
  
  if(sum(inc)==0)                          
    return(0)                          
  #print(inc)
  
  outcome <-"Attrition"
  inputs <- paste(names(xx)[inc], collapse =" + ")
  
  fRpart <- as.formula(paste(outcome, inputs, sep=" ~ "))
  
  DT <- rpart(formula = fRpart, method="class", control = rpart.control(minsplit = 3),
              data = dataDT)
  
  t_pred = predict(DT,dataDT, type='class')
  
  #Maximise accuracy
  #return( mean(dataDT$Attrition == t_pred))
  
  #Wa <- 0.8
  #Wf <- 0.2
  
  #Maximise accuracy and minimise the number of features
  return(( mean(dataDT$Attrition == t_pred) + (1 - sum(string == 1)/length(string) ) )/2)
  
  #return((Wa * ( mean(dataDT$Attrition == t_pred))  + (Wf * (1 - sum(string == 1)/length(string) ) ))/2)
}

