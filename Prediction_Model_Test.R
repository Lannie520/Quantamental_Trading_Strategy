#Linear Regression, Stepwise Regression, and Ridige Regression part 
ML_model <- function(sector,trade_date){
  #First factor colume number
  start_column=12
  #In total 89 stock selections
  #Model error MSE
  model_error= data.frame(mse_linear=replicate(89,0))
  model_error[,2]=data.frame(mse_step=replicate(89,0))
  model_error[,3]=data.frame(mse_ridge=replicate(89,0))
  
  #Predict return to select stocks 
  predicted_return=list()
  #main model
  features_linear=list()
  features_ridge=list()
  features_step=list()

#for(i in 1:(length(trade_date)-19)){LR_features[[i]]=c(1:i)}
#Rolling Window
#for(i in 1:length(trade_date)-19)){print(c(i,i+15,i+16,i+19,trade_date[i+20]))}
 for(i in 1:(length(trade_date)-21)){
   ##train the model based on 4 years, 16 quarters data
   #growing window 10 years 
   if (i<=25){
     train_data=sector[(sector$tradedate <= trade_date[i+15]),]
     train_x=train_data[,c(start_column:(dim(sector)[2]-1))]
     train_y=train_data[,dim(sector)[2]]
   } else{
     train_data=sector[(sector$tradedate <= trade_date[i+15]) & sector$tradedate >= trade_date[i-25],]
     train_x=train_data[,c(start_column:(dim(sector_data)[2]-1))]
     train_y=train_data[,dim(sector_data)[2]]
   }
##Test set  
 test_data=sector[(sector$tradedate <= trade_date[i+19]) & (sector$tradedate >= trade_date[i+16]),]
 test_x=test_data[,c(start_column:(dim(sector)[2]-1))]
 test_y=test_data[,dim(sector)[2]]
 
 train=cbind(train_y,train_x)
 test=cbind(test_y,test_x)
 
 ##Trade set
 trade_data = sector[(sector$tradedate == trade_date[i+20]),]
 trade_x=trade_data[,c(start_column:(dim(sector)[2]-1))]
 trade_y=trade_data[,dim(sector)[2]]
 trade=cbind(trade_x,trade_y)
 
 row.names(trade_x)=trade_data$tic
 
 ###Linear Regression###
 linear_model=lm(y_return~., data=train)
 linear_pre_y=predict(linear_model,test_x)
 mse_linear=mean((test_y-linear_pre_y)^2,na.rm=TRUE)
 features_linear[[i]]=summary(linear_model)
 
 ###Ridge###
 x_train_ridge=model.matrix(y_return~.,train)[,-1]
 y_train_ridge=train$y_return
 
 x_test_ridge=model.matrix(y_return~.,test)[,-1]
 y_test_ridge=test$y_return
 
 #Tunning for lambda
 opt_ridge = cv.gmlnet(x_train_ridge,y_train_ridge,alpha=1)
 opt_lambda = opt_ridge$lamda.min
 
 ridge_model=glmnet(x_train_ridge,y_train_ridge,alpha=0,lambda=opt_lambda)
 ridge_pred_y=predict(ridge_model,newx= x_test_ridge)
 
 mse_ridge = mean((ridge_pred_y-y_test_ridge)^2,na.rm=TRUE)
 

 ridge_coef = coef(ridge_model)
 ridge_coef = data.frame(name=ridge_coef@Dimnames[[1]][ridge_coef@i + 1], coefficient = ridge_coef@x)
 ridge_features[[i]]=ridge_coef
 
 ###Stepwise Regression###
 step_model=stepAIC(linear_model, direction="both",trace=0)
 step_pre_y=predict(step_model,test_x)
 
 mse_step=mean((test_y-step_pre_y)^2,na.rm=TRUE)
 #Step Features
 features_step[[i]]=summary(step_model)
 
 ######Result######
 #all model trade
 #Prediction by Linear Regression
 trade_linear_y=predict(linear_model,trade_x)
 #Prediction by Ridge
 x_train_ridge=model.matrix(y_return~.,trade)[,-1]
 row.names(x_trade_ridge)=trade$tic
 trade_ridge_y=predict(ridge_model,x_trade_ridge)
 colnames(trade_ridge_y)=c('trade_ridge_y')
 
 ###Store Model ERROR###
 if (length(unique(trade_linear_y))<length(trade_linear_y)*0.2){
   mse_linear=NA
 }
 
 if(length(unique(trade_ridge_y))<length(trade_ridge_y)*0.2){
   mse_ridge=NA
 }
 if(length(unique(trade_step_y))<length(trade_step_y)*0.2){
   mse_step=NA
 }
 
 model_error[i,]=c(mse_linear,mse_ridge,mse_step)

 #Store all the predicted returns
 return_store=cbind(trade_linear_y,trade_linear_y,trade_ridge_y,trade_step_y)
 predicted_return[[i]]=return_store
 }
 results=list(model_error=model_error,
             predicted_return=predict_return,
             features_step=features_step,
             features_ridge=features_ridge,
             features_linear=features_linear
             )
 return(results)
 }
  
 
 
 

