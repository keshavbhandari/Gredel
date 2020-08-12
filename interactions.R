library(xgboost)
library(data.table)
library(glmnet)

setwd("C://Users//kbhandari//OneDrive - Epsilon//Desktop//server")

train <- fread('train.csv', data.table = F)
test <- fread('test.csv', data.table = F)

train$y <- as.factor(train$y)
levels(train$y)[levels(train$y) == "no"] <- 0
levels(train$y)[levels(train$y) == "yes"] <- 1
train$y <- as.integer(as.character(train$y))

library(dummies)
train <- dummy.data.frame(train, sep="_")

target <- "y"

set.seed(123)
sample <- sample.int(n = nrow(train), size = floor(.75*nrow(train)), replace = F)
validation  <- train[-sample,]
train <- train[sample,]
rm(sample)
invisible(gc())

Y_train <- train[,target]
Y_valid <- validation[,target]

dtrain  <- xgb.DMatrix(as.matrix(train[,-which(names(train) %in% c(target))]), label = Y_train)
dvalid  <- xgb.DMatrix(as.matrix(validation[,-which(names(validation) %in% c(target))]), label = Y_valid)
watchlist <- list(train = dtrain, valid = dvalid)

set.seed(123)
best_param <- list(objective = "binary:logistic",
                   eval_metric = "auc",
                   booster="gbtree",
                   max_depth = 3,
                   eta = 0.034871,
                   gamma = 0.09515809, 
                   subsample = 0.7703455,
                   colsample_bytree = 0.6855204, 
                   min_child_weight = 2,
                   max_delta_step = 7)
xgb_model <- xgb.train(params=best_param, dtrain, nrounds = 1000, watchlist, print_every_n = 5, early_stopping_rounds = 10, nthread=6)

imp <- xgb.importance(colnames(train), model=xgb_model)
train <- rbind(train, validation)
train <- train[order(as.numeric(rownames(train))),,drop=FALSE]
rm(validation)

#Internal Function To Generate If-Else Codes For Each Leaf
gen_ifelse <- function(tmp_df, leaf_id){
  var_sign_logic = c()
  id_to_find = leaf_id
  for (j in nrow(tmp_df):1) {
    if(tmp_df[j,4]!="Leaf"){
      if(tmp_df[j,6]==id_to_find){
        var_sign_logic <- c(var_sign_logic, paste0("train$`",tmp_df[j,4],"` < ",tmp_df[j,5],sep=""))
        id_to_find <- tmp_df[j,3]
      }else{
        if(tmp_df[j,7]==id_to_find){
          var_sign_logic <- c(var_sign_logic, paste0("train$`",tmp_df[j,4],"` >= ",tmp_df[j,5],sep=""))
          id_to_find <- tmp_df[j,3]
        }else{
          if(tmp_df[j,8]==id_to_find){
            var_sign_logic <- c(var_sign_logic, paste0("is.na(train$`",tmp_df[j,4],"`)",sep=""))
            id_to_find <- tmp_df[j,3]
          }
        }
      }
    }
  }
  statement <- paste("ifelse(",paste(var_sign_logic, collapse = ' & '),",1,0)",sep = "")
  return(statement)
}

#Extract Leaf Info - ID, If-Else, Gain, Cover
get_leaf_info <- function(xgb_model){
  dump <- data.frame(xgb.model.dt.tree(model = xgb_model))
  df_leaf <- dump[dump$Feature=="Leaf",]
  leaf_code <- list()
  for (i in 1:NROW(df_leaf)) {
    get_leaf_id <- df_leaf[i,3]
    tmp_df <- dump[dump$Tree==df_leaf[i,1],]
    tmp <- list(id=get_leaf_id,code=gen_ifelse(tmp_df, get_leaf_id),gain=df_leaf[i,9],cover=df_leaf[i,10])
    leaf_code[[get_leaf_id]] <- tmp
  }
  return(leaf_code)
}



get_imp_nodes <- function(leaf_info, train_df, response, print_every_n=100, minimum_iv=0.1){
  train <- train_df
  target <- response
  get_leaf_code <- leaf_info

  #Information Value Calculation
  if (!require("Information")) install.packages("Information")
  
  print_every <- print_every_n
  min_iv <- minimum_iv
  
  signif_coeffs <- data.frame(ID= character(), IV=numeric(), stringsAsFactors = F)
  for (i in 1:length(get_leaf_code)) {
    df <- data.frame(x_var=eval(parse(text=get_leaf_code[[i]][["code"]])), resp=train[,target])
    IV <- create_infotables(data=df, valid=NULL, y="resp", parallel=FALSE)
    signif_coeffs[nrow(signif_coeffs)+1, ] <- c(get_leaf_code[[i]][["id"]], IV$Summary[,2])
    
    if(i%%print_every==0){
      cat("\r","Processing IV on",i,"variables out of",length(get_leaf_code))
    }else{
      if(i%%length(get_leaf_code)==0){
        cat("\r","Completed",i,"out of",length(get_leaf_code),"variables",rep(" ",5))
      }
    }
  }
  signif_coeffs$IV <- as.numeric(signif_coeffs$IV)
  signif_coeffs <- signif_coeffs[signif_coeffs$IV>min_iv,]
  
  #Lasso
  cat("\n","Training Lasso","\n")
  start.time <- Sys.time()
  
  if (!require("doParallel")) install.packages("doParallel") # load the parallel backend
  cl <- makeCluster(4) # use 4 workers
  registerDoParallel(cl) # register the parallel backend
  
  batch_size <- nrow(signif_coeffs)
  enet_coeffs <- NULL
  for (i in 1:batch_size) {
    if(((i-1)*batch_size)>=nrow(signif_coeffs)){
      break
    }
    df <- as.data.frame(train[target])
    df_y=as.factor(df[,1])
    for (j in as.integer(batch_size*(i-1)+1):as.integer(batch_size*i)) {
      if(j>nrow(signif_coeffs)){
        break
      }
      df[signif_coeffs[j,1]] <- eval(parse(text=get_leaf_code[[signif_coeffs[j,1]]][["code"]]))
    }
    #Elastic Net
    x <- as.matrix(df[,2:NCOL(df)])
    df <- NULL
    glmmod <- cv.glmnet(x, y=df_y, alpha=1, family="binomial", type.measure = "auc", parallel = TRUE, standardize=TRUE, nfolds = 3, intercept=FALSE)
    coeffs <- data.frame(coef.name = dimnames(coef(glmmod))[[1]], coef.value = matrix(coef(glmmod)))
    coeffs <- coeffs[coeffs$coef.value!=0,]
    enet_coeffs <- rbind(enet_coeffs, coeffs)
    enet_coeffs$coef.name <- as.character(enet_coeffs$coef.name)
  }
  enet_coeffs <- enet_coeffs[order(abs(enet_coeffs$coef.value),decreasing = T),]
  
  stopCluster(cl) # good practice
  
  end.time <- Sys.time()
  time.taken <- end.time - start.time
  cat("\n",time.taken)
  
  return(enet_coeffs)
}

leaf_val <- get_leaf_info(xgb_model)
enet_coeffs <- get_imp_nodes(leaf_info = leaf_val, train_df = train, response = "y")

#Get Final Interactions in Dataframe
get_interactions <- function(df_name, target, enet_coeffs, leaf_info, max_interactions=20, keep_vars=TRUE){
  df_name_str <- deparse(substitute(df_name))
  get_leaf_code <- leaf_info
  if(keep_vars==TRUE){
    for (i in 1:max_interactions) {
      df_name[enet_coeffs[i,1]] <- eval(parse(text=gsub("train",df_name_str,get_leaf_code[[enet_coeffs[i,1]]][["code"]])))
    }
    return(df_name)
  }else{
    df_name <- df_name[,target,drop=FALSE]
    for (i in 1:max_interactions) {
      df_name[enet_coeffs[i,1]] <- eval(parse(text=gsub("train",df_name_str,get_leaf_code[[enet_coeffs[i,1]]][["code"]])))
    }
    return(df_name)
  }
}

new_df <- get_interactions(train,"y",enet_coeffs,leaf_val,50,keep_vars = F)


#---------------------------------------------------------------------------------------

library(car)

new_df <- get_interactions(train,"y",enet_coeffs,leaf_val,70,keep_vars = F)


library(car)
log_model <- glm(y ~ `201-10` + `253-10` + `332-8` + `132-7` + `435-11` + 
                        `267-9` + `75-9` + `293-9` + 
                        `530-8` + `328-11` + `182-12` + `214-11` + 
                        `180-10` + `272-13` + 
                        `261-9` + `245-8` + `497-8` + `396-13` + `195-12` + 
                        `330-11` + `233-10` + `216-7` + `415-11` + 
                        `460-8` + `382-9` + `397-8` + `265-14` + `432-13` + `406-7` + 
                        `395-8` + `378-8` + `515-8` + `328-10` + 
                        `532-8` + `327-9` + `512-14` + `390-5` + `171-11` + `380-7` + 
                        `236-10` + `286-7` + `503-9` + `385-14` + 
                        `86-9` + `496-7` + `253-13`, 
                 data=new_df, family=binomial())
summary(log_model)
vif(log_model)

log_coef <- as.data.frame(coef(summary(log_model)))
log_coef <- cbind(Variables = rownames(log_coef), log_coef)
rownames(log_coef) <- 1:nrow(log_coef)
statement <- c(NA)
for (i in 2:NROW(log_coef)) {
  statement <- c(statement,leaf_val[[log_coef[i,1]]][["code"]])
}
log_coef$Statement <- statement


test$y <- as.factor(test$y)
levels(test$y)[levels(test$y) == "no"] <- 0
levels(test$y)[levels(test$y) == "yes"] <- 1
test$y <- as.integer(as.character(test$y))

library(dummies)
test <- dummy.data.frame(test, sep="_")

test <- get_interactions(test,"y",enet_coeffs,leaf_val,70,keep_vars = F)

length(statement[grep("duration", statement)])

prediction <- predict(log_model,newdata = test,type="response")
library(pROC)
test$y <- as.factor(test$y)
rocCurve   <- roc(response = test$y, predictor = prediction, levels = rev(levels(test$y)))

#Metrics - Fit Statistics
predclass <-ifelse(prediction>coords(rocCurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = test$y)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)
Gini <-2*auc(rocCurve)-1
AUCmetric <- data.frame(c(coords(rocCurve,"best"),AUC=auc(rocCurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(rocCurve)
