##Setting working directory
setwd("C:/Users/rishabh.parihar/Desktop/WNS Hack")

#loading libraries
library(data.table)
library(dplyr)
library(xgboost)

##Reading data
train <- fread("train.csv")
view_log <- fread("view_log.csv")
item_data <- fread("item_data.csv")
test <- fread("test.csv")
submission <- fread("sample_submission.csv")

##Adding target variable in test data
test$is_click <- sample(0:1,size=nrow(test),replace = T)

##Appending train and test data sets
test$set <- "test"
train$set <- "train"
alldata <- bind_rows(train,test)
alldata <- alldata[order(user_id,impression_time),]
alldata <- alldata[,impression_time_1:=as.POSIXct(impression_time,format="%Y-%m-%d %H:%M:%S")]

##View Log Table
view_log <- merge(view_log,item_data,by="item_id",all.x=T,all.y=F,sort=F)
view_log <- view_log[,server_time_1:=as.POSIXct(server_time,format="%Y-%m-%d %H:%M:%S")]


setkey(alldata,"user_id")
setkey(view_log,"user_id")

##Getting user Timestamp level features

##Cross Join impression data with log data
new_df <- merge(alldata,view_log,by="user_id",allow.cartesian = TRUE)

##Keeping only past data for every impression id
new_df <- new_df[new_df$impression_time_1 > new_df$server_time_1,]

##Creating feature to capture time between two browses before every impression for every user
new_df <- new_df[order(user_id,impression_time,server_time),]
new_df <- new_df[,lag_server_time := shift(as.POSIXct(server_time_1),n=1,type = c("lag")) 
                 ,by=c("user_id","impression_time")]
new_df <- new_df[,time_between_two_browses:= as.numeric(difftime(server_time_1
                                                                 ,lag_server_time,units = "mins"))]
new_df <- new_df[,time_between_two_browses:= ifelse(is.na(time_between_two_browses),0,time_between_two_browses),]

##Getting features
user_timestamp <- new_df[,.(last_browsed_timestamp=max(server_time_1)
                          ,first_browsed_timestamp = min(server_time_1)),by=c("user_id","impression_time")]
user_timestamp<-user_timestamp[,':='(time_since_last_visit=as.numeric(difftime(impression_time,last_browsed_timestamp,units = "mins"))
               ,time_since_first_visit = as.numeric(difftime(impression_time,first_browsed_timestamp,units = "mins")))]
user_timestamp<-user_timestamp[,time_between_first_and_last_visit:=time_since_first_visit-time_since_last_visit]
user_timestamp<-user_timestamp[,last_browsed_timestamp:=NULL]
user_timestamp<-user_timestamp[,first_browsed_timestamp:=NULL]

################user_id level features##################

user_id_level<-view_log[,month_day:=substr(server_time,6,10),]
user_id_level<-user_id_level[, .(
                         Number_Of_Browses= .N
                         ,Number_Of_Items_Viewed=uniqueN(item_id)
                         ,Number_Of_Cat1_Items_Viewed=uniqueN(category_1)
                         ,Number_Of_Cat2_Items_Viewed=uniqueN(category_2)
                         ,Number_Of_Cat3_Items_Viewed=uniqueN(category_3)
                         ,Number_Of_Sessions=uniqueN(session_id)
                         ,Number_Of_Days_Browsed=uniqueN(month_day)
                         ,Different_Device_Type=uniqueN(device_type)
                         ,Mean_Price_Viewed=as.double(mean(item_price,na.rm=T))
                         ,Sd_Price_Viewed= as.double(sd(item_price,na.rm=T))
                         ,Sd_Server_Time=as.double(sd(as.numeric(server_time_1)))
                         ,Min_Price_Viewed=as.double(min(item_price,na.rm=T))
                         ,Max_Price_Viewed=as.double(max(item_price,na.rm=T))
                         ,Number_Of_Prices_Viewed=uniqueN(item_price)),by=user_id]

user_id_level <- user_id_level[,':='(
                          Min_Max_Price_Ratio=Min_Price_Viewed/Max_Price_Viewed
                          ,Prices_Viewed_Per_Item=Number_Of_Prices_Viewed/Number_Of_Items_Viewed
                          ,Items_Per_Cat_1=Number_Of_Items_Viewed/Number_Of_Cat1_Items_Viewed
                          ,Items_Per_Cat_2=Number_Of_Items_Viewed/Number_Of_Cat2_Items_Viewed
                          ,Items_Per_Cat_3=Number_Of_Items_Viewed/Number_Of_Cat3_Items_Viewed)]

user_id_level <- user_id_level[,':='(Min_Price_Viewed=NULL
                                     ,Max_Price_Viewed=NULL
                                     ,Number_Of_Cat1_Items_Viewed=NULL
                                     ,Number_Of_Cat2_Items_Viewed=NULL
                                     ,Number_Of_Cat3_Items_Viewed=NULL)]

##User Level Views of commonly viewed product category 1
cat_1_views <- view_log%>%group_by(user_id,category_1)%>%
  summarise(views=n())
cat_1_views <- cat_1_views%>%dcast(user_id~category_1)
cat_1_views[is.na(cat_1_views)]<-0
names(cat_1_views) <- c("user_id",paste("category_1_cat", names(cat_1_views)[2:19] , sep="_"))
user_id_level <- merge(user_id_level,cat_1_views,by="user_id",all.x=T,sort=F)

##User Level Views of commonly viewed product category 2
top_cat2_cats <- view_log%>%group_by(category_2)%>%summarise(count=n())%>%
       arrange(-count)%>%mutate(row_num = row_number())%>%filter(row_num<=40)%>%select(category_2)
top_cat2_cats <- top_cat2_cats$category_2
cat_2_views <- view_log%>%filter(category_2%in%top_cat2_cats)%>%
  group_by(user_id,category_2)%>%
  summarise(views=n())
cat_2_views <- cat_2_views%>%dcast(user_id~category_2)
cat_2_views[is.na(cat_2_views)]<-0
names(cat_2_views) <- c("user_id",paste("category_2_cat", names(cat_2_views)[2:41] , sep="_")) 
user_id_level <- merge(user_id_level,cat_2_views,by="user_id",all.x=T,sort=F)

##Getting user level and user timestamp level feaures in alldata
alldata<-merge(alldata,user_id_level,by=c("user_id"),all.x=T,all.y=F,sort=F)
alldata<-merge(alldata,user_timestamp,by=c("user_id","impression_time"),all.x=T,all.y=F,sort=F)
alldata<-alldata[,os_version:=ifelse(os_version %in% c("old"),1
                             ,ifelse(os_version %in% c("intermediate"),2,3))]

##Ratio Variables
alldata <- alldata[,Items_Viewed_Per_Day:=Number_Of_Items_Viewed/Number_Of_Days_Browsed]
alldata <- alldata[,Sessions_Per_Day:=Number_Of_Sessions/Number_Of_Days_Browsed]
alldata <- alldata[,Browses_Per_Day:=Number_Of_Browses/Number_Of_Days_Browsed]
alldata <- alldata[,Items_Per_Session:=Number_Of_Items_Viewed/Number_Of_Sessions]
alldata <- alldata[,Browses_Per_Session:=Number_Of_Browses/Number_Of_Sessions]

##Features to capture cyclical nature of variables like day of month,day of week,hour,minute
alldata$day_of_month <- as.numeric(substr(alldata$impression_time,9,10))
alldata$day_of_month_sin <- sin(2*pi*alldata$day_of_month/30)
alldata$day_of_month_cos <- cos(2*pi*alldata$day_of_month/30)
alldata$hour_of_day <- as.numeric(substr(alldata$impression_time,12,13))
alldata$hour_of_day_sin <- sin(2*pi*alldata$hour_of_day/23)
alldata$hour_of_day_cos <- cos(2*pi*alldata$hour_of_day/23)
alldata$minute_of_hour <- as.numeric(substr(alldata$impression_time,15,16))
alldata$minute_of_hour_sin <- sin(2*pi*alldata$minute_of_hour/59)
alldata$minute_of_hour_cos <- cos(2*pi*alldata$minute_of_hour/59)
alldata$day_of_week <- wday(as.Date(alldata$impression_time,format="%Y-%m-%d %H:%M:%S"))
alldata$weekend_flag <- ifelse(alldata$day_of_week %in% c(6,7),1,0)
alldata$day_of_week_sin <- sin(2*pi*alldata$day_of_week/7)
alldata$day_of_week_cos <- cos(2*pi*alldata$day_of_week/7)

##Order Variable 
alldata<-alldata[,month_day = paste(substr(impression_time,6,7),substr(impression_time,9,10),sep="-")]
alldata<-alldata[,':='(min_time=min(as.numeric(impression_time_1))
                       ,max_time=max(as.numeric(impression_time_1))),by=c("user_id","impression_time")]
alldata<-alldata[,order_pct=(as.numeric(impression_time_1)-min_time)/(max_time-min_time)]
alldata<-alldata[,':='(month_day=NULL,min_time=NULL,max_time=NULL)]

##Count Variables
alldata <- alldata[,app_code_users_count := length(unique(user_id)),by=app_code]
alldata <- alldata[,app_code_count := .N,by=app_code]
alldata <- alldata[,user_id_counts := .N,by=user_id]
alldata <- alldata[,apps_per_user := n_distinct(app_code),by=user_id]
alldata <- alldata[,sd_impression_time := as.double(sd(as.numeric(impression_time_1))),by=user_id]

##feature to get time from last impression
alldata<-alldata[order(user_id,impression_time),]
alldata <- alldata[,lag_impression_time:=shift(impression_time,n=1,type=c("lag")),by=user_id]
alldata <- alldata[,time_from_last_impression:=as.numeric(difftime(impression_time
                                                                   ,lag_impression_time,units = "mins")),]
alldata<-alldata[,lag_impression_time:=NULL]

##Imputing NA with -99
alldata[is.na(alldata)] <- -99

##Separating train and test sets
final_train<-alldata[set%in%c("train"),]
final_test<-alldata[set%in%c("test"),]

names(final_train)
names(final_test)

##Target encoding app code variable
app_code_event_rate <- final_train[,.(app_code_target_enc=mean(is_click)),by=app_code]
final_train <- merge(final_train,app_code_event_rate,by="app_code",all.x=T,all.y=F,sort=F)
final_train <- final_train[,app_code_target_enc := ifelse(is.na(app_code_target_enc),0,app_code_target_enc)]

final_test <- merge(final_test,app_code_event_rate,by="app_code",all.x=T,all.y=F,sort=F)
final_test <- final_test[,app_code_target_enc := ifelse(is.na(app_code_target_enc),0,app_code_target_enc)]

indep_vars <- names(final_train)[!names(final_train)%in%c(
                                                          "user_id"
                                                          ,"is_click"
                                                          ,"impression_id"
                                                          ,"impression_time_1"
                                                          ,"impression_time"
                                                          ,"set"
                                                          ,"month_day"
                                                          ,"app_code"
                                                          ,"hour_of_day"
                                                          ,"day_of_week"
                                                          ,"minute_of_hour"
                                                          ,"day_of_month"
                                                        )]

dep_var <- "is_click"

##Fitting XGB Model
mat1 <- xgb.DMatrix(data=data.matrix(final_train[,indep_vars,with=F])
                    ,label=data.matrix(final_train$is_click))

##Fitting Xgboost Model with cv
xgb.cv(params=list(eta=0.05
                   ,max_depth=6
                   ,subsample=0.8
                   ,colsample_bytree=0.8
                   ,objective="binary:logistic")
       ,metrics=list("auc")
       ,data = mat1
       ,nrounds = 1000
       ,nfold = 5
       ,min_child_weight=2
       ,missing = c(-99,Inf,-Inf)
       ,early_stopping_rounds = 20)

##Fitting XGB Model
xgb_model <- xgb.train(params = list(
  booster="gbtree"
  ,eta=0.05
  ,max_depth=6
  ,subsample=0.75
  ,colsample_bytree=0.8
)
,data=mat1, nrounds=472
,watchlist = list(train=mat1)
,min_child_weight=1
,objective="binary:logistic"
,eval_metric="auc"
,print_every_n = 100
,missing=c(-99,Inf,-Inf)
,verbose = 1)

##Plotting Feature Importances
xgb.plot.importance(importance_matrix = xgb.importance(feature_names = indep_vars,model = xgb_model))

##Predicting on train
xgb_train <- predict(xgb_model,data.matrix(final_train[,indep_vars]))
summary(xgb_train)

##Submission
xgb_test <- predict(xgb_model,data.matrix(final_test[,indep_vars]))
summary(xgb_test)

xgb_sub <- data.frame(impression_id=final_test$impression_id,is_click=xgb_test)
write.csv(xgb_sub,"xgb_sub_4.csv",row.names = F)


