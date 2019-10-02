library(dplyr)
library(R6)
library(bayesm)
library(skimr)
library(tidyr)
library(friendlyeval)
library(simputation)

OnlyVariablePrint <- function(table){
     len <- length(names(table))
     for(VarCount in 1:len){
         var <- length(levels(factor(table[,names(table)[VarCount]])))
         if( var == 1 ){
             print( names(table)[VarCount])
         }
     }
}

train<-read.csv('HousePrices/csv/train.csv',stringsAsFactors=F)
train$type<-'train'
test<-read.csv('HousePrices/csv/test.csv',stringsAsFactors=F)
test$type<-'test'
head(train)

union.d<-train%>%select(-SalePrice)%>%union_all(test)
skimr::skim(union.d)
dt<-sapply(union.d, class)
c.n<-names(dt[dt=='character'])
i.n<-names(dt[dt=='integer'])

ds<-union.d
for( cname in c.n[-44]) {
  #print(treat_input_as_col(cname))
  ds<-ds%>%mutate(value=1)%>%spread(key=!!treat_input_as_col(cname),value=value,sep='_',fill=0)
}
for(iname in i.n[-1]) {
  #print(treat_input_as_col(iname))
  ds<-ds%>%mutate(!!treat_input_as_col(iname) := as.vector(scale(!!treat_input_as_col(iname))))
}

skimr::skim(ds)
ds%>%dplyr::filter(is.na(BsmtFinSF2))%>%select(Id)
i.n
ds<-ds%>%impute_lm(BsmtFinSF1~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(BsmtFinSF2~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(BsmtFullBath~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(BsmtHalfBath~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(BsmtUnfSF~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(GarageArea~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(GarageCars~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(GarageYrBlt~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(LotFrontage~MSSubClass+MSSubClass+MoSold+YrSold)%>%
  impute_lm(MasVnrArea~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(TotalBsmtSF~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)%>%
  impute_lm(TotalBsmtSF~MSSubClass+LotFrontage+MSSubClass+MoSold+YrSold)
  
ud<-ds%>%select(-type,-Id,-Exterior1st_NA,-Utilities_NA,-SaleType_NA,-MSZoning_NA
                     ,-Exterior1st_NA,-Exterior2nd_NA,-KitchenQual_NA,-Functional_NA,-MiscFeature_NA)
rownames(ud)<-ds$Id
hc<-hclust(d=dist(ud),method="ward.D2")
plot(hc)
class.k<-cutree(tree=hc,k=12)
cl.d<-data.frame(Id=as.integer(names(class.k)),k=class.k)
nrow(ds)
ds.r<-ds%>%inner_join(cl.d)
nrow(ds.r)

ds.r%>%group_by(type,k)%>%summarise(n=n())%>%data.frame
table(ds.r$k)

nrow(ds)
nrow(union.d)

train.d<-ds.r%>%filter(type=='train')
train.d<-train.d%>%inner_join(train%>%select(Id,SalePrice))
skimr::skim(train.d)

test.d<-ds.r%>%filter(type=='test')
table(test.d$k)
skimr::skim(tr)
OnlyVariablePrint(train.d)
table(train.d$k)
tr<-train.d
tr$k<-as.factor(tr$k)
tr<-train.d%>%select(Id,SalePrice,k,
                LotArea               ,  
           OverallQual           ,
           OverallCond           ,
           YearBuilt             ,
           YearRemodAdd          ,
           BsmtFinSF1            ,
           BsmtFinSF2            ,
           BsmtUnfSF             ,
           X1stFlrSF             ,
           X2ndFlrSF             ,
           LowQualFinSF          ,
           BsmtFullBath          ,
           GarageArea            ,
           WoodDeckSF            ,
           EnclosedPorch         ,
           ScreenPorch           ,
           PoolArea              ,
           `MSZoning_C (all)`    ,
           MSZoning_FV           ,
           MSZoning_RL           ,
           LotConfig_CulDSac     ,
           LandSlope_Gtl         ,
           LandSlope_Mod         ,
           Neighborhood_Crawfor  ,
           Neighborhood_Edwards  ,
           Neighborhood_MeadowV  ,
           Neighborhood_Mitchel  ,
           Neighborhood_NWAmes   ,
           Neighborhood_StoneBr  ,
           Condition1_RRAe       ,
           Condition2_PosN       ,
           Condition2_RRAe       ,
           HouseStyle_2.5Fin     ,
           RoofStyle_Flat        ,
           RoofStyle_Gable       ,
           RoofStyle_Gambrel     ,
           RoofStyle_Hip         ,
           RoofStyle_Mansard     ,
           RoofMatl_ClyTile      ,
           RoofMatl_Membran      ,
           RoofMatl_Metal        ,
           Exterior1st_BrkComm   ,
           Exterior1st_BrkFace   ,
           ExterCond_Gd          ,
           Foundation_PConc      ,
           Foundation_Stone      ,
           Heating_Grav          ,
           HeatingQC_Ex          ,
           CentralAir_N          ,
           KitchenQual_Ex        ,
           Functional_Maj2       ,
           Functional_Mod        ,
           Functional_Sev        ,
           GarageQual_Fa         ,
           PoolQC_Ex             ,
           PoolQC_Fa             ,
           Fence_GdWo            ,
           SaleType_ConLD        )
tr$k<-as.factor(tr$k)
tr.lm<-lm(log(SalePrice)~.+0,data=tr%>%select(-Id))
summary(tr.lm)
setp.lm<-step(tr.lm)

summary(tr.lm)
pred<-exp(predict(setp.lm))
plot(pred,tr$SalePrice)+abline(0,1)

tr<-tr%>%mutate(Outlier=ifelse(abs(pred-SalePrice)>50000,1,0))
tr.lm<-lm(log(SalePrice)~.,data=tr%>%select(-pred))

plot(tr$pred,tr$SalePrice)+abline(0,1)
summary(tr.lm)

# 階層ベイズ
source('HousePrices/HierLinerModel.R')
hlm<-HierLinearModel$new()
train.d<-ds.r%>%filter(type=='train')
train.d<-train.d%>%inner_join(train%>%select(Id,SalePrice))
table(train.d$k)

train.d$log_SalePrice<-log(train.d$SalePrice)
#na.n<-colnames(train.d)[grepl("NA", colnames(train.d))]
x.n<-colnames(train.d)[-which (colnames(train.d) %in% c("Id", "k", "SalePrice",'log_SalePrice','type'))]

hlm$Run(burn.in=0.2,tr=train.d,y.n="log_SalePrice",ind='k',x.n=x.n,R=10000 ,set.Z = F)
pred<-hlm$Predict()
pred$R2

#0.9758115
#0.9849385
plot(hlm$out$taudraw)
plot(pred$pred$SalePrice,pred$pred$predict_l)+abline(0,1)
test.d<-ds.r%>%filter(type=='test')
test.d[is.na(test.d)]
skimr::skim(test.d)

test.d%>%filter(Id==1559)

test.d[is.na(test.d)]
pred<-hlm$Predict(test.d)
pred$test%>%select(Id,predict_l)
hist(pred$test$predict_l)
hist(pred$pred$SalePrice)

pred$test<-pred$test%>%mutate(predict_l=if_else(predict_l>=600000,predict_l/100,predict_l))
write.csv(pred$test%>%select(Id,predict_l),'HousePrices/csv/submission.csv',row.names = F)
pred$test%>%filter(predict_log_SalePrice<1)%>%select(Id)
pred$test%>%filter(predict_l>=600000)%>%.$k
pred$test%>%filter(Id==2504)

pred$test%>%filter(is.na(predict_log_SalePrice))%>%select(Id)

ga<-pred$test%>%filter(is.infinite(predict_l))
hlm$beta.n

