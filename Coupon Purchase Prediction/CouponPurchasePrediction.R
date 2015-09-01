library(dplyr)
library(lsa) #cosine package
library(infotheo) # discrizetion package

# read trainning Data
# クーポンマスター
list.train.d<-read.csv("~/Kaggle/Coupon Purchase Prediction/data/coupon_list_train.csv",header=T, fileEncoding="UTF-8",stringsAsFactors=F)
list.test.d<-read.csv("~/Kaggle/Coupon Purchase Prediction/data/coupon_list_test.csv",header=T, fileEncoding="UTF-8",stringsAsFactors=F)

nrow(list.train.d)
head(list.train.d)
unique(list.train.d$small_area_name)
nrow(list.test.d)


# クーポンVSエリア情報
area.train.d<-read.csv("~/Kaggle/Coupon Purchase Prediction/data/coupon_area_train.csv",header=T, fileEncoding="UTF-8",stringsAsFactors=F)
area.test.d<-read.csv("~/Kaggle/Coupon Purchase Prediction/data/coupon_area_test.csv",header=T, fileEncoding="UTF-8",stringsAsFactors=F)

length(unique(area.train.d$COUPON_ID_hash))
# テストデータは新しいクーポンのみ
area.test.d%>% inner_join(area.train.d,by="COUPON_ID_hash")
head(area.test.d)

# クーポン閲覧履歴
#visit.train.d<-read.csv("~/Kaggle/Coupon Purchase Prediction/data/coupon_visit_train.csv",header=T, fileEncoding="UTF-8",stringsAsFactors=F)
#head(visit.train.d)

# ユーザ購買履歴
detail.train.d<-read.csv("~/Kaggle/Coupon Purchase Prediction/data/coupon_detail_train.csv",header=T, fileEncoding="UTF-8",stringsAsFactors=F)

length(unique(detail.train.d$USER_ID_hash))

# ユーザ情報   
user.d<-read.csv("~/Kaggle/Coupon Purchase Prediction/data/user_list.csv",header=T, fileEncoding="UTF-8",stringsAsFactors=F)
length(unique(user.d$USER_ID_hash))

head(user.d)


#--------------------------------------------------------------------------------------
# ユーザのクラスタを実施
# ユーザマスタと購買履歴のJOIN
nrow(user.ma)
user.d<-user.d%>%mutate(AGE_F=ifelse(AGE>0 & AGE< 20 , "10-",
                                   ifelse(AGE >=20 & AGE <25 , "20-",
                                   ifelse(AGE >=25 & AGE <30 , "25-",
                                   ifelse(AGE >=30 & AGE <35 , "30-",
                                   ifelse(AGE >=35 & AGE <40 , "35-",
                                   ifelse(AGE >=40 & AGE <45 , "40-",
                                   ifelse(AGE >=45 & AGE <50 , "45-",
                                   ifelse(AGE >=50 & AGE <55 , "50-",
                                   ifelse(AGE >=55 & AGE <60 , "55-",
                                   ifelse(AGE >=60 & AGE <70 , "60-",
                                   ifelse(AGE >=70 & AGE <80 , "70-",
                                   ifelse(AGE >=80 , "80-",NA)))))))))))))
user.ma<-left_join(user.d,detail.train.d,by="USER_ID_hash")

head(user.age.cast)
user.age.cast<-user.ma %>% 
    dplyr::group_by(USER_ID_hash,AGE_F) %>%   
    dplyr::summarise(Total = n()) %>% data.frame %>%
    mutate(Total=ifelse(Total>0,1,0)) %>%   
    tidyr::spread(AGE_F,Total,fill=0,drop=FALSE) %>% data.frame
user.sex.cast<-user.ma %>% 
    dplyr::group_by(USER_ID_hash,SEX_ID) %>%   
    dplyr::summarise(Total = n()) %>% data.frame %>%
    mutate(Total=ifelse(Total>0,1,0)) %>%   
    tidyr::spread(SEX_ID,Total,fill=0,drop=FALSE) %>% data.frame
head(user.sex.cast)

colnames(user.ma)
head(user.ma)
user.item.cast<-user.ma %>% 
    dplyr::group_by(USER_ID_hash,COUPON_ID_hash) %>%   
    dplyr::summarise(Total = n()) %>% data.frame %>%
    mutate(Total=ifelse(Total>0,1,0)) %>%   
    tidyr::spread(COUPON_ID_hash,Total,fill=0,drop=FALSE) %>% data.frame

user.cast<-inner_join(user.sex.cast,user.age.cast,by="USER_ID_hash")%>%
    inner_join(user.item.cast,by="USER_ID_hash")
nrow(user.cast)
rownames(user.cast)<-user.item.cast$USER_ID_hash
colnames(user.cast)[1:10]
length(colnames(user.item.cast))
user.cast[is.na(user.cast)]<-0
colnames(user.item.cast)[1]

t<-proc.time()
coupon.user.cos<-cosine(t(as.matrix(user.cast[,-c(1)])))
proc.time()-t

#ユーザ   システム       経過  
#132824.64      15.07  132870.00 

write.csv(coupon.user.cos,"Coupon Purchase Prediction/data/coupon_user_cos.csv")
rm(coupon.user.cos)

# 距離行列に変換
diag(coupon.user.cos)<-1
coupon.user.dist<-as.dist(1-coupon.user.cos)
nrow(coupon.user.cos)
rownames(coupon.user.cos)

# 地域情報のクラスタリング
coupon.user.hc<-hclust(coupon.user.dist,method="ward.D2")
summary(coupon.user.hc$height)
hist(coupon.user.hc$height)
coupon.user.hc$height<-sort(coupon.user.hc$height)
user.class<-cutree(coupon.user.hc, h =2)
table(user.class)
length(user.class)

user.ma<-inner_join(user.d[,-c(7)],data.frame(USER_ID_hash=names(user.class),
                                     CL=user.class))
colnames(user.d)
nrow(user.ma)
head(detail.train.d)
#--------------------------------------------------------------------------------------
# クーポンのクラスタを実施
# テストのマスタも一緒にクラスタリング
head(coupon.ma.d)

coupon.ma.d<-rbind(list.train.d,list.test.d)
coupon.area.d<-rbind(area.train.d,area.test.d)
head(coupon.area.d)
#--------------------------------------------------------------------------------------
# 地域情報でクーポンのクラスタリング
# join by area and master data
coupon.area.tr<-coupon.ma.d %>% left_join(coupon.area.d,by="COUPON_ID_hash")%>%arrange(COUPON_ID_hash)
head(coupon.area.tr)
# cast by small_area_name
coupon.area.cast<- coupon.area.tr %>%dplyr::group_by(COUPON_ID_hash,SMALL_AREA_NAME) %>%
    dplyr::summarise(CNT = n())%>%data.frame %>%
    tidyr::spread(SMALL_AREA_NAME, CNT, fill=0, convert=FALSE)

rownames(coupon.area.cast)<-coupon.area.cast$COUPON_ID_hash
coupon.area.cast[is.na(coupon.area.cast)]<-0
nrow(coupon.area.cast)
# 地域情報をコサイン類似度→距離行列作成
t<-proc.time()
coupon.area.cos<-cosine(t(as.matrix(coupon.area.cast[,-c(1)])))
proc.time()-t
#ユーザ   システム       経過  
#2802.41       1.75    2807.08
getwd()
write.csv(coupon.area.cos,"Coupon Purchase Prediction/data/coupon_area_cos.csv")
rm(coupon.area.cos)
# 距離行列に変換
diag(coupon.area.cos)<-1
coupon.area.dist<-as.dist(1-coupon.area.cos)
nrow(coupon.area.cos)
# 地域情報のクラスタリング
coupon.area.hc<-hclust(coupon.area.dist,method="ward.D2")
summary(coupon.area.hc$height)
hist(coupon.area.hc$height)

area.class<-cutree(coupon.area.hc, h = 8)
table(area.class)
coupon.area.cl<-data.frame(COUPON_ID_hash=names(area.class),CLASS=area.class) %>% inner_join(coupon.ma.d,by="COUPON_ID_hash")

write.csv(coupon.area.cl,file="~/Kaggle/Coupon Purchase Prediction/area_class.csv",row.names = F)
subset(coupon.area.cl,CLASS==42)

length(area.class)

#--------------------------------------------------------------------------------------
# 地域情報以外でのクラスタリング

# PRICE関連 の離散化
coupon.price.disc<-discretize(dplyr::select(coupon.ma.d,PRICE_RATE,CATALOG_PRICE,
                                            DISCOUNT_PRICE), disc="equalwidth")
coupon.price.disc<-cbind(coupon.ma.d$COUPON_ID_hash,coupon.price.disc)
colnames(coupon.price.disc)[1]<-"COUPON_ID_hash"
# reolacement values
coupon.price.disc$PRICE_RATE<-paste("PRR",coupon.price.disc$PRICE_RATE,sep='_')
coupon.price.disc$DISCOUNT_PRICE<-paste("DP",coupon.price.disc$DISCOUNT_PRICE,sep='_')
coupon.price.disc$CATALOG_PRICE<-paste("CP",coupon.price.disc$CATALOG_PRICE,sep='_')

# cast by price_rate 
coupon.price_rate<- coupon.price.disc %>%dplyr::group_by(COUPON_ID_hash,PRICE_RATE) %>%
    dplyr::summarise(CNT = n())%>%data.frame %>%
    tidyr::spread(PRICE_RATE, CNT, fill=0, convert=FALSE)
# cast by discount_price 
coupon.discount_price<- coupon.price.disc %>%dplyr::group_by(COUPON_ID_hash,DISCOUNT_PRICE) %>%
    dplyr::summarise(CNT = n())%>%data.frame %>%
    tidyr::spread(DISCOUNT_PRICE, CNT, fill=0, convert=FALSE)
# cast by catalog_price
coupon.catalog_price<- coupon.price.disc %>%dplyr::group_by(COUPON_ID_hash,CATALOG_PRICE) %>%
    dplyr::summarise(CNT = n())%>%data.frame %>%
    tidyr::spread(CATALOG_PRICE, CNT, fill=0, convert=FALSE)
# all price join
coupon.price.cast<-Reduce(dplyr::inner_join, 
                          list(coupon.discount_price,coupon.price_rate,coupon.catalog_price))
# cast by coupon genre 
coupon.genre<- coupon.ma.d %>%dplyr::group_by(COUPON_ID_hash,GENRE_NAME) %>%
    dplyr::summarise(CNT = n())%>%data.frame %>%
    tidyr::spread(GENRE_NAME, CNT, fill=0, convert=FALSE)
# all coupon data join
coupon.ma.cast<-Reduce(inner_join,list(coupon.price.cast,
                         dplyr::select(coupon.ma.d,COUPON_ID_hash,USABLE_DATE_MON,USABLE_DATE_TUE,
                                   USABLE_DATE_WED,USABLE_DATE_THU,USABLE_DATE_FRI,
                                   USABLE_DATE_SAT,USABLE_DATE_SUN,USABLE_DATE_HOLIDAY,
                                   USABLE_DATE_BEFORE_HOLIDAY),
                         coupon.genre))
head(coupon.ma.cast)

# replacement Data
coupon.ma.cast[is.na(coupon.ma.cast)]<-0
coupon.ma.cast<-coupon.ma.cast %>%
    mutate(USABLE_DATE_MON=ifelse(USABLE_DATE_MON>=2,1,USABLE_DATE_MON),
           USABLE_DATE_TUE=ifelse(USABLE_DATE_TUE>=2,1,USABLE_DATE_TUE),
           USABLE_DATE_WED=ifelse(USABLE_DATE_WED>=2,1,USABLE_DATE_WED),
           USABLE_DATE_THU=ifelse(USABLE_DATE_THU>=2,1,USABLE_DATE_THU),
           USABLE_DATE_FRI=ifelse(USABLE_DATE_FRI>=2,1,USABLE_DATE_FRI),
           USABLE_DATE_SAT=ifelse(USABLE_DATE_SAT>=2,1,USABLE_DATE_SAT),
           USABLE_DATE_SUN=ifelse(USABLE_DATE_SUN>=2,1,USABLE_DATE_SUN),
           USABLE_DATE_HOLIDAY=ifelse(USABLE_DATE_HOLIDAY>=2,1,USABLE_DATE_HOLIDAY),
           USABLE_DATE_BEFORE_HOLIDAY=ifelse(USABLE_DATE_BEFORE_HOLIDAY>=2,1,USABLE_DATE_BEFORE_HOLIDAY))
rownames(coupon.ma.cast)<-coupon.ma.cast$COUPON_ID_hash

rownames(coupon.ma.cast)
table(coupon.ma.cast$USABLE_DATE_HOLIDAY)
gc()

# クーポン情報のコサイン類似度
t<-proc.time()
coupon.ma.cos<-cosine(t(as.matrix(coupon.ma.cast[,-c(1)])))
proc.time()-t
write.csv(coupon.ma.cos,"Coupon Purchase Prediction/data/coupon_ma_cos.csv")
rm(coupon.ma.cos)
#write.csv(coupon.ma.cos,"coupon_ma_cos.csv")
nrow(coupon.ma.cos)
# 距離行列に変換
diag(coupon.ma.cos)<-1
coupon.ma.dist<-as.dist(1-coupon.ma.cos)

# クーポン情報のクラスタリング
coupon.ma.hc<-hclust(coupon.ma.dist,method="ward.D2")
#plot(coupon.ma.hc)
summary(coupon.ma.hc$height)

ma.class<-cutree(coupon.ma.hc, h = 2)
table(ma.class)
coupon.ma.cl<-data.frame(COUPON_ID_hash=names(ma.class),CLASS=ma.class) %>% inner_join(coupon.ma.d,by="COUPON_ID_hash")

write.csv(coupon.ma.cl,file="~/Kaggle/Coupon Purchase Prediction/ma_class.csv",row.names = F)
subset(coupon.ma.cl,CLASS==42)

head(coupon.area.cl)
length(ma.class)
#--------------------------------------------------------------------------------------
user.class<-cutree(coupon.user.hc, h =2)
table(user.class)
length(user.class)

ma.class<-cutree(coupon.ma.hc, h = 2)

(table(ma.class))

area.class<-cutree(coupon.area.hc, h = 3)
(table(area.class))

# join by all clas data 
coupon.all.cl<-data.frame(COUPON_ID_hash=names(ma.class),MA_CLASS=ma.class) %>% 
    inner_join(data.frame(COUPON_ID_hash=names(area.class),AREA_CLASS=area.class),
               by="COUPON_ID_hash")%>% mutate(ALL_CLASS=paste(MA_CLASS,AREA_CLASS,sep=":"))

sum(table(coupon.all.cl$ALL_CLASS))
#--------------------------------------------------------------------------------------
# 学習データ作成
coupon.train.cl<-coupon.all.cl%>%dplyr::filter(COUPON_ID_hash %in%area.train.d$COUPON_ID_hash)
nrow(coupon.all.cl)

# ユーザマスタと購買履歴のJOIN
user.ma<-inner_join(user.d[,-c(7)],data.frame(USER_ID_hash=names(user.class),
                                              CL=user.class))

user.ma<-left_join(user.ma,detail.train.d,by="USER_ID_hash")
length(unique(user.ma$USER_ID_hash))
head(user.ma)

# user &  cl
coupon.train.d<-left_join(user.ma,coupon.train.cl,by="COUPON_ID_hash")
length(unique(coupon.train.d$USER_ID_hash))
head(coupon.train.d)
#subset(coupon.train.d,is.na(ALL_CLASS))

# user毎の最大化クラス
head(coupon.train.d)
User.Coupon.cl<-coupon.train.d%>%group_by(MA_CLASS,AREA_CLASS,CL)%>%summarise(N=n())
head(User.Coupon.cl)

User.cl<-User.Coupon.cl%>%group_by(CL)%>% summarise(MAX=max(N))
head(User.cl)

User.Coupon.res<-User.Coupon.cl%>%inner_join(User.cl)%>%dplyr::filter(N==MAX)
length(unique(User.Coupon.res$CL))

head(User.Coupon.res)
table(User.Coupon.res$AREA_CLASS)
table(User.Coupon.res$MA_CLASS)
table(User.Coupon.res$CL)

gc()

# test
# list.test.d
# area.test.d
nrow(list.test.d)
nrow(area.test.d)

coupon.test.cl<-coupon.all.cl%>%dplyr::filter(COUPON_ID_hash %in%
                                                  area.test.d$COUPON_ID_hash)
head(coupon.test.cl)
nrow(coupon.test.cl)

table(coupon.test.cl$MA_CLASS)
table(ma.class)
table(coupon.test.cl$AREA_CLASS)
table(area.class)
# check
table(User.Coupon.res$MA_CLAS,User.Coupon.res$AREA_CLASS)
table(User.Coupon.res$CL,User.Coupon.res$MA_CLAS)

Coupon.test.tmp<-coupon.test.cl %>% inner_join(User.Coupon.res)
subset(Coupon.test.tmp,CL==3)
length(unique(Coupon.test.tmp$COUPON_ID_hash))


Coupon.test.n<-Coupon.test.tmp%>%group_by(CL)%>% summarise(n=n())
subset(Coupon.test.n,n>1)

# data csv 
head(list.test.d)
head(user.d)
nrow(user.d)
test.res<-Coupon.test.tmp%>% inner_join(list.test.d) %>% inner_join(user.d)
nrow(test.res)


# rank by
head()
Coupon.test.res<-Coupon.test.tmp%>%group_by(CL)%>% 
    filter(min_rank(desc(COUPON_ID_hash))==1)

head(Coupon.test.res)
table(Coupon.test.res$AREA_CLASS)
table(Coupon.test.res$MA_CLASS)

length(unique(Coupon.test.res$CL))

nrow(list.test.d)
nrow(user.d)

res<-user.d%>%left_join(Coupon.test.res)%>%dplyr::select(USER_ID_hash,COUPON_ID_hash,CL)
nrow(res)
unique(subset(res,is.na(COUPON_ID_hash))$CL)
length(user.class[user.class==134])

library(Lahman)

head(res)
nrow(res)
length(unique(res$COUPON_ID_hash))
length(unique(res$USER_ID_hash))

colnames(res)[2]<-"PURCHASED_COUPONS"
write.csv(res,file="~/Kaggle/Coupon Purchase Prediction/submisstion/submission1.csv",row.names = F,quote = F)

#----------------------------------------------
# メモリ足りずできない
library("VGAM")
colnames()
head(coupon.test.d)
coupon.train.d$CL<-as.factor(coupon.train.d$CL)
coupon.train.d$AGE_F<-as.factor(coupon.train.d$AGE_F)
table(coupon.train.d$CL)
head(coupon.train.d)
unique(coupon.train$ALL_CLASS)

subset(coupon.train,is.na(ALL_CLASS))

coupon.train<-coupon.train.d%>% group_by(ALL_CLASS,CL,SEX_ID,AGE_F)%>%
    dplyr::summarise(CNT = n())%>%data.frame 
head(coupon.train)
nrow(coupon.train)


cp.vglm.cl<-vglm(ALL_CLASS~CL+SEX_ID+AGE_F+CNT,
              coupon.train,
              family=multinomial)

nrow(user.d)

out.vglm<-predict(cp.vglm,newdata=user.d,type="response")
