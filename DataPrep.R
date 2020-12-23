gdp<-read.csv(file="~/Desktop/Datasets/GDP.csv")
suicide<-read.csv(file="~/Desktop/Datasets/master.csv")
names(gdp)<-c("Country.Name","Country.Code","Indicator.Name","Indicator.Code","1960","1961","1962","1963","1964","1965","1966","1967","1968","1969","1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982","1983","1984","1985","1986","1987","1988","1989","1990","1991","1992","1993","1994","1995","1996","1997","1998","1999","2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017","2018")
#index(John)
n<-dim(gdp)[1]
gdp$Index<-c(1:n)
m<-dim(suicide)[1]
suicide$Index<-c(1:m)
head(gdp)
head(suicide)

#outliers(Michael)
suicide$suicides_no_z<-scale(x=suicide$suicides_no)
suicide_outliers<-suicide[which(suicide$suicides_no_z< -3| suicide$suicides_no_z > 3),]
suicide_sort<- suicide_outliers[order(- suicide_outliers$suicides_no_z),]
head(suicide_sort)
#contingency tables that return strange results(Michael)
t.v1 <- table(suicide$suicides_no, suicide$sex)
t.v2<-addmargins(A=t.v1, FUN = list (total=sum), quiet = TRUE)
t.v1.rnd<-round(prop.table(t.v1, margin=2)*100,1)

t.v1 <- table(suicide$suicides_no, groupbysuicide$country)
t.v2<-addmargins(A=t.v1, FUN = list (total=sum), quiet = TRUE)
t.v1.rnd<-round(prop.table(t.v1, margin=2)*100,1)

t.v1 <- table(suicide$sex, suicide$country)
t.v2<-addmargins(A=t.v1, FUN = list (total=sum), quiet = TRUE)
t.v1.rnd<-round(prop.table(t.v1, margin=2)*100,1)

t.v1 <- table(suicide$suicides_no, suicide$year)
t.v2<-addmargins(A=t.v1, FUN = list (total=sum), quiet = TRUE)
t.v1.rnd<-round(prop.table(t.v1, margin=2)*100,1)

t.v1 <- table(suicide$age, suicide$country)
t.v2<-addmargins(A=t.v1, FUN = list (total=sum), quiet = TRUE)
t.v1.rnd<-round(prop.table(t.v1, margin=2)*100,1)

#graphs that return strange results(Michael)
library(ggplot2)
ggplot(suicide, aes(suicides_no))+geom_histogram(aes(fill=sex),color="black")
ggplot(suicide, aes(suicides_no))+geom_histogram(aes(fill=age),color="black")

ggplot(suicide, aes(country))+geom_bar(aes(fill=sex))+coord_flip()
ggplot(suicide, aes(country))+geom_bar(aes(fill=age))+coord_flip()
ggplot(suicide,aes(year) )+geom_bar()
ggplot(suicide,aes(year) )+geom_bar(aes(fill=sex))

#tables with group_by() implemented that return accurate and helpful results(Michael)
library(dplyr)

sui_by_country <-tbl_df(suicide)
sui_by_country %>%
  group_by(country) %>%
  summarize(sum_suicides=sum(suicides_no, na.rm=TRUE))%>%
  print(n=101)

total_suicides=sum(suicide$suicides_no, na.rm=TRUE)
sui_by_age <-tbl_df(suicide)
sui_by_age %>%
  group_by(age) %>%
  summarize(sum_suicides=sum(suicides_no, na.rm=TRUE))


sui_by_sex <-tbl_df(suicide)
sui_by_sex %>%
  group_by(sex) %>%
  summarize(sum_suicides=sum(suicides_no, na.rm=TRUE))

#Bin based on predictive value to enhance models(Steven)
suicide$suicides.100k_binned <- cut(x = suicide$suicides.100k.pop, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110.01, 300),
right = FALSE, labels = c("Under 10", "10 to 20", "20 to 30", "30 to 40", "40 to 50", "50 to 60", "60 to 70", "70 to 80", "80 to 90", "90 to 100", "100 to 110" ,"Over 110"))

suicide$gdp_per_capita_binned<-cut(x=suicide$gdp_per_capita...., breaks = c(0, 500, 2500, 5000, 10000, 15000, 25000, 50000, 75000 , 100000, 125000, 130000 ),
    right = FALSE, labels= c("<500", "500-2.5k", "2.5k-5k","5k-10k","10k-15k", "15k-25k", "25k-50k", "50k-75k", "75k-100k", "100k-125k",">125k"))

gdp[5:29]<-list(NULL)
suicide$sui_perc_pop<- suicide$suicides_no/suicide$population
sui_by_spp <-tbl_df(suicide)
sui_by_spp %>%
  group_by(year) %>%
  summarize(sum_suicides=sum(sui_perc_pop, na.rm=TRUE))


#SETUP PHASE
set.seed(7)
n <-dim(suicide)[1]
m <-dim(gdp)[1]
ind_train<- runif(n) < 0.75
sui_train <- suicide[ind_train, ]
sui_test <- suicide[!ind_train, ]
gdp_train <- gdp[ind_train, ]
gdp_test <- gdp[!ind_train, ]


t.test(sui_train$suicides_no, sui_test$suicides_no)
t.test(sui_train$population, sui_test$population)

t.test(gdp_train$`2016`, gdp_test$`2016`)
t.test(gdp_train$`2001`, gdp_test$`2001`)
t.test(gdp_train$`1985`, gdp_test$`1985`)
t.test(gdp_train$`1995`, gdp_test$`1995`)
t.test(gdp_train$`2018`, gdp_test$`2018`)
#checking sex balance
p1<-sum(sui_train$sex=="female")/dim(sui_train)[1]
p2<-sum(sui_test$sex=="female")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$sex=="female")+sum(sui_test$sex=="female"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))

p1<-sum(sui_train$sex=="male")/dim(sui_train)[1]
p2<-sum(sui_test$sex=="male")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$sex=="male")+sum(sui_test$sex=="male"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))

#checking gdp per capita balance
p1<-sum(sui_train$gdp_per_capita_binned=="<500")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="<500")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="<500")+sum(sui_test$gdp_per_capita_binned=="<500"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))

p1<-sum(sui_train$gdp_per_capita_binned=="500-2.5k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="500-2.5k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="500-2.5k")+sum(sui_test$gdp_per_capita_binned=="500-2.5k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))

p1<-sum(sui_train$gdp_per_capita_binned=="2.5k-5k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="2.5k-5k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="2.5k-5k")+sum(sui_test$gdp_per_capita_binned=="2.5k-5k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))

p1<-sum(sui_train$gdp_per_capita_binned=="5k-10k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="5k-10k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="5k-10k")+sum(sui_test$gdp_per_capita_binned=="5k-10k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))

p1<-sum(sui_train$gdp_per_capita_binned=="10k-15k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="10k-15k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="10k-15k")+sum(sui_test$gdp_per_capita_binned=="10k-15k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))

p1<-sum(sui_train$gdp_per_capita_binned=="15k-25k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="15k-25k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="15k-25k")+sum(sui_test$gdp_per_capita_binned=="15k-25k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))


p1<-sum(sui_train$gdp_per_capita_binned=="25k-50k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="25k-50k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="25k-50k")+sum(sui_test$gdp_per_capita_binned=="25k-50k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))

p1<-sum(sui_train$gdp_per_capita_binned=="50k-75k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="50k-75k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="50k-75k")+sum(sui_test$gdp_per_capita_binned=="50k-75k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))


p1<-sum(sui_train$gdp_per_capita_binned=="75k-100k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="75k-100k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="75k-100k")+sum(sui_test$gdp_per_capita_binned=="75k-100k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))


p1<-sum(sui_train$gdp_per_capita_binned=="100k-125k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned=="100k-125k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned=="100k-125k")+sum(sui_test$gdp_per_capita_binned=="100k-125k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))



p1<-sum(sui_train$gdp_per_capita_binned==">125k")/dim(sui_train)[1]
p2<-sum(sui_test$gdp_per_capita_binned==">125k")/dim(sui_test)[1]

p_pooled<- (sum(sui_train$gdp_per_capita_binned==">125k")+sum(sui_test$gdp_per_capita_binned==">125k"))/(dim(sui_train)[1]+dim(sui_test)[1])
z<-(p1-p2)/sqrt(p_pooled *(1-p_pooled) * (1/dim(sui_train)[1]+1/dim(sui_test)[1]))



sui_by_co_train <-tbl_df(sui_train)
sui_by_co_train %>%
  group_by(country) %>%
  summarize(sum_suicides=sum(suicides_no, na.rm=TRUE))%>%
  print(n=101)

total_sui=sum(sui_train$suicides_no, na.rm=TRUE)

sui_by_co_train <-tbl_df(sui_train)
sui_by_co_train %>%
  group_by(gdp_per_capita_binned) %>%
  summarize(sum_suicide=sum(suicides_no, na.rm=TRUE))%>%
  print(n=11)

total_sui=sum(sui_train$suicides_no, na.rm=TRUE)
#The baseline performance is that the gdp per capita range with the most suicides is 25k to 50k range. 1652154 total amount of suicides. 
#1652154/5061969=32.63% of the total amount of suicides

#modeling phase
sui_train$suicides.100k_binned<-factor(sui_train$suicides.100k_binned)
sui_test$suicides.100k_binned<-factor(sui_test$suicides.100k_binned)
sui_train$sex<-factor(sui_train$sex)
sui_train$year<-factor(sui_train$year)
sui_train$gdp_per_capita_binned<-factor(sui_train$gdp_per_capita_binned)
sui_test$gdp_per_capita_binned<-factor(sui_test$gdp_per_capita_binned)

cart01 <- rpart(formula =  gdp_per_capita_binned~ suicides.100k_binned, data = sui_train, method = "class")
rpart.plot(cart01)

#C5.0
x=data.frame(suicides_no=sui_train$suicides_no)
c5<- C5.0(formula = gdp_per_capita_binned~ suicides_no, data=sui_train, control = C5.0Control(minCases=75))
plot(c5)
predict(object = c5, newdata= x)

x=data.frame(suicides.100k_binned=sui_train$suicides.100k_binned)
c5<- C5.0(formula = gdp_per_capita_binned~ suicides.100k_binned, data=sui_train, control = C5.0Control(minCases=75))
plot(c5)
predict(object = c5, newdata= x)

#random forest
library(randomForest)
rf01<-randomForest(formula=gdp_per_capita_binned~ suicides_no, data=sui_train, ntree=100, type="classification")
rf01$predicted

#confusion matrix
test.X<-subset(x=sui_test,select=c("suicides.100k_binned"))
ypred<-predict(object= c5, newdata= test.X)

t1<-table(sui_test$gdp_per_capita_binned, ypred)
row.names(t1)<-c("Actual:<500","Actual:500-2.5k","Actual:2.5k-5k","Actual:5k-10k","Actual:10k-15k","Actual:15k-25k","Actual:25k-50k","Actual:50k-75k","Actual:75k-100k","Actual:100k-125k","Actual:>125k")
colnames(t1)<-c("Predicted:<500","Predicted:500-2.5k","Predicted:2.5k-5k","Predicted:5k-10k","Predicted:10k-15k","Predicted:15k-25k","Predicted:25k-50k","Predicted:50k-75k","Predicted:75k-100k","Predicted:100k-125k","Predicted:>125k")
t1<-addmargins(A=t1, FUN=list(Total=sum),quiet=TRUE)
t1

accuracy<-(t1[1,1]+t1[2,2]+t1[3,3]+t1[4,4]+t1[5,5]+t1[6,6]+t1[7,7]+t1[8,8]+t1[9,9]+t1[10,10])/t1[11,11]
error_rate<-1-accuracy
sensitivity<-t1[2,2]/t1[2,11]



#clustering
X<-subset(sui_train, select=c("suicides_no","gdp_per_capita...."))
Xs<-as.data.frame(scale(X))
colnames(Xs)<-c("suicides_no_z","gdp_per_capita_binned_z")
library(stats)
kmeans01<-kmeans(Xs, centers=2)
cluster<-as.factor(kmeans01$cluster)
Cluster1<-Xs[which(cluster==1),]
Cluster2<-Xs[which(cluster==2),]
summary(Cluster1)
summary(Cluster2)

X_test<-subset(sui_test, select=c("suicides_no","gdp_per_capita...."))
Xs_test<-as.data.frame(scale(X))
colnames(Xs_test)<-c("suicides_no_z","gdp_per_capita_z")
library(stats)
kmeans01_test<-kmeans(Xs_test, centers=2)
cluster_test<-as.factor(kmeans01_test$cluster)
Cluster1_test<-Xs_test[which(cluster_test==1),]
Cluster2_test<-Xs_test[which(cluster_test==2),]
summary(Cluster1_test)
summary(Cluster2_test)




