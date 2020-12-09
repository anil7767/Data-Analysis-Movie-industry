## Required libraries
library(GGally)
library(dplyr)
library(nnet)
library(gridExtra)
library(ggplot2)
library(caret)
library(lattice)
library(MASS)
library(klaR)
library(zoo)
library(clue)

data=read.csv('C:/Users/ANIL/Downloads/movies.csv')
dim(data)
summary(data)
data_type=sapply(data, class)  #to get data type of each column
fact_data=data[data_type=='factor']  #to get only factors from all columns
num_data=data[data_type!='factor']  #to get only numeric  from all columns

#Data-preprocessing
#1)Checking missing values
colnames(num_data)
any(num_data$budget==0)  #check whether budget columns are zero or not
#Building Correlation plot
ggcorr(num_data, name = "Correlation", label = TRUE, alpha = TRUE, palette = "PuOr") +
  ggtitle("correlation matrix plot") + theme_dark()

#we see that correlation between gross and budget is 0.7
#hence we compute missing values of budget by using Gross

#1)
#we fit linear regression model 
#here budget be response and gross be the regressor
data_nonzero=subset(num_data, budget!= 0) 
data_zero=subset(num_data, budget==0) 
dim(num_data);dim(data_nonzero);dim(data_zero)
#we see that 2182 values of budget columns are missing

model=lm(data_nonzero$budget~data_nonzero$gross,data=data_nonzero)
df1=data.frame(model$fitted.values,data_nonzero$budget)
colnames(df1)=c('Fitted_values','Actual_values')
#acccuracy
d = data_nonzero$budget-model$fitted.values

d=scale(data_nonzero$budget)-scale(model$fitted.values)  #scale data
mse = mean((d)^2)
mae = mean(abs(d))
rmse = sqrt(mse)
mse;mae;rmse
#here we see that mse,mae,rmse are moderate,hence we use linear regression
#to impute missing values

#Prediction of Missing Values
prdf=as.data.frame(data_zero$gross)
miss_budgt_pred=predict(model,newdata=prdf)
fill_miss_data=num_data
for (i in 1:nrow(data_zero)) {
  if(data_zero$budget[i]==0)
  {
    fill_miss_data$budget[i]=miss_budgt_pred[i]
  }
}

#Getting numeric  Data frame with missing values
#numeric_data=bind_rows(data_nonzero,fill_miss_data)
#merge(a,b,by=0)
Total_data=cbind(fill_miss_data,fact_data,by = 0)[,-16]

#2)
#Missing of rating columns
unique(Total_data$rating)
#Here we see that some movies rating are Not specified
Not_specifi_rating=subset(Total_data,rating=='Not specified')
specifi_rating=subset(Total_data,rating!='Not specified')
dim(Not_specifi_rating);dim(specifi_rating)
#here we see that only 63 values are missing hence we remove that part from data

Final_data=subset(Total_data,rating!='Not specified')
Final_data=subset(Final_data,budget!=0)
Final_data['Re_Month']=as.numeric(format(as.Date(Final_data$released), "%m"))
Final_data=subset(Final_data,select = -c(name,released))
dim(Final_data)

#Box plot model for Genere wise movie runtime in minutes
p_genrerun = ggplot(Final_data, aes(x=factor(genre), y=runtime)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Genre to runtime") + 
  geom_hline(yintercept =median(Final_data$runtime,na.rm = TRUE), col = "royalblue",lwd = 1)
#Box plot model for Genere wise score
p_genrerating = ggplot(Final_data, aes(x=factor(genre), y=score)) +
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ggtitle("Genre to rating") + 
  geom_hline(yintercept=median(Final_data$score, na.rm = TRUE), col = "royalblue",lwd = 1)


#Histogram along with Scatterplot showing theater release month and year data
g1=ggplot(data = na.omit(Final_data), aes(x = Re_Month)) + geom_histogram(colour = "black", fill =
                                                                     "orange", alpha = 0.5)
g2=ggplot(data = na.omit(Total_data), aes(x = year)) + geom_histogram(colour = "black", fill =
                                                                      "blue", alpha = 0.5)
grid.arrange(g1, g2,p_genrerun,p_genrerating,nrow = 2, ncol = 2)


#Q.1---------------------------------------------------------------
#ROI=(gross-budget)/budget
ROI=(Final_data$gross-Final_data$budget)/Final_data$budget
Final_data['ROI']=ROI
Final_data=arrange(Final_data,desc(ROI))
boxplot(Final_data$ROI,pch=19)
scatter.smooth(Final_data$ROI,type='b',pch=19)
#Here we see that first three points are two large i.e. outlier
#hence we remove it

Arr_data=arrange(Final_data,desc(ROI))
ggplot(data=Arr_data,aes(y=ROI,x=score,colour=rating))+
  geom_point()
summary(Arr_data)
#For categorical variables selection,which are related to ROI
s=c(15,8,9,10,11,12,13,14)
Cat_Data=Arr_data[,s]
summary(Cat_Data)
#--For Choice_Of_Star----------------
y=Cat_Data$ROI
x=Cat_Data$star
v=c(unique(as.character(x)))
SMean=c()
for(i in 1:length(v)){
  ystar=y[which(Cat_Data$star==v[i])]
  SMean[i]=mean(ystar)
}
Choice_Of_Star=v[which(SMean==max(SMean))];Choice_Of_Star

#--For Choice_Of_Director----------------
D=Cat_Data$director
v=c(unique(as.character(D)))
SMean=c()
for(i in 1:length(v)){
  ystar=y[which(Cat_Data$director==v[i])]
  SMean[i]=mean(ystar)
}
Choice_Of_Director=v[which(SMean==max(SMean))];Choice_Of_Director


#--For Choice_Of_Writer----------------
W=Cat_Data$writer
v=c(unique(as.character(W)))
SMean=c()
for(i in 1:length(v)){
  ystar=y[which(Cat_Data$writer==v[i])]
  SMean[i]=mean(ystar)
}
Choice_Of_Writer=v[which(SMean==max(SMean))];Choice_Of_Writer
#--For Choice_Of_Contry----------------
c=Cat_Data$country
v=c(unique(as.character(c)))
SMean=c()
for(i in 1:length(v)){
  ystar=y[which(Cat_Data$country==v[i])]
  SMean[i]=mean(ystar)
}
Choice_Of_Contry=v[which(SMean==max(SMean))];Choice_Of_Contry

#--For Choice_Of_Genre----------------
g=Cat_Data$genre
v=c(unique(as.character(g)))
SMean=c()
for(i in 1:length(v)){
  ystar=y[which(Cat_Data$genre==v[i])]
  SMean[i]=mean(ystar)
}
Choice_Of_Genre=v[which(SMean==max(SMean))];Choice_Of_Genre

#--For Choice_Of_Rating----------------
r=Cat_Data$rating
v=c(unique(as.character(r)))
SMean=c()
for(i in 1:length(v)){
  ystar=y[which(Cat_Data$rating==v[i])]
  SMean[i]=mean(ystar)
}
Choice_Of_Rating=v[which(SMean==max(SMean))];Choice_Of_Rating

#--For Choice_Of_Re_month----------------
re=Cat_Data$Re_Month
v=c(unique(re))
length(which(is.na(re))) # only 57 observation is missing which we can neglect
v=c(na.omit(v))
SMean=c()
for(i in 1:length(v)){
  ystar=y[which(Cat_Data$Re_Month==v[i])]
  SMean[i]=mean(ystar)
}
Choice_Of_Re_month=v[which(SMean==max(SMean))];Choice_Of_Re_month

#-----Summary Result----------------
Variables=c("Choice_Of_Star","Choice_Of_Director","Choice_Of_Writer","Choice_Of_Genre","Choice_Of_Re_month","Choice_Of_Rating")
Choice=c(Choice_Of_Star,Choice_Of_Director,Choice_Of_Writer,Choice_Of_Genre,Choice_Of_Re_month,Choice_Of_Rating)
Cat_summary=data.frame(Variables,Choice);Cat_summary
aa=Cat_summary$Choice
Hroi=c(as.character(Arr_data$star[aa[1]]),as.character(Arr_data$director[aa[2]]),as.character(Arr_data$writer[aa[3]]),as.character(Arr_data$genre[aa[4]]),as.character(Arr_data$Re_Month[aa[5]]),as.character(Arr_data$rating[aa[6]]))
Cat_summary['info']=Hroi
Cat_summary

#----For Continuous variables------------
#since non of this correlation are modarate
#hence we go for increase the gross and decrease the Budget,so as ROI is increase
#but we know gross and budget are positive correlated hence we only going to gross
s=c(2,1,3,4,5)
K=Arr_data[,s]
ContinuousData=Arr_data[,s]
plot(ContinuousData)
cor(ContinuousData)
#we see that gross is high related with votes and budget
#hence we can predict gross from votes
model=lm(ContinuousData$gross~ContinuousData$votes+ContinuousData$budget,ContinuousData)
Cont_S=summary(model)
#we can see that model is  significant
#---------Summary----------------------
Catagorical_Data_summary=Cat_summary;Catagorical_Data_summary
Continuous_Data_summary=Cont_S;Continuous_Data_summary

#Q.2------------------
#in case of actor
set.seed(12)
rep_data=subset(Final_data,select= c(star,director,writer))
Kmode=kmodes(rep_data,modes=3,iter.max = 10, weighted = FALSE, fast = TRUE)
plot(Kmode$withindiff,type='b')
Kmode

#
aggfunc=function(coln='director-star-writer'){
  a=aggregate(Final_data$ROI, by=list(Final_data[,coln]), FUN=mean)
  b=aggregate(Final_data$votes, by=list(Final_data[,coln]), FUN=sum)
  c=aggregate(Final_data$score, by=list(Final_data[,coln]), FUN=mean)
  Group_m=data.frame(b,a[,2],c[,2])
  colnames(Group_m)=c(coln,'votes','ROI','score')
  return(Group_m)
}

#
kmean_withinss = function(k,arrd) {
  cluster = kmeans(scale(arrd), k,iter.max = 50)  #remove first two row
  return (cluster$tot.withinss)
}


#Replace for perticular 
Rep_direct=function(da='data',anyrep='director-star-writer name'){
  if(anyrep %in% da[,1])
  {
    temp1=which(da[,1]==anyrep)
    df=subset(da,Cluster=da[temp1,]$Cluster)
    temp=''
    maxi=sort(df$ROI,decreasing = TRUE)
    temp=subset(df,ROI==maxi[1])[,1]
    if(temp==anyrep){
      temp=subset(df,ROI==maxi[2])[,1]
    }else{
      temp=temp
    }
  }else{
    temp='Given Name Not exist'
  }
  return(temp)
}

#
elbow_method=function(arrd){
  wss=c(0)
  max_k=30
  for (i in 1:max_k) {
    wss[i]=kmean_withinss(i,arrd)
  }
  elbow=data.frame(1:max_k, wss)
  library(ggplot2)
  ggplot(elbow, aes(x = X1.max_k, y = wss)) +
    geom_point() +
    geom_line() +
    scale_x_continuous(breaks = seq(1, max_k, by = 1))
}

#a)#replace director
Group_m=aggfunc('director')
arrd=Group_m[,-1]
elbow_method(arrd)
Kmean=kmeans(scale(arrd),9,iter.max = 20)
#data with respect to cluster
Newd=arrange(Group_m,desc(ROI))
Newd['Cluster']=Kmean$cluster
#prediction
Rep_direct(Newd,'Doug Liman') #replace that director

#b)
#a)#replace star
Group_m=aggfunc('star')
arrd=Group_m[,-1]
elbow_method(arrd)
Kmean=kmeans(scale(arrd),9,iter.max = 20)
#data with respect to cluster
Newd=arrange(Group_m,desc(ROI))
Newd['Cluster']=Kmean$cluster
#prediction
Rep_direct(Newd,'Sean Gullette')

#c)#writer
set.seed(12)
Group_m=aggfunc('writer')
arrd=Group_m[,-1]
elbow_method(arrd)
Kmean=kmeans(scale(arrd),9,iter.max = 20)
#data with respect to cluster
Newd=arrange(Group_m,desc(ROI))
Newd['Cluster']=Kmean$cluster
#prediction
Rep_direct(Newd,'Jared Hess')

#Q.3
#Succes:According to my point of view the defination of success would be
#we choose that country which will give the high gross collection and
#which having higest votes
#a)
suc_data1=subset(Final_data,select = c(votes,gross,country,star,director))
head(suc_data1)
a1=aggregate(Final_data$gross, by=list(Final_data$country), FUN=mean)
b1=aggregate(Final_data$votes, by=list(Final_data$country), FUN=sum)
df=data.frame(a1,b1[,2])
colnames(df)=c('Country','gross','votes')
head(df)
#arrange(df,desc(votes))

#Plot with respect to votes
ggplot(data=arrange(suc_data1,desc(votes))[1:50,],aes(y=gross,x=votes,colour=country))+
  geom_point()

#Here we plot top 50 values of gross collection after arranging
#data with respect to country we see that in top only four country's
#having highest number of gross collection and having high number votes
#Hence we say that USA is prefered city for producer
ggplot(data=arrange(df,desc(votes)),aes(y=gross,x=votes,colour=Country))+
  geom_point()

#b) we choose that city where more number of stars and directors are living

s=suc_data1$star
C=suc_data1$country
d=suc_data1$director
v=as.character(unique(C))

#aggregate(df1$n, by=list(df1$country), FUN=sum)
no_star=c(0)
no_dir=c(0)
for (i in 1:length(v)){
  no_star[i]=length(unique(s[c(which(C==v[i]))]))
  no_dir[i]=length(unique(d[c(which(C==v[i]))]))
}
dd=data.frame('Country'=v,no_star,no_dir)
head(dd)

#for director
arr_dir=arrange(dd,desc(no_dir))
arr_dir=arr_dir[1:10,]
barplot(arr_dir$no_dir, 
        main = "Number of Director by Country",
        xlab = "Country",
        ylab = "Director",
        names = arr_dir$Country)

#for stars
arr_str=arrange(dd,desc(no_star))
arr_str=arr_str[1:10,]
barplot(arr_str$no_star, 
        main = "Number of Stars by Country",
        xlab = "Country",
        ylab = "Stars",
        names = arr_str$Country)

#Hence we see that most of stars and directors are belongs to USA
#Hence we may settel in USA
#It it prefered country
