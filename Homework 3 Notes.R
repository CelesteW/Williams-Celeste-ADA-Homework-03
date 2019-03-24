#Problem 1: Two-Sample Z test
x<-c(1, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 
     1, 0)
y<-c(1, 1, 0, 0, 1, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 1, 1, 0, 
     0, 1, 1, 0, 1, 1, 1)
p1<-sum(x)
n1<-length(x)
p2<- sum(y)
n2<- length(y)
alt<- "two.sided"
CL<-0.95

Z.prop.test <- function(p1,n1,p2,n2,alt,CL) {
  pstar<-(p1+p2)/(n1+n2)
  phat1<- p1/ n1
  phat2<- p2/n2
  pi<-0
  Z<-(phat2 - phat1)/ sqrt((pstar * (1-pstar))*((1/n1)+ (1/n2)))
  n1 * pi >5
  n2 * pi>5
  n1*(1-pi)> 5
  n2*(1-pi)>5
  p.upper<-1-pnorm(Z, lower.tail=TRUE)
  p.lower<- pnorm(Z,lower.tail = FALSE)
  p<- p.upper + p.lower
  pt <- prop.test(x = c(p2,p1), n = c(n2,n1), alternative = alt, conf.level = CL, correct=FALSE)  
  pt
  results<-c(Z,p,pt$conf.int)
  return(results)
}

Z.prop.test(p1,n1,p2,n2,alt,CL)




#Problem 2



f<-file.choose()
d <- read_csv(f, col_names = TRUE)
head(d)
names(d)
d<- d[!(is.na(d$Brain_Size_Species_Mean)),]
d <- d[!(is.na(d$MaxLongevity_m)),]

BS<- d$Brain_Size_Species_Mean
ML<- d$MaxLongevity_m
beta1<- cor(ML,BS) * (sd(ML)/sd(BS))
beta1
beta0<- mean(ML) - beta1 * mean(BS)
beta0
a<-lm(MaxLongevity_m ~ Brain_Size_Species_Mean, data=d)
a
plot1<- ggplot(data = d, aes(x=Brain_Size_Species_Mean, y=MaxLongevity_m, label="y= 1.218x + 248.952"))+ geom_point()+ geom_smooth(method = lm, formula = y~x)+ geom_text(x = 200, y = 850, label= "y= 1.218x + 248.952")
plot1

pe_beta1<- predict(a,newdata = data.frame(Brain_Size_Species_Mean=beta1), interval = "confidence", level=0.90)
pe_beta1
beta1==0

ci <- predict(a, newdata = data.frame(Brain_Size_Species_Mean = d$Brain_Size_Species_Mean), interval = "confidence", level = 0.90) 
head(ci)
df1<- cbind(BS, ML, ci)
df1<- as.data.frame(df1)
head(df1)
plot1<-ggplot(data = df1, aes(x=BS, y=ML))+geom_line(aes(x=BS,y=fit),colour="black") + geom_line(aes(x=BS,y=lwr),colour="blue") + geom_line(aes(x=BS,y=upr), colour="blue")
pi<-predict(a,newdata = data.frame(Brain_Size_Species_Mean = d$Brain_Size_Species_Mean), interval = "prediction", level = 0.90) 
head(pi)
df1<- cbind(df1,pi)
names(df1)<- c("BS", "ML","CIfit", "CIlwr", "CIupr", "PIfit", "PIlwr","PIupr")
head(df1)
plot1<- plot1 + geom_line(data = df1, aes(x=BS, y=PIlwr), colour="red") + geom_line(data=df1, aes(x=BS, y=PIupr), colour= "red")+ geom_text(x = 200, y = 850, label= "y= 1.218x + 248.952")
plot1


pi800<- predict(a,newdata = data.frame(Brain_Size_Species_Mean=800), interval = "prediction", level=0.90)
pi800
#No, because most of the brain sizes are under 200 grams. Our data/graph may not accurately depict the longevity of species when the brain size is over a certain weight, or until there is more data for species.


#Log Problem Set

log_BS<- log(d$Brain_Size_Species_Mean)
log_ML<- log(d$MaxLongevity_m)
beta1<- cor(log_BS,log_ML) * (sd(log_ML)/sd(log_BS))
beta1
beta0<- mean(log_ML) - (beta1 * mean(log_BS))
beta0
b<-lm(log_ML ~ log_BS, data=d)
b
plot2<-ggplot(data=d,aes(x=log_BS, y=log_ML))+ geom_point()+geom_smooth(method = lm, formula = y~x)+geom_text(x = 1.5, y = 6.25, label= "y = 0.234x + 4.879")
plot2

pe2_beta1<- predict(b,newdata = data.frame(log_BS=beta1), interval = "confidence", level=0.90)
pe2_beta1
beta1==0

ci2 <- predict(b, newdata = data.frame(logBrain_Size_Species_Mean = log_BS), interval = "confidence", level = 0.90) 
head(ci2)
df2<- cbind(log_BS, log_ML, ci2)
df2<- as.data.frame(df2)
head(df2)
plot2<-ggplot(data = df2, aes(x=log_BS, y=log_ML))+geom_point()+geom_line(aes(x=log_BS,y=fit),colour="black") + geom_line(aes(x=log_BS,y=lwr),colour="blue") + geom_line(aes(x=log_BS,y=upr), colour="blue")
plot2
pi<-predict(b,newdata = data.frame(logBrain_Size_Species_Mean = log_BS), interval = "prediction", level = 0.90) 
head(pi)
df2<- cbind(df2,pi)
names(df2)<- c("log_BS", "log_ML","CIfit", "CIlwr", "CIupr", "PIfit", "PIlwr","PIupr")
head(df2)
plot2<- plot2 + geom_line(data = df2, aes(x=log_BS, y=PIlwr), colour="red") + geom_line(data=df2, aes(x=log_BS, y=PIupr), colour= "red")+geom_text(x = 1.5, y = 6.25, label= "y = 0.234x + 4.879")
plot2

pi2_800<- predict(b,newdata = data.frame(log_BS=log(800)), interval = "prediction", level=0.90)
pi2_800
#I trust this model to predict observations more accurately because there are at least a few values that we can see directly that are clode to our estimates, whereas for the previous model we had no x-value even near our estimate.




#When looking at the two models, I would say the log one is better because it is more evenly spread than model 1. Also, our point estimates showed that it may only be a good estimator for certain brain weights, whereas the log model at least had a couple of points near our estimator.








