# PREDICT 422 Practical Machine Learning

# OBJECTIVE: A charitable organization wishes to develop a machine learning
# model to improve the cost-effectiveness of their direct marketing campaigns
# to previous donors.

# 1) Develop a classification model using data from the most recent campaign that
# can effectively capture likely donors so that the expected net profit is maximized.

# 2) Develop a prediction model to predict donation amounts for donors - the data
# for this will consist of the records for donors only.

# load the data
require(moments)
require(rockchalk)
require(flux)
require(ggplot2)
require(gridExtra)
library(grid)
charity <- read.csv(file.choose()) # load the "charity.csv" file
dim(charity)
str(charity)
head(charity)
summary(charity)
attach(charity)

#check for duplicate rec
anyDuplicated(charity$ID)

#number of test, train and validate
summary(part)

Reg1Count = sum(reg1==1)
Reg2Count  = sum(reg2==1)
Reg3Count = sum(reg3==1)
Reg4Count = sum(reg4==1)
Reg5Count = dim(charity)[1] - (Reg1Count + Reg2Count + Reg3Count + Reg4Count )
Reg1Count
Reg2Count
Reg3Count
Reg4Count
Reg5Count

round(cor(charity[sapply(charity, function(x) !is.factor(x))]),2)

##########################################
mosaicplot(donr ~ chld,
             main="Number of Children", shade=FALSE, color=TRUE,
             xlab="CHLD", ylab="DONR")


mosaicplot(donr ~ home,
           main="Home", shade=FALSE,color=TRUE,
           xlab="HOME", ylab="DONR")

mosaicplot(donr ~ hinc,
           main="Household income", shade=FALSE,color=TRUE,
           xlab="HINC", ylab="DONR")

mosaicplot(donr ~ genf,
           main="Gender", shade=FALSE,color=TRUE,
           xlab="GENF", ylab="donr")


mosaicplot(donr ~ wrat,
           main="Wealth Rating", shade=FALSE,color=TRUE,
           xlab="WRAT", ylab="DONR")


####################################################################################
plot(density(hinc))
plot(density(wrat))
plot(density(avhv))
plot(density(log(avhv)))
plot(density(incm))
plot(density(inca))
plot(density(log(inca)))

plot(density(plow))
plot(density(log(plow)))
plot(density((plow)^(1/3)))

plot(density(npro))

plot(density(tgif))
plot(density(log(tgif)) )

plot(density(lgif))
plot(density(log(lgif)) )


plot(density(rgif))
plot(density(log(rgif)) )
plot(density(tdon))
plot(density(log(tdon)))

plot(density(tlag))
plot(density(log(tlag)) )
plot(density(agif))
plot(density(log(tlag)) )


#######################################################################

boxplot(avhv ~ donr,
        main="Average Home Value",
        ylab="AVHV",xlab="DONR")

boxplot(incm ~ donr,
        main="Median Family Income",
        ylab="INCM",xlab="DONR")

boxplot(inca ~ donr,
        main="Average Family Income",
        ylab="INCA",xlab="DONR")

boxplot(plow ~ donr,
        main="Percent categorized as Low Income Household",
        ylab="PLOW",xlab="DONR")

boxplot(npro ~ donr,
        main="Lifetime number of promotions received to date",
        ylab="NPRO",xlab="DONR")

boxplot(tgif ~ donr,
        main="Dollar amount of lifetime gifts to date",
        ylab="TGIF",xlab="DONR")
boxplot(lgif ~ donr,
        main="Dollar amount of largest gift to date",
        ylab="LGIF",xlab="DONR")
boxplot(rgif ~ donr,
        main="Dollar amount of most recent gift",
        ylab="RGIF",xlab="DONR")
boxplot(tdon ~ donr,
        main="Number of months since last donation",
        ylab="TDON",xlab="DONR")

boxplot(tlag ~ donr,
        main="Number of months between first and second gift",
        ylab="TLAG",xlab="DONR")

boxplot(agif ~ donr,
        main="Average dollar amount of gifts to date",
        ylab="AGIF",xlab="DONR")


#Child
ggplot(data = charity,
       aes(x = chld,
           y = home))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = chld,
           y = hinc))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = chld,
           y = wrat))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = chld,
           y = avhv))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = chld,
           y = incm))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = chld,
           y = inca))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = chld,
           y = plow))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = chld,
           y = npro))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = chld,
           y = tgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = chld,
           y = lgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = chld,
           y = rgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = chld,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = chld,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = chld,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


####HOME
ggplot(data = charity,
       aes(x = home,
           y = hinc))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = home,
           y = wrat))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = home,
           y = avhv))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = home,
           y = incm))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = home,
           y = inca))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")



ggplot(data = charity,
       aes(x = home,
           y = plow))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = home,
           y = npro))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = home,
           y = tgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = home,
           y = lgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = home,
           y = rgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = home,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = home,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = home,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

##########HINC

ggplot(data = charity,
       aes(x = hinc,
           y = wrat))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = hinc,
           y = avhv))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = hinc,
           y = incm))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = hinc,
           y = plow))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = hinc,
           y = npro))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = hinc,
           y = tgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = hinc,
           y = lgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = hinc,
           y = rgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = hinc,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = hinc,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = hinc,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

#####WRAT

ggplot(data = charity,
       aes(x = wrat,
           y = avhv))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = wrat,
           y = incm))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")





ggplot(data = charity,
       aes(x = wrat,
           y = plow))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = wrat,
           y = npro))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = wrat,
           y = tgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = wrat,
           y = lgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = wrat,
           y = rgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = wrat,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = wrat,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = wrat,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

#####INCM




ggplot(data = charity,
       aes(x = incm,
           y = plow))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = incm,
           y = npro))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = incm,
           y = tgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = incm,
           y = lgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = incm,
           y = rgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = incm,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = incm,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = incm,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

#####PLOW


ggplot(data = charity,
       aes(x = plow,
           y = npro))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = plow,
           y = tgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = plow,
           y = lgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = plow,
           y = rgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = plow,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = plow,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = plow,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

#####NPRO
ggplot(data = charity,
       aes(x = npro,
           y = tgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


ggplot(data = charity,
       aes(x = npro,
           y = lgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = npro,
           y = rgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = npro,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = npro,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = npro,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

#####TGIF

ggplot(data = charity,
       aes(x = tgif,
           y = lgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = tgif,
           y = rgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = tgif,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = tgif,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = tgif,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

#####LGIF



ggplot(data = charity,
       aes(x = lgif,
           y = rgif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")
ggplot(data = charity,
       aes(x = lgif,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = lgif,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = lgif,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

#####RGIF


ggplot(data = charity,
       aes(x = rgif,
           y = tdon))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = rgif,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = rgif,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

#####TDON



ggplot(data = charity,
       aes(x = tdon,
           y = tlag))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

ggplot(data = charity,
       aes(x = tdon,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

#####TLAG



ggplot(data = charity,
       aes(x = tlag,
           y = agif))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")

###########AGIF

ggplot(data = charity,
       aes(x = agif,
           y = inca))+ geom_point(aes(color = donr),
                                  size = 2)+ggtitle("Figure")


######



hist(charity.t$avhv,
     main="Average Home Value ",
     xlab="AVHV",
     border="4")
hist(charity.t$chld,
     main="Number of Children",
     xlab="child",
     border="4")
hist(charity.t$hinc,
     main="household income",
     xlab="hinc",
     border="4")
hist(charity.t$wrat,
     main="wealth rating",
     xlab="wrat",
     border="4")

hist(charity.t$incm,
     main="median family income",
     xlab="incm",
     border="4")

hist(charity.t$inca,
     main="average family income",
     xlab="inca",
     border="4")

hist(charity.t$plow,
     main="percent low income",
     xlab="plow",
     border="4")

hist(charity.t$npro,
     main="Lifetime number of promotions received to date",
     xlab="npro",
     border="4")

hist(charity.t$tgif,
     main="Dollar amount of lifetime gifts to date",
     xlab="tgif",
     border="4")

hist(charity.t$lgif,
     main="Dollar amount of largest gift to date",
     xlab="lgif",
     border="4")

hist(charity.t$rgif,
     main="Dollar amount of most recent gift",
     xlab="rgif",
     border="4")

hist(charity.t$tdon,
     main="Number of months since last donation",
     xlab="tdon",
     border="4")

hist(charity.t$tlag,
     main="Number of months between first and second gift",
     xlab="tlag",
     border="4")

hist(charity.t$agif,
     main="Average dollar amount of gifts to date",
     xlab="agif",
     border="4")


avhv_mean<-aggregate(avhv~donr, data=charity,mean)[2]
incm_mean<-aggregate(incm~donr, data=charity,mean)[2]
inca_mean<-aggregate(inca~donr, data=charity,mean)[2]
plow_mean<-aggregate(plow~donr, data=charity,mean)[2]
npro_mean<-aggregate(npro~donr, data=charity,mean)[2]
tgif_mean<-aggregate(tgif~donr, data=charity,mean)[2]
lgif_mean<-aggregate(lgif~donr, data=charity,mean)[2]
rgif_mean<-aggregate(rgif~donr, data=charity,mean)[2]
tdon_mean<-aggregate(tdon~donr, data=charity,mean)[2]
tlag_mean<-aggregate(tlag~donr, data=charity,mean)[2]
agif_mean<-aggregate(agif~donr, data=charity,mean)[2]


mean_cont<-cbind(round(avhv_mean,2),
round(incm_mean,2),
round(inca_mean,2),
round(plow_mean,2),
round(npro_mean,2),
round(tgif_mean,2),
round(lgif_mean,2),
round(rgif_mean,2),
round(tdon_mean,2),
round(tlag_mean,2),
round(agif_mean,2))

rownames(mean_cont)<-c("non-donor","donor")
mean_cont
######################################
# predictor transformations

charity.t <- charity
charity.t$avhv <- log(charity.t$avhv)
charity.t$inca <- log(charity.t$inca)
charity.t$tgif <- log(charity.t$tgif)
charity.t$plow <- charity.t$plow ^ (1/3)
#charity.t$plow <- log(charity.t$plow)
#charity.t$tgif <- log(charity.t$tgif)
#charity.t$lgif <- charity.t$lgif ^ (1/3)
charity.t$lgif <- log(charity.t$lgif)
charity.t$rgif <- log(charity.t$rgif)
charity.t$agif <- log(charity.t$agif)
#charity.t$tdon <- log(charity.t$tdon)
charity.t$tlag <- log(charity.t$tlag)

#categorical variables
charity.t$chld[charity.t$chld==0] <- 0
charity.t$chld[charity.t$chld==1] <- 1
charity.t$chld[charity.t$chld==2] <- 2
charity.t$chld[charity.t$chld==3] <- 3
charity.t$chld[charity.t$chld > 3] <- 4

charity.t$hinc[charity.t$hinc==1] <- 0
charity.t$hinc[charity.t$hinc==2] <- 0
charity.t$hinc[charity.t$hinc==6] <- 0
charity.t$hinc[charity.t$hinc==7] <- 0
charity.t$hinc[charity.t$hinc==3] <- 1
charity.t$hinc[charity.t$hinc==5] <- 1
charity.t$hinc[charity.t$hinc==4] <- 2

charity.t$wrat[charity.t$wrat==0] <- 0
charity.t$wrat[charity.t$wrat==1] <- 1
charity.t$wrat[charity.t$wrat==2] <- 2
charity.t$wrat[charity.t$wrat==3] <- 2
charity.t$wrat[charity.t$wrat==4] <- 2
charity.t$wrat[charity.t$wrat==5] <- 3
charity.t$wrat[charity.t$wrat==6] <- 4
charity.t$wrat[charity.t$wrat==7] <- 4
charity.t$wrat[charity.t$wrat==8] <- 5
charity.t$wrat[charity.t$wrat==9] <- 5

# add further transformations if desired
# for example, some statistical methods can struggle when predictors are highly skewed

# set up data for analysis

data.train <- charity.t[charity$part=="train",]
x.train <- data.train[,2:21] #all the variables except ID and response
c.train <- data.train[,22] # donnar variable
n.train.c <- length(c.train) # 3984
y.train <- data.train[c.train==1,23] # damt for observations with donr=1
n.train.y <- length(y.train) # 1995

data.valid <- charity.t[charity$part=="valid",]
x.valid <- data.valid[,2:21]
c.valid <- data.valid[,22] # donr
n.valid.c <- length(c.valid) # 2018
y.valid <- data.valid[c.valid==1,23] # damt for observations with donr=1
n.valid.y <- length(y.valid) # 999


data.test <- charity.t[charity$part=="test",]
n.test <- dim(data.test)[1] # 2007
x.test <- data.test[,2:21]

x.train.mean <- apply(x.train, 2, mean)
x.train.sd <- apply(x.train, 2, sd)
x.train.std <- t((t(x.train)-x.train.mean)/x.train.sd) # standardize to have zero mean and unit sd
apply(x.train.std, 2, mean) # check zero mean
apply(x.train.std, 2, sd) # check unit sd
data.train.std.c <- data.frame(x.train.std, donr=c.train) # to classify donr
data.train.std.y <- data.frame(x.train.std[c.train==1,], damt=y.train) # to predict damt when donr=1

x.valid.std <- t((t(x.valid)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.valid.std.c <- data.frame(x.valid.std, donr=c.valid) # to classify donr
data.valid.std.y <- data.frame(x.valid.std[c.valid==1,], damt=y.valid) # to predict damt when donr=1

x.test.std <- t((t(x.test)-x.train.mean)/x.train.sd) # standardize using training mean and sd
data.test.std <- data.frame(x.test.std)

# scale the data for knn
stand_x.train<-scale(x.train)
stand_x.valid<-scale(x.valid)



##### CLASSIFICATION MODELING ######
############################################################################

# linear discriminant analysis

library(MASS)

model.lda1 <- lda(donr ~ reg1 + reg2 + reg3 + reg4 + home + chld + hinc+genf + wrat +
                    avhv + incm + inca + plow + npro + tgif + lgif + rgif + tdon + tlag + agif,
                  data=data.train.std.c) #1331 11707 err: 0.1259

summary(model.lda1)
round(coef(model.lda1),4)
post.valid.lda1 <- predict(model.lda1, data.valid.std.c)$posterior[,2] # n.valid.c post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.lda1 <- cumsum(14.5*c.valid[order(post.valid.lda1, decreasing=T)]-2)
n.mail.valid <- which.max(profit.lda1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.lda1)) # report number of mailings and maximum profit
# 1279 11,710

cutoff.lda1 <- sort(post.valid.lda1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.lda1 <- ifelse(post.valid.lda1>cutoff.lda1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.lda1, c.valid) # classification table
#               c.valid
#chat.valid.lda1   0   1
#              0 724  15
#              1 295 984
# check n.mail.valid = 295+984 = 1279
# check profit = 14.5*984-2*1279 = 11710

glm.pred.valid = rep("0", 2018)
glm.pred.valid[post.valid.lda1 > .5] = "1"
table(glm.pred.valid, c.valid)
LdaError <- mean(glm.pred.valid != c.valid)
LdaError

###########################################################################################

# logistic regression
model.log1 <- glm(donr ~ . ,
                 data.train.std.c, family=binomial("logit")) # 1335 11772 err 0.1253717

#model.log1 <- glm(donr ~ reg1+reg2+reg3+reg4+home+chld+hinc+wrat+incm+tgif+tdon+tlag ,
                  data.train.std.c, family=binomial("logit")) # 1330 11768 error 0.1249

summary(model.log1)
post.valid.log1 <- predict(model.log1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.log1 <- cumsum(14.5*c.valid[order(post.valid.log1, decreasing=T)]-2)

n.mail.valid <- which.max(profit.log1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.log1)) # report number of mailings and maximum profit
# 1291.0 11642.5

cutoff.log1 <- sort(post.valid.log1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.log1 <- ifelse(post.valid.log1>cutoff.log1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.log1, c.valid) # classification table

# select model.log1 since it has maximum profit in the validation sample

glm.pred.valid = rep("0", 2018)
glm.pred.valid[post.valid.log1 > .5] = "1"
table(glm.pred.valid, c.valid)
LogisticError <- mean(glm.pred.valid != c.valid)
LogisticError

########################################################################
#GAM model

library(gam)
library(splines)


gam.model1 <- gam(I(donr==1) ~ reg1 + reg2 +reg3+reg4+ home + chld + hinc +plow+
                    wrat + tdon + ns(tlag) + npro*tgif + avhv*incm,
                  family = binomial, data = data.valid.std.c) #1278 11785 err 0.1219


summary(gam.model1)
coef(gam.model1)
post.valid.gam1 <- predict(gam.model1, data.valid.std.c, type="response") # n.valid post probs

# calculate ordered profit function using average donation = $14.50 and mailing cost = $2

profit.gam1 <- cumsum(14.5*c.valid[order(post.valid.gam1, decreasing=T)]-2)

n.mail.valid <- which.max(profit.gam1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.gam1)) # report number of mailings and maximum profit

cutoff.gam1 <- sort(post.valid.gam1, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.gam1 <- ifelse(post.valid.gam1>cutoff.gam1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.gam1, c.valid) # classification table


glm.pred.valid = rep("0", 2018)
glm.pred.valid[post.valid.gam1 > .5] = "1"
table(glm.pred.valid, c.valid)
GamError <- mean(glm.pred.valid != c.valid)
GamError

############################################################################
library(gbm)
set.seed(1)

boost.Donr = gbm(donr ~ reg1+reg2+reg3+reg4+home+chld+hinc+wrat+incm+tgif+tdon+tlag, data = data.train.std.c, distribution = "bernoulli",
                 n.trees = 3500, shrinkage=0.005, interaction.depth = 4) # 1205 11945 err: 0.10


summary(boost.Donr)

boost.probs = predict.gbm(boost.Donr, newdata = data.valid.std.c,
                          n.trees = 3500)

profit.gbm1 <- cumsum(14.5*c.valid[order(boost.probs, decreasing=T)]-2)

n.mail.valid <- which.max(profit.gbm1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.gbm1)) # report number of mailings and maximum profit


cutoff.gbm1 <- sort(boost.probs, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.gbm1 <- ifelse(boost.probs>cutoff.gbm1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.gbm1, c.valid) # classification table

boost.pred = rep("0", 2018)
boost.pred[boost.probs > .5] = "1"
table(boost.pred , c.valid)
boostError <- mean(boost.pred != c.valid)
boostError

#############################################################################
#Random Forest
library (randomForest)
set.seed (1)
bag.donr =randomForest(donr~.,data=data.train.std.c ,mtry = 5,
                       importance =TRUE, type = "classification") 

  #1281 11793; err 0.1020813

rf.donr

importance(rf.donr)

varImpPlot(rf.donr)

predict.rf = predict(rf.donr, newdata =data.valid.std.c)
profit.rf1 <- cumsum(14.5*c.valid[order(predict.rf, decreasing=T)]-2)
n.mail.valid <- which.max(profit.rf1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.rf1)) # report number of mailings and maximum profit


cutoff.rf1 <- sort(predict.rf, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.rf1 <- ifelse(predict.rf>cutoff.rf1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.rf1, c.valid) # classification table

rf.pred = rep("0", 2018)
rf.pred[predict.rf > .5] = "1"
table(rf.pred , c.valid)
rfError <- mean(rf.pred != c.valid)
rfError

###########################################################################
data.train.std.c.pridictors <- data.train.std.c[,names(data.train.std.c)!="donr"]
rf.cv.results <- rfcv(data.train.std.c.pridictors, as.factor(data.train.std.c$donr),cv.fold=10)
with(rf.cv.results, plot(n.var, error.cv, main="RF CV error vs num of predic", xlab="num predic",
                         ylab="CV erro", type = "b", lwd=5, col="red"))

random.forrest.error <- rbind(rf.cv.results$n.var, rf.cv.results$error.cv)
rownames(random.forrest.error) <- c("Num of predict", "Random forest error")
random.forrest.error
############################################################################
#Decision Tree
library(tree)
tree.data.train = data.train.std.c
tree.data.train$donr = as.factor(tree.data.train$donr)
tree.data.valid = data.valid.std.c
tree.data.valid$donr = as.factor(tree.data.valid$donr)


tree.Donr = tree(donr~.,tree.data.train ) #1162 10958 err: 0.1496531
#1162 10958; err = 0.1496531

summary(tree.Donr)
text(tree.Donr, pretty=0)
tree.pred=predict(tree.Donr, tree.data.valid, type="class")
table(tree.pred, c.valid)


#To prune our tree we have to run cross-validation
set.seed(3)
cv_trees = cv.tree(tree.Donr,FUN=prune.misclass)
names(cv_trees)
par(mfrow=c(1,1))
plot(cv_trees$size, cv_trees$dev, type="b")
plot(cv_trees$k, cv_trees$dev, type="b")

#prune
set.seed(1)
prune.Donr=prune.misclass(tree.Donr, best=12)
plot(prune.Donr)
text(prune.Donr,pretty=0)
tree.pred=predict(prune.Donr,tree.data.valid, type="class")
mean(tree.pred!=c.valid)
mean(tree.pred==c.valid)
table(tree.pred, c.valid)
AUC_LogReg<- roc(as.numeric(c.valid), as.numeric(as.matrix((tree.pred))))$auc
AUC_LogReg

profit.tree1 <- cumsum(14.5*c.valid[order(tree.pred, decreasing=T)]-2)
n.mail.valid <- which.max(profit.tree1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.tree1)) # report number of mailings and maximum profit
#1162 10958

cutoff.tree1 <- sort(tree.pred, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.tree1 <- ifelse(tree.pred>cutoff.tree1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.tree1, c.valid) # classification table

table(tree.pred , c.valid)
treeError <- mean(tree.pred != c.valid)
treeError #0.1497

#Support Vector Machine*********************************************
library(e1071)
svmfit<-svm(donr~.,data=data.train.std.c,kernel="radial", gamma=1, cost=1)
summary(svmfit)

set.seed(1)
tune.out<-tune(svm,donr~.,data=data.train.std.c,kernel="radial",
               ranges=list(cost=c(0.1,1,10,100,1000),
                           gamma=c(0.5,1,2,3,4)))
summary(tune.out)
tune.out$best.model
svm.pred<-predict(tune.out$best.model,
                  newdata=data.valid.std.c)


profit.svm1 <- cumsum(14.5*c.valid[order(svm.pred, decreasing=T)]-2)

n.mail.valid <- which.max(profit.svm1) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.svm1)) # report number of mailings and maximum profit
#1250 11188

cutoff.svm1 <- sort(svm.pred, decreasing=T)[n.mail.valid+1] # set cutoff based on n.mail.valid
chat.valid.svm1 <- ifelse(svm.pred>cutoff.svm1, 1, 0) # mail to everyone above the cutoff
table(chat.valid.svm1, c.valid) # classification table
svmError <- mean(chat.valid.svm1 != c.valid)
svmError #0.1788

#KNN******************************************************************
library(class)

set.seed(1)
knn.pred<-knn(stand_x.train,stand_x.valid,c.train,k=50)#1290,11543; error 0.1690


profit.knn <- cumsum(14.5*c.valid[order(knn.pred, decreasing=T)]-2)
n.mail.valid <- which.max(profit.knn) # number of mailings that maximizes profits
c(n.mail.valid, max(profit.knn)) # report number of mailings and maximum profit
#1278 11437

table(knn.pred , c.valid)
knnError <- mean(knn.pred != c.valid)
knnError #0.1720

#################################################################################################
#Plot
par(mfrow=c(1,1))
plot(profit.gbm1, ylab="Profit", col=2, lwd=2, type="l", main="Profit by classification models")
points(profit.log1, col=3,lwd=1, type="l")
points(profit.tree1, col=4, lwd=1, type="l")
points(profit.rf1, col="dodgerblue", lwd=1,type="l")
points(profit.gam1, col="orange", lwd=1,type="l")
points(profit.svm1, col="seagreen", lwd=1,type="l")
points(profit.knn, col="chocolate", lwd=1,type="l")
legend("bottomright",lty=1,col=c("red","green","blue","dodgerblue","orange","seagreen","chocolate"),
       legend=c("XBoost","Logistic","Tree","RForest","GAM","SVM","KNN"),bty='n')

#**************************************************************************
library(data.table)
profit_values<-rbind(max(profit.lda1),
                   max(profit.log1),
                   max(profit.gam1),
                   max(profit.gbm1),
                   max(profit.rf1),
                   max(profit.tree1),
                   max(profit.svm1),
                   max(profit.knn))
class_error<-rbind(round(LdaError,4),
                   round(LogisticError,4),
                   round(GamError,4),
                   round(boostError,4),
                   round(rfError,4),
                   round(treeError,4),
                   round(svmError,4),
                   round(knnError,4))

criteria<-cbind(profit_values,class_error)
name_head<-c("LDA",
             "Logistc",
             "GAM",
             "XBoost",
             "RForest",
             "Tree",
             "SVM",
             "KNN")

rownames(criteria) <- name_head
colnames(criteria) <- c("Maximum Profit","Misclassification Rate")
criteria

#############################################################################
post.test <- predict(boost.Donr, data.test.std, type="response",n.trees=3500) # post probs for test data

# Oversampling adjustment for calculating number of mailings for test set

n.mail.valid <- which.max(profit.gbm1)
tr.rate <- .1 # typical response rate is .1
vr.rate <- .5 # whereas validation response rate is .5
adj.test.1 <- (n.mail.valid/n.valid.c)/(vr.rate/tr.rate) # adjustment for mail yes
adj.test.0 <- ((n.valid.c-n.mail.valid)/n.valid.c)/((1-vr.rate)/(1-tr.rate)) # adjustment for mail no
adj.test <- adj.test.1/(adj.test.1+adj.test.0) # scale into a proportion
n.mail.test <- round(n.test*adj.test, 0) # calculate number of mailings for test set

cutoff.test <- sort(post.test, decreasing=T)[n.mail.test+1] # set cutoff based on n.mail.test
chat.test <- ifelse(post.test>cutoff.test, 1, 0) # mail to everyone above the cutoff
table(chat.test)
#    0    1
# 1727  280


##### PREDICTION MODELING ###############################################

# Least squares regression

model.ls1 <- lm(damt ~ .,
                data.train.std.y)
par(mfrow=c(2,2))
plot(model.ls1)
summary(model.ls1)
pred.valid.ls1 <- predict(model.ls1, newdata = data.valid.std.y) # validation predictions
mean((y.valid - pred.valid.ls1)^2) # mean prediction error
#1.7245
sd((y.valid - pred.valid.ls1)^2)/sqrt(n.valid.y) # std error
# 0.1636

#BEST SUBSET MODEL**********************************************************************************************
library(leaps)
regfit.best<-regsubsets(damt ~ .,
                        data=data.train.std.y,nvmax=20)
reg.summary<-summary(regfit.best)
which.min(reg.summary$bic)
reg.summary$bic[11]

par(mfrow=c(1,2))
plot(reg.summary$bic,
     xlab="Number of Variables",
     ylab="BIC",
     type="b",
     main="Best Subset Selection by BIC")
points(which.min(reg.summary$bic),reg.summary$bic[which.min(reg.summary$bic)],col="red",cex=2,pch=20)
#*************************************************************
predict.regsubsets<-function(object, newdata, id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}
#************************************************************
coef_bestsubset<-coef(regfit.best,11)
coef_bestsubset
pred_bestsubset<-predict(regfit.best,data.valid.std.y,11)
mean((y.valid - pred_bestsubset)^2) #1.7203
sd((y.valid - pred_bestsubset)^2)/sqrt(n.valid.y) #0.1618


#RIDGE*******************************************************
library(glmnet)
x.train<-x.train.std[c.train==1,]
x.valid<-x.valid.std[c.valid==1,]


grid<-10^seq(10,-2,length=100)
ridge.mod<-glmnet(x.train,y.train,alpha=0, lambda=grid)
plot(ridge.mod,xvar="lambda",label=T)
set.seed(1306)
cv.out<-cv.glmnet(x.train,y.train,alpha=0)
plot(cv.out)
one_se_lam_ridge<-cv.out$lambda.1se
one_se_lam_ridge
coef_ridge<-predict(ridge.mod, type="coefficients",s=one_se_lam_ridge)[1:21,]
coef_ridge

ridge.pred<-predict(ridge.mod,s=one_se_lam_ridge,newx=x.valid)
mean((y.valid - ridge.pred)^2) #1.8351
sd((y.valid - ridge.pred)^2)/sqrt(n.valid.y) #0.1740

#LASSO******************************************************
lasso.mod<-glmnet(x.train,y.train,alpha=1, lambda=grid)
plot(lasso.mod)

set.seed(1)
cv.out<-cv.glmnet(x.train,y.train,alpha=1)
plot(cv.out)

one_se_lam_lasso<-cv.out$lambda.1se
one_se_lam_lasso
coef_lasso<-predict(lasso.mod, type="coefficients",s=one_se_lam_lasso)[1:21,]
coef_lasso_val<-coef_lasso[coef_lasso!=0]
coef_lasso_val
lasso.pred<-predict(lasso.mod,s=one_se_lam_lasso,newx=x.valid)
mean((y.valid - lasso.pred)^2) #1.8840
sd((y.valid - lasso.pred)^2)/sqrt(n.valid.y) #0.1714

#Random Forest***********************************************
rf.fit <- randomForest(damt~.,
                       data=data.train.std.y,
                       importance=TRUE,
                       ntree=3500)
imp<-importance(rf.fit)
varImpPlot(rf.fit, main="Variable Importance")
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
impvar
plot(rf.fit, col="red", lwd=3, main="MSE Error")
#Test Random Forest with Original variables**************************************************************
set.seed(1)
RF.pred<- predict(rf.fit, newdata=data.valid.std.y, type = "response")
mean((y.valid - RF.pred)^2) #1.7578
sd((y.valid - RF.pred)^2)/sqrt(n.valid.y) #0.1740


#Gradient Boosting*******************************************************

gbm.fit<- gbm(damt~.-avhv-inca-genf,
              data=data.train.std.y,
              n.trees=3500,
              shrinkage=0.005,
              distribution="gaussian",
              interaction.depth=2)

set.seed(1)
gbm.pred<- predict(gbm.fit, newdata=data.valid.std.y, type = "response",n.trees = 3500)
plot(gbm.fit)
mean((y.valid - gbm.pred)^2) #1.5335
sd((y.valid - gbm.pred)^2)/sqrt(n.valid.y) #0.1604


#GAM*******************************************************************
gam.reg <- gam(damt ~reg1+reg2+reg3+reg4+home+wrat+chld+hinc+s(incm,5)+s(npro,5)+s(tgif,7)+s(rgif,5)+tdon+poly(agif,3)+poly(tlag,3),
               data = data.train.std.y)
summary(gam.reg)
gam.pred<- predict(gam.reg, newdata=data.valid.std.y, type = "response")

mean((y.valid - gam.pred)^2) #1.7400
sd((y.valid - gam.pred)^2)/sqrt(n.valid.y) #0.1602


#*******************************************************************

yhat.test <- predict(gbm.fit, newdata = data.test.std,n.trees = 3500) # test predictions

# FINAL RESULTS

# Save final results for both classification and regression

length(chat.test) # check length = 2007
length(yhat.test) # check length = 2007
chat.test[1:10] # check this consists of 0s and 1s
yhat.test[1:10] # check this consists of plausible predictions of damt

ip <- data.frame(chat=chat.test, yhat=yhat.test) # data frame with two variables: chat and yhat
write.csv(ip, file="C:/MSPA/422_Machine_Learning/Projects/Group Project/Group_7_charity_test_ouput.csv", row.names=FALSE) 

# submit the csv file in Canvas for evaluation based on actual test donr and damt values

