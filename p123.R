#reading the csv file
ap = read.csv("C:/UC/Flex 3/Data Analysis Methods/Project/Admission_Predict.csv")
names(ap)
head(ap)
pairs(ap,pch=20)
#******************************************************************

#subsetting to remove serial number from the data set
amod <- subset(ap, select = -c(Serial_No.))
head(amod)

#******************************************************************

#considering all variables for regression
summary(lm(Chance_of_Admit ~ GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research,data=amod))

#*********************************************************************

#ANNOVA or partial f-test
model3=lm(Chance_of_Admit ~ GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research,data=amod)
model4=lm(Chance_of_Admit ~ SOP + University_Rating ,data=amod)
anova(model3,model4)
# note that p value < 2.2e-16 so we reject null hypothesis and conclude all slopes are non zero

#*********************************************************************************************

#Standardized Regression Coefficients to identify most influential coefficient 

#astd <- subset(amod, select = -c(Research))
#head(astd)
astd_unit_normal=as.data.frame(apply(amod[1:7],2,function(x){(x-mean(x))/sd(x)}))
astd_unit_normal$Chance_of_Admit = amod$Chance_of_Admit
#astd_unit_normal$Research <- amod$Research
head(astd_unit_normal)

# redo regression
model1_unit_normal <- lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + LOR +University_Rating + SOP + CGPA + Research , data=astd_unit_normal)
# obtain standardized regression coefficients
model1_unit_normal
summary(model1_unit_normal)

model5=lm(Chance_of_Admit ~ SOP + University_Rating ,data=astd_unit_normal)
anova(model1_unit_normal,model5)

#criterion for variable selection
install.packages("leaps")
library(leaps)

#R^2 method
amod_allmodels_r2 <- leaps( x=astd_unit_normal[,-8], y=astd_unit_normal[,8], method="r2")
amod_allmodels_r2
cbind(amod_allmodels_r2$which, Rsq = amod_allmodels_r2$r2)

#adjusted R^2 method

amod_allmodels_r2adj <- leaps( x=astd_unit_normal[,-8], y=astd_unit_normal[,8], method="adjr2")
amod_allmodels_r2adj
cbind(amod_allmodels_r2adj$which, adjRsq = amod_allmodels_r2adj$adjr2)

#5 1 1 0 0 1 1 1 0.8188449
#6 1 1 1 0 1 1 1 0.8196889

###################################################################################
#AIC and BIc
amod_allmodels=regsubsets(Chance_of_Admit~GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research, data=astd_unit_normal, nbest=4)
summary(amod_allmodels)
plot(amod_allmodels, scale="bic")
plot(amod_allmodels, scale="adjr2")

#***************************************************************************

# forward selection using F
add1(lm(Chance_of_Admit~1,data=astd_unit_normal), Chance_of_Admit~GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research, test="F")

#considering CGPA with largest F value
add1(lm(Chance_of_Admit~CGPA,data=astd_unit_normal), Chance_of_Admit~GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research, test="F")

#considering GRE_SCORE with largest F value
add1(lm(Chance_of_Admit~CGPA+GRE_Score,data=astd_unit_normal), Chance_of_Admit~GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research, test="F")

#considering LOR with largest F value
add1(lm(Chance_of_Admit~CGPA+GRE_Score+LOR,data=astd_unit_normal), Chance_of_Admit~GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research, test="F")

#considering Research with largest F value
add1(lm(Chance_of_Admit~CGPA+GRE_Score+LOR+Research,data=astd_unit_normal), Chance_of_Admit~GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research, test="F")

#considering TOEFL_Score with largest F value
add1(lm(Chance_of_Admit~CGPA+GRE_Score+LOR+Research+TOEFL_Score,data=astd_unit_normal), Chance_of_Admit~GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research, test="F")

#****************************************************************************************

#backward selection 
drop1(lm(Chance_of_Admit~GRE_Score+TOEFL_Score+University_Rating+SOP+LOR+CGPA+Research,data=astd_unit_normal), test="F")

#dropping SOP
drop1(lm(Chance_of_Admit~GRE_Score+TOEFL_Score+University_Rating+LOR+CGPA+Research,data=astd_unit_normal), test="F")

#dropping University_Rating
drop1(lm(Chance_of_Admit~GRE_Score+TOEFL_Score+LOR+CGPA+Research,data=astd_unit_normal), test="F")

#*****************************************************************************************

# Stepwise regression using AIC
null=lm(Chance_of_Admit~1, data=astd_unit_normal)
full=lm(Chance_of_Admit~., data=astd_unit_normal)
step(null, scope=list(lower=null, upper=full), direction="forward") #forward
#AIC forward selection result:CGPA + GRE_Score + LOR + Research + TOEFL_Score #AIC = -848.24

step(full, scope=list(lower=null, upper=full), direction="backward") #backward
#AIC backward selection result:GRE_Score + TOEFL_Score + University_Rating + LOR + CGPA + Research

#*****************************************************************************************

#AIC both direction
step(full, scope=list(lower=null, upper=full), direction="both")
#AIC forward and backward selection result:GRE_Score + TOEFL_Score + University_Rating + LOR + CGPA + Research

anew <- subset(amod, select = -c(SOP,University_Rating))
anew_unit_normal <- subset(astd_unit_normal, select = -c(SOP,University_Rating))


model11 <- lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + LOR + CGPA + Research, data=anew)
model11_unit_normal <- lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + LOR + CGPA + Research, data=anew_unit_normal)
summary(model11)

#model validation

# obtain standardized residuals
standardized_res=model11_unit_normal$residuals/summary(model11_unit_normal)$sigma
standardized_res

# generate QQ plot
qqnorm(model11_unit_normal$residuals)
qqline(model11_unit_normal$residuals)

head(anew)

# generate residual plot, NumberofCases vs residuals
plot(anew$GRE_Score,model11_unit_normal$residuals,pch=20) #double bow
abline(h=0,col="grey")

plot(anew$TOEFL_Score,model11_unit_normal$residuals,pch=20) #double bow
abline(h=0,col="grey")

plot(anew$LOR,model11_unit_normal$residuals,pch=20) #double bow
abline(h=0,col="grey")

plot(anew$CGPA,model11_unit_normal$residuals,pch=20) #double bow
abline(h=0,col="grey")

plot(anew$Chance_of_Admit,model11_unit_normal$residuals,pch=20) #double bow
abline(h=0,col="grey")

install.packages("MASS")
library(MASS)
boxcox(model11)

#***************************************************************************************

#checking for interaction term
afinal_unit_normal <-  subset(anew_unit_normal, select = -c(Research))
afinal_unit_normal$Research <- anew$Research
afinal_unit_normal$Research[afinal_unit_normal$Research==0]<- 'no'
afinal_unit_normal$Research[afinal_unit_normal$Research==1]<- 'yes'
head(afinal_unit_normal)
model1a <- lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + LOR + CGPA + Research + Research:GRE_Score + Research:TOEFL_Score + Research:LOR + Research:CGPA,data=afinal_unit_normal)
model_indicator <- lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + LOR + CGPA + Research ,data=afinal_unit_normal)
#summary(model1a)
#*************************************************************************************

#model22 <- lm(Chance_of_Admit ~ GRE_Score + TOEFL_Score + CGPA,data = astd_unit_normal)
#summary(model22)

#Ridge Regression

install.packages("MASS")
install.packages("car")
library(MASS)
library(car)
vif(model_indicator)


MSRes=summary(model_indicator)$sigma^2
MSRes
# obtain standardized residuals
standardized_res=model1$residuals/summary(model1)$sigma
standardized_res

#######################################################################

#cross validation of the selected model

install.packages("DAAG")
library(DAAG)
KCV=cv.lm(data=anew_unit_normal, model_indicator, m=5, seed=123)

#mean square prediction error

head(anew_unit_normal)
nrow(anew_unit_normal)

# MSPE can be alternatively calculated as
sum((anew_unit_normal$Chance_of_Admit-KCV$cvpred)^2)/500 #0.00366


# PRESS can be calculated as 
sum((anew_unit_normal$Chance_of_Admit - KCV$cvpred)^2) #1.83

# Prediction R squared can be calculated as
1-sum((anew_unit_normal$Chance_of_Admit-KCV$cvpred)^2)/sum((anew_unit_normal$Chance_of_Admit-mean(anew_unit_normal$Chance_of_Admit))^2)

summary(model_indicator)$r.squared

#LOOCV method
anew_unit_normal$Chance_of_Admit[9] # 0.5
model_indicator$fitted.values[9] # 0.555 
KCV$cvpred[9] # 0.554

LOOCV=cv.lm(data=anew_unit_normal, model_indicator, m=500,seed=123)

# MSPE can be alternatively calculated as
sum((anew_unit_normal$Chance_of_Admit-LOOCV$cvpred)^2)/500 #0.00365

# PRESS can be calculated as 
sum((anew_unit_normal$Chance_of_Admit-LOOCV$cvpred)^2) #1.83

# Prediction R squared can be calculated as
1-sum((anew_unit_normal$Chance_of_Admit-LOOCV$cvpred)^2)/sum((anew_unit_normal$Chance_of_Admit-mean(anew_unit_normal$Chance_of_Admit))^2) #0.816

summary(model_indicator)$adj.r.squared #0.819

##########################################################################

#performing model validation on our intial model
#model is model3
#data is amod

#k-cross validation

head(amod)
KCV=cv.lm(data=amod, model3, m=3, seed=123)

# MSPE can be alternatively calculated as
sum((amod$Chance_of_Admit-KCV$cvpred)^2)/500 #0.00369

# PRESS can be calculated as 
sum((amod$Chance_of_Admit - KCV$cvpred)^2) #1.84

# Prediction R squared can be calculated as
1-sum((amod$Chance_of_Admit-KCV$cvpred)^2)/sum((amod$Chance_of_Admit-mean(amod$Chance_of_Admit))^2) #0.815

summary(model3)$r.squared #0.822

#LOOCV method
amod$Chance_of_Admit[9] # 0.5
model3$fitted.values[9] # 0.55
KCV$cvpred[9] # 0.547

LOOCV=cv.lm(data=amod, model3, m=500,seed=123)

# MSPE can be alternatively calculated as
sum((amod$Chance_of_Admit-LOOCV$cvpred)^2)/500 #0.00365

# PRESS can be calculated as 
sum((anew_unit_normal$Chance_of_Admit-LOOCV$cvpred)^2) #1.83

# Prediction R squared can be calculated as
1-sum((amod$Chance_of_Admit-LOOCV$cvpred)^2)/sum((amod$Chance_of_Admit-mean(amod$Chance_of_Admit))^2) #0.816

summary(model_indicator)$adj.r.squared #0.819






