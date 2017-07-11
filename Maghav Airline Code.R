#Analysis of Airline Ticket Pricing
#Name: Maghav Goyal
#EMail: maghavgoyal@gmail.com
#College: DTU

#Setting the working directory 
setwd("C:/Users/ANIL GOYAL/Desktop/New folder (2)/data sets")

#Reading the data and creating a data frame 
air.df<-read.csv(paste("AIRLINE.csv",sep=""),)

#Viewing the data frame 
View(air.df)

#Summarizing the data 
summary(air.df)

# Visualizing using plots
boxplot(air.df$PRICE_PREMIUM~air.df$AIRLINE)
boxplot(air.df$PRICE_ECONOMY~air.df$AIRLINE)
boxplot(air.df$PRICE_PREMIUM~air.df$MONTH)
boxplot(air.df$PRICE_ECONOMY~air.df$MONTH)

#Visualizing with PRICE_RELATIVE as Y vs Categorical variables
boxplot(air.df$PRICE_RELATIVE~air.df$MONTH) 
boxplot(air.df$PRICE_RELATIVE~air.df$INTERNATIONAL)                    # Huge differnce is observed
boxplot(air.df$PRICE_RELATIVE~air.df$AIRCRAFT)
boxplot(air.df$PRICE_RELATIVE~air.df$QUALITY)                          # As quality increases, relative price increases

#Converting other variables in categories and then visualizing bivariately


x0<-factor(air.df$FLIGHT_DURATION)                        #No pattern
boxplot(air.df$PRICE_RELATIVE~x0)

x<-factor(air.df$SEATS_ECONOMY)
boxplot(air.df$PRICE_RELATIVE~x)                        # For seats >180, the realtive price is mostly good.

x1<-factor(air.df$SEATS_PREMIUM) 
boxplot(air.df$PRICE_RELATIVE~x1)                     #No definite pattern

x3<-factor(air.df$PITCH_ECONOMY)
boxplot(air.df$PRICE_RELATIVE~x3)                   # as pitch increases, the relative price decreases

x2<-factor(air.df$PITCH_PREMIUM)
boxplot(air.df$PRICE_RELATIVE~x2)                  # as pitch increases, the relative price increases

boxplot(air.df$PRICE_RELATIVE~x2+x3)                # No definite pattern

x4<-factor(air.df$WIDTH_ECONOMY)                    # No inference
boxplot(air.df$PRICE_RELATIVE~x4)

x5<-factor(air.df$WIDTH_PREMIUM)              #Width increases, Relative price increases
boxplot(air.df$PRICE_RELATIVE~x5)

boxplot(air.df$PRICE_RELATIVE~x4+x5)          #Increases gradually

x6<-factor(air.df$N)                         # No definite pattern
boxplot(air.df$PRICE_RELATIVE~x6)

x7<-factor(air.df$LAMBDA)                    #No definite pattern
boxplot(air.df$PRICE_RELATIVE~x7)

#Scatterplot for non-categorical variables
library(car)
scatterplot(air.df$PRICE_RELATIVE,air.df$FLIGHT_DURATION)             # No pattern
scatterplot(air.df$PRICE_RELATIVE,air.df$SEATS_ECONOMY)              # As seats increase, the relative price decreases

#Pattern obtained by scatterplot vs (variable) and boxplot vs factor(variable) gives the same inferences

#VISUALIZING VIA GGVIS
library(ggvis)
ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~AIRLINE,data=air.df)
ggvis(~PRICE_PREMIUM,~WIDTH_PREMIUM,fill=~PRICE_RELATIVE,data=air.df)
#Interaction between the price quantities
ggvis(~PRICE_ECONOMY,~PRICE_RELATIVE,fill=~PRICE_PREMIUM,data=air.df)



#CORRGRAM
library(corrgram)
corrgram(air.df, order=TRUE, lower.panel=panel.shade,upper.panel=panel.pie, text.panel=panel.txt)

corrgram(air.df, order=NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel=panel.txt)

#CORRELATIONS
cor(air.df[,-c(1)])                     #Removing the aphabetical column

#CHECKING CORRELATIONS FOR PRICE QUANTITIES
y<-air.df[,c(2,3,6,7,8,9,10,11,12,13,14,15,16,17)]
y1<-air.df[,c(12,13,14)]
cor(y,y1)

# Variance-Covariance Matrix
 cor(air.df[,-c(1)])                    #Removing the aphabetical column

                             
                           #APPROACH 1

#PRICE_RELATIVE SHOULD BE TAKEN AS Y
fit<-lm(PRICE_RELATIVE~.,data=air.df)
summary(fit)

                              #HYPOTHESIS
# Mean of the RELATIVE_PRICE should not be equal to zero.


#USING THE GLM COMMAND AND CHECKING AIC VALUE FOR EACH EXCLUSION OF VARIABLE

fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM,data=air.df)
summary(fit)                                                # AIC VALUE DECREASES SO IT CAN BE REMOVED 

fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-PITCH_ECONOMY,data=air.df)
summary(fit)                                                # AIC VALUE REMAINS THE SAME SO IT CANNOT BE REMOVED

fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-PITCH_PREMIUM,data=air.df)
summary(fit)                                                # AIC VALUE REMAINS THE SAME SO IT CANNOT BE REMOVED

fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-WIDTH_ECONOMY,data=air.df)
summary(fit)                                                # AIC VALUE INCREASES SO IT CANnOT BE REMOVED


fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-MONTH,data=air.df)
summary(fit)                                                 # AIC VALUE DECREASES SO IT CAN BE REMOVED 

fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL,data=air.df)
summary(fit)                                                 # AIC VALUE DECREASES SO IT CAN BE REMOVED

fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-AIRCRAFT,data=air.df)
summary(fit)                                                 # AIC VALUE REMAINS THE SAME AND IT CANNOT BE REMOVED 

fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-SEATS_ECONOMY,data=air.df)
summary(fit)                                                 # AIC VALUE REMAINS THE SAME AND IT CANNOT BE REMOVED 


fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-SEATS_PREMIUM,data=air.df)
summary(fit)                                                 # AIC VALUE REMAINS THE SAME AND IT CANNOT BE REMOVED 

fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-LAMBDA,data=air.df)
summary(fit)                                                 #AIC VALUE INCREASES AND SO CANNOT BE REMOVED

fit<-lm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-AIRLINE,data=air.df)
summary(fit)                                                 #AIC VALUE INCREASES AND SO IT CANNOT BE REMOVED

fit<-glm(PRICE_RELATIVE~.-WIDTH_PREMIUM-INTERNATIONAL-MONTH-QUALITY,data=air.df)
summary(fit)                                                 #AIC VALUE REMAINS THE SAME AND SO CANOT BE REMOVED


#AIC VALUE OBTAINED IS MINIMUM FOR THIS MODEL TILL NOW
fit<-lm(PRICE_RELATIVE~.-INTERNATIONAL-MONTH-WIDTH_PREMIUM,data=air.df)
summary(fit)

                     #ADDING INTERACTION TERMS

fit<-glm(PRICE_RELATIVE~.-INTERNATIONAL-MONTH-WIDTH_PREMIUM+WIDTH_PREMIUM*WIDTH_ECONOMY,data=air.df)
summary(fit) # AIC INCREASES SO IT SHOULD NOT BE ADDED

fit<-glm(PRICE_RELATIVE~.-INTERNATIONAL-MONTH-WIDTH_PREMIUM+PRICE_PREMIUM*PRICE_ECONOMY,data=air.df)
summary(fit)# AIC DECREASES SO IT SHOULD BE ADDED
 
#INTUITIVELY,WIDTH_PREMIUM SHOULD NOT BE REMOVED, ALSO BECAUSE CORRELATION IS HIGH
fit<-glm(PRICE_RELATIVE~.-INTERNATIONAL-MONTH+PRICE_PREMIUM*PRICE_ECONOMY,data=air.df)
summary(fit)

#To check the prediction values from this model
predict(fit)

#As the p-value of the model is 2.2e-16 wwhich is much lower than the significant levels of 0.05, so we can safely reject the null hypothesis that the value for the co-efficient is zero.
                                     
                                          #APPROACH   2

fit<-lm(PRICE_PREMIUM~.,data=air.df)
summary(fit)

fit<-lm(PRICE_ECONOMY~.,data=air.df)
summary(fit)

                             #HYPOTHESIS

#The mean of the PRICE_ECONOMY is greater than or equal to the mean of the PRICE_PREMIUM.

                         #Alternate Hypothesis

#The mean of the PRICE_PREMIUM is higher than the mean of the PRICE_ECONOMY.

t.test(air.df$PRICE_ECONOMY,air.df$PRICE_PREMIUM,alternative = "less")


#Linear regression for Differnce of the prices as Y
Temp<-air.df$PRICE_PREMIUM- air.df$PRICE_ECONOMY

#R-Sqaured value equals to 1 and residual error term reduced to ~Zero
fit<-lm(Temp~.-INTERNATIONAL-MONTH+PRICE_PREMIUM*PRICE_ECONOMY,data=air.df)
summary(fit)

#AIC Value is reduced to -2260
fit<-glm(Temp~.-INTERNATIONAL-MONTH+PRICE_PREMIUM*PRICE_ECONOMY,data=air.df)
summary(fit)

#PREDICTION OF VALUES 
predict(fit)

#SUMMARY TELLS US THAT IT IS ~ a-b=a-b type
#HENCE MODEL NOT CONSIDERED

                                