# Library--------------------------------------------------

library(readr) #Load csv data files
library(corrplot) #Run correlation plots
library(leaps) # Regression Subsets
library(randomForest) # Regression with Random Forest Trees
library(ggplot2) #Plotting output from Random Forest 
library(tidyverse) #mutate function


# Data Loading and preparing ------------------------------

Admission_Predict_Ver1_1 <- read_csv("Admission_Predict_Ver1.1.csv")

data <- Admission_Predict_Ver1_1
originaldata <- data

rm(Admission_Predict_Ver1_1)


colnames(data)[colnames(data)=="GRE Score"] <- "GRE"
colnames(data)[colnames(data)=="TOEFL Score"] <- "TOEFL"
colnames(data)[colnames(data)=="University Rating"] <- "UniRating"
colnames(data)[colnames(data)=="Chance of Admit"] <- "AdmitChance"

data$`Serial No.` <- NULL

# Data Loading and preparing ------------------------------

# Plot Theme
plot_theme <- function(base_size = 12, base_family = "Helvetica") {
  theme(
    plot.background = element_rect(fill = "#F7F6ED"),
    legend.key = element_rect(fill = "#F7F6ED"),
    legend.background = element_rect(fill = "#F7F6ED"),
    panel.background = element_rect(fill = "#F7F6ED"),
    panel.border = element_rect(colour = "magenta", fill = NA, linetype = "solid"),
    panel.grid.minor = element_line(colour = "#7F7F7F", linetype = "dashed"),
    panel.grid.major = element_line(colour = "#7F7F7F", linetype = "dashed")
  )
}

#Histogram and Density plots

#GRE Scores
a <- ggplot(data,aes(x=GRE,..density..)) +
  geom_histogram(binwidth=2, color="black", aes(fill=..count..)) + scale_fill_gradient("Count", low="cyan", high="blue") +
  labs(x="GRE Scores",y="Counts",title="Distribution for GRE Score", fill= FALSE) + geom_line(stat="density", color="red",size=1.2) 
a + guides(fill=FALSE)

#TOEFL Scores
a <- ggplot(data,aes(x=TOEFL,..density..)) + 
  geom_histogram(binwidth=2, color="black", aes(fill=..count..)) + scale_fill_gradient("Count", low="cyan", high="blue") +
  labs(x="TOEFL Scores",y="Counts",title=" Distribution for TOEFL Score") + geom_line(stat="density", color="red",size=1.2) 
a + guides(fill=FALSE)

#CGPA
a <- ggplot(data,aes(x=CGPA,..density..)) + 
  geom_histogram(binwidth=0.4, color="black", aes(fill=..count..)) + scale_fill_gradient("Count", low="cyan", high="blue") +
  labs(x="CGPA",y="Counts",title="Distribution for CGPA ",fill=FALSE) + geom_line(stat="density", color="red",size=1.2)
a + guides(fill=FALSE)

#Chance of Admit
a <- ggplot(data,aes(x=AdmitChance,..density..)) + 
  geom_histogram(binwidth=0.07, color="black", aes(fill=..count..)) + scale_fill_gradient("Count", low="cyan", high="blue") +
  labs(x="Chance of Admit",y="Counts",title="Distribution for Chance of Admit ", fill= FALSE) + geom_line(stat="density", color="red",size=1.2)
a + guides(fill=FALSE)

# CORRELATION PLOT ----------------------------------------

#form the model matrix and correlation variable
varMat <- model.matrix(~.-Research,data=data)[,-1]     #the first column is all 1's, remove this.
varCor <- cor(varMat)

#plot the correlation
corrplot(varCor,method = "circle",
         tl.col = "black", mar = c(0,0,2,0),
         title = "Graduate Admission numerical Variables Correlation")

corrplot(varCor,add = TRUE,                            # add the above plot
         type = "lower", method = "number",number.font = 2,
         number.cex = .75,col = "black",
         diag = FALSE,tl.pos = "n", cl.pos = "n")

rm(varMat, varCor)


########## SCATTER PLOTS for best indicators in Correlation ####

scatterdata <- tbl_df(data)
scatterdata <- mutate(scatterdata, GRE_Score = ifelse(GRE>325, "High", ifelse(GRE<308, "Low", "Medium")))

ggplot(scatterdata, aes(x = CGPA, y = AdmitChance, color = GRE_Score)) + 
  geom_point(size = 2) + labs(x="CGPA (scale of 10)", y = "Admission Probability") + 
  geom_smooth(method = "lm", color = "red") + scale_color_discrete(breaks=c("High","Medium","Low"))

rm(scatterdata)

#Clear linear relationship visible between CGPA and admission probability

##################### LINEAR REGRESSION ########################

str(data)

#Check if there are any rows with missing values
anyNA(data)

#scaling function
#normalize <- function(x) {
#  return ((x - min(x)) / (max(x) - min(x)))
#}

head(data)
regdata <- as.data.frame(data)
#Scaling - not yielding the results - stick with as is
#regdata <- as.data.frame( normalize(data[1:2] ))
#regdata <- cbind(regdata, data[3:7])

regdata$GRE <- as.integer(regdata$GRE)
regdata$TOEFL <- as.integer(regdata$TOEFL)
#regdata$UniRating <- as.factor(regdata$UniRating)
regdata$Research <- as.logical(regdata$Research)

head(regdata)

summary(regdata)


#contrasts(regdata$UniRating)

#Fit a model using all predictors
linear.fit=lm(AdmitChance ~.,data=regdata)
summary(linear.fit)

# SOP is not a reliable indicator with a p value of 0.73, Unirating is 0.12

#Fit a model using all predictors but SOP
linear.fit=lm(AdmitChance ~ .-SOP,data=regdata)
summary(linear.fit)
# All predictors are significant now exception of UniRating at 0.06

#linear.fit=lm(AdmitChance ~ .-SOP -UniRating,data=regdata)
#summary(linear.fit)

# How do Residuals look?
ggplot(linear.fit, aes(x = .fitted, y = .resid)) + 
  geom_point(color = "red", size = 1) + geom_hline(yintercept = 0) +
  plot_theme() +
  ylab("Residuals") +
  xlab("Fitted Values")

#Use subsets and run the model
regfit.full = regsubsets(AdmitChance ~., regdata)
reg.summary <- summary(regfit.full)
reg.summary

cor(reg.summary$outmat)
round(coef(regfit.full,loc), 3)
names(reg.summary)

adjr2 <- reg.summary$adjr2 #best with all indicators in place except SOP, 5 var => 0.819, 6 var => 0.820 marginal diff
cp <- reg.summary$cp #lowest with all predictors in place except SOP
rss <- reg.summary$rss #lowest with all predictors except SOP in place
bic <- reg.summary$bic #lowest with 5 predictors, SOP and UniRating not imporving BIC

#Diagnostic Plots
subsets_data <- data.frame(adjr2, cp, rss, bic, vars)
colnames(subsets_data) <- c("Adjusted R Square", "CP", "Residual Sum of Squares", "BIC")

ggplot(subsets_data, aes(x = vars, y = adjr2)) + 
  geom_line(color = "blue") + 
  geom_point(color = "red", size = 3) +
  plot_theme() +
  scale_x_discrete(limits = c(1:7)) +
  ylab("Adjusted R Square") +
  xlab("Number of Variables")

ggplot(subsets_data, aes(x = vars, y = rss)) + 
  geom_line(color = "blue") + 
  geom_point(color = "red", size = 3) +
  plot_theme() +
  scale_x_discrete(limits = c(1:7)) +
  ylab("Residual Sum of Squares") +
  xlab("Number of Variables")

ggplot(subsets_data, aes(x = vars, y = bic)) + 
  geom_line(color = "blue") + 
  geom_point(color = "red", size = 3) +
  plot_theme() +
  scale_x_discrete(limits = c(1:7)) +
  ylab("BIC") +
  xlab("Number of Variables")

ggplot(subsets_data, aes(x = vars, y = cp)) + 
  geom_line(color = "blue") + 
  geom_point(color = "red", size = 3) +
  plot_theme() +
  scale_x_discrete(limits = c(1:7)) +
  ylab("CP") +
  xlab("Number of Variables")


# ggplot yields better plots than Plot function
# windows()
# par(mfrow = c(2,2))
# 
# xlab = "Number of Variables"
# # 1st row 1st  column
# plot(reg.summary$rss,xlab = xlab, ylab = "Residual Sum of Squares", col= "dark green", lwd = 2, type = "l") +
# loc <- which.min(reg.summary$rss)
# loc
# points(loc,reg.summary$rss[loc], col = "red",cex = 2,pch = 20)
# 
# 
# # 1st row 2nd  column
# plot(reg.summary$adjr2,xlab = xlab, ylab = "Adjusted R Square", col= "dark blue", lwd = 2, type = "l")
# loc <- which.max(reg.summary$adjr2)
# loc
# points(loc,reg.summary$adjr2[loc], col = "red",cex = 2,pch = 20)
# 
# # 2nd row 1st column
# plot(reg.summary$cp,xlab = xlab, ylab = "CP", col= "brown", lwd = 2, type = 'l')
# loc <- which.min(reg.summary$cp)
# loc
# points(loc,reg.summary$cp[loc], col = "red",cex = 2,pch = 20)
# 
# # 2nd row 2nd column
# plot(reg.summary$bic,xlab = xlab, ylab = "BIC", col= "orange", lwd = 2, type = 'l')
# loc <-  which.min(reg.summary$bic)
# loc
# points(loc,reg.summary$bic[loc], col = "red",cex = 2,pch = 20)
# 
# 
# dev.off()

## What is the mean square error (base case)?
mse <- round(reg.summary$rss[6]/nrow(regdata), 4)
mse #0.0035 for 5, 6, 7 variable model, lets see if train/test technique improves the result substantially
rmse <- round(sqrt(mse), 2)
rmse #0.06


######## Linear Regression with subset selection methods ########
### Base case no folds ###
?regsubsets

set.seed(123)

train = sample(c(TRUE,FALSE), nrow(regdata)*0.75, replace = TRUE)

test = !train

#regdata[train, ]

# Find the best training set models
regfit.best = regsubsets(AdmitChance~., data = regdata[train,], nvmax = 7)
coef(regfit.best, 5)

# Obtain the test set design matrix
test.mat = model.matrix(AdmitChance~., data = regdata[test,])

train.mat = model.matrix(AdmitChance~., data = regdata[train,])
mean(regfit.best$rss -regfit.best$ress)

regdata[test.mat]

head(regdata[train, 8])

#regfit.best$adjr2 #best with all indicators in place except SOP, 5 var => 0.819, 6 var => 0.820 marginal diff
#regfit.best$cp #lowest with all predictors in place except SOP
#regfit.best$rss #lowest with all predictors except SOP in place
#regfit.best$bic #lowest with 5 predictors, SOP and UniRating not imporving BIC

# Vector for errors
val.errors = rep(NA,7)
training.errors = rep(NA, 7)

# Run for each number of variables in the model
for (i in 1:7) {
  # obtain the training set coefficients
  coefi = coef(regfit.best,id = i)
  
  # predict test set values
  pred = test.mat[,names(coefi)] %*% coefi
  
  pred2 = train.mat[,names(coefi)] %*% coefi
  
  # Obtain the MSE
  val.errors[i] = mean((regdata$AdmitChance[test] - pred)^2)
  training.errors[i] = mean((regdata$AdmitChance[train] - pred2)^2)
}

round(val.errors, 4) 
round(training.errors, 4) 

## test and training mse 0.0033 vs 0.0039 for 6 var model


### Regsubsets with 10 Folds ###
k <- 10
set.seed(123)

#define the predict function to work on each fold

predict.regsubsets =
  function(object,newdata,id,...){
    form = as.formula(object$call[[2]])
    mat = model.matrix(form,newdata)
    coefi = coef(object,id = id)
    xvars = names(coefi)
    mat[,xvars] %*% coefi
  }

folds <- sample(1:k,nrow(regdata),  replace = TRUE)

cv.errors = matrix(NA,nrow = k,ncol = 7, dimnames = list(NULL, paste(1:7)))

# The fold and number of variables loops
for (j in 1:k) { # fold loop
  
  # The 7 best models with jth fold omitted
  bestfit.fold = regsubsets(AdmitChance ~., data = regdata[folds != j,],nvmax = 7)
  
  # The MSE for the fold prediction error
  for (i in 1:7) {# number of variable loop
    pred = predict.regsubsets(bestfit.fold, regdata[folds == j,],id = i)
    cv.errors[j,i] = mean((regdata$AdmitChance[folds == j] - pred)^2)
  }
}

# Find the mean across the fold MSE for each model
mean.cv.errors = apply(cv.errors,2,mean)
round(mean.cv.errors, 4) # 0.0037 for 5 to 7 var models
#rmse = 0.06 again
# base case performs better

par(mfrow = c(1,1))
plot(mean.cv.errors,type = 'b')
which.min(mean.cv.errors)

##############  Random Forest  ###############
#  Define Train and Test

# Train and Test -----------------------------------------

set.seed(123)
num <- sample(1:500, nrow(data)*0.75, replace = FALSE)

train <- data[num,]
test <- data[-num,]

rm(num)

# Train and Test -----------------------------------------

temp <- test$AdmitChance
temp <- as.data.frame(temp)

# Random Forest -------------------------------------------

rf <- randomForest(AdmitChance ~., data = train) # with all the variables

varImpPlot(rf, main = 'Model Importance Plot')

impplot <- rf$importance


impplot <- as.data.frame(impplot)

impplot$Attribute <- rownames(impplot)

#mesh2$cat2 <- order(mesh2$Category, mesh2$Count, decreasing=TRUE)

p <- ggplot(data = impplot, aes(reorder(Attribute, IncNodePurity), IncNodePurity)) + geom_col(mapping = NULL, data = NULL, position = "stack",
                                                                 width = NULL, na.rm = FALSE, show.legend = NA,
                                                                 inherit.aes = TRUE,fill = 'steelblue4') + plot_theme() 
p + coord_flip() + xlab("Attributes") + labs(title = "Variable Importance Plot")

rm(p,impplot)

plot(rf, main = "Error with Number of Trees")

pretest <- predict(rf,test[-8])

temp$rf <- pretest


rf2 <- randomForest(AdmitChance ~ CGPA+UniRating+SOP+LOR+Research, data = train) # with few variables

pretest <- predict(rf2,test[c(-1,-2,-8)])

temp$rf2 <- pretest


rf3 <- randomForest(AdmitChance ~GRE+TOEFL+UniRating+SOP+LOR+CGPA, data = train) # without research based imp plot

pretest <- predict(rf3,test[c(-7,-8)])

temp$rf3 <- pretest

rm(pretest)


# RMSE ----------------------------------------------------

library(ModelMetrics)

rmse(temp$temp,temp$rf) # with all variable = 0.03700746


rmse(temp$temp,temp$rf2)# with all variable = 0.05727096


rmse(temp$temp,temp$rf3) # without Research  = 0.03746171

# Based on the RMSE value model with all the predictors and without Research performed best

# RMSE ----------------------------------------------------


# RANDOM FOREST WITH CV ----------------------------------------------


k = 10

fold = 1:10

datacv <- data

datacv$kfold <- sample(1:k, nrow(datacv), replace = TRUE)

length(which(datacv$kfold == 9))

prediction <- data.frame()
test_sets <- data.frame()

train_sets <- data.frame()
train_pred <- data.frame()

for(n in 1:k){
  ###Grab all the rows with the id 'n', aggregate them into a test set
  test_set = datacv[which(datacv$kfold %in% n), -ncol(datacv)]
  
  ###All the other rows (the other 9 parts) go in the training set 
  train_set = datacv[which(datacv$kfold %in% fold[-c(n)]), -ncol(datacv)]
  
  forest = randomForest(AdmitChance ~., data = train_set, importance = TRUE,   ntree = 500)
  
  ###Run the model on the test set, save the prediction in a dataframe, the test set in another. Then you can compare them to get your performance measure.	
  n_predict = data.frame(predict(forest, test_set))
  prediction = rbind(prediction, n_predict)
  test_sets = rbind(test_sets, as.data.frame(test_set))
  
  train_sets = rbind(train_sets, train_set)
  train_pred = rbind(train_pred, as.data.frame(forest$predicted))
} 

test_sets$

library(ModelMetrics)

  
a <- rmse(prediction$predict.forest..test_set., test_sets$AdmitChance) # = 0.062

varImpPlot(forest)

b <- rmse(train_pred$`forest$predicted`, train_sets$AdmitChance) # = 0.062


rm(a,b)




# RANDOM FOREST WITH CV ----------------------------------------------
