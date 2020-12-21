library(readxl)
library(ggplot2)
library(tseries)
library(tidyr)
library(zoo)
library(urca)

# ---------

database = read_excel('database.xlsx')

# Plotting the response variables:
database = database[,-9]
gather_dt = gather(database[,2:13])
gather_dt$Date = rep(database$Date, 12)
ggplot(data = gather_dt, aes(x = Date, y = value))+
        geom_line()+
        facet_wrap(facets = .~key ,nrow = 4,ncol = 3, scales = 'free_y')+
        theme_bw()+
        ylab('Time Series Value')
ggsave('1.tiff')
database = database[length(database$Date):1, ]

# adf test for the explantory variables:
adf.test(x = database$basis, alternative = 'stationary', k = 1)
adf.test(x = database$`OCED OIL CONSUMPTION`, alternative = 'stationary', k = 3) 
adf.test(x = database$`NYM1CNCN INDEX`, alternative = 'stationary', k = 3) 
adf.test(x = database$`OPEC TOTAL SURPLUS`, alternative = 'stationary', k = 3) # need to use diff
adf.test(x = database$`DOLLAR INDEX`, alternative = 'stationary', k = 3) # need to use diff
adf.test(x = database$`SP500 INDEX`, alternative = 'stationary', k = 3) # need to use diff
adf.test(x = database$`GDP GROWTH RATE`, alternative = 'stationary', k = 3) 
adf.test(x = database$`CPI US`, alternative = 'stationary', k = 3) 

adf.test(x = database$`FEDERAL RATE`, alternative = 'stationary', k = 3) # need to use diff
adf.test(x = database$`DOW JONES OIL INDEX`, alternative = 'stationary', k = 3) # need to use diff
adf.test(x = database$`OPEN INTERESTS CRUDE OIL TOTAL`, alternative = 'stationary', k = 3) # need to use diff
adf.test(x = database$Terrorism_attack, alternative = 'stationary', k = 3) # need to use diff



pp = ur.df(y = database$basis, type = 'trend',selectlags = 'AIC')
summary(pp)

pp = ur.df(y = database$`OCED OIL CONSUMPTION`, type = 'trend',selectlags = 'AIC')
summary(pp)
pp = ur.df(y = database$`NYM1CNCN INDEX`, type = 'trend',selectlags = 'AIC')
summary(pp)
pp = ur.df(y = database$`OPEC TOTAL SURPLUS`, type = 'trend',selectlags = 'AIC')
summary(pp)
pp = ur.df(y = database$`DOLLAR INDEX`, type = 'trend',selectlags = 'AIC')
summary(pp)
pp = ur.df(y = database$`SP500 INDEX`, type = 'trend',selectlags = 'AIC')
summary(pp)
pp = ur.df(y = database$`GDP GROWTH RATE`, type = 'trend',selectlags = 'AIC')
summary(pp)
pp = ur.df(y = database$`CPI US`, type = 'trend',selectlags = 'AIC')
summary(pp)

pp = ur.df(y = database$`FEDERAL RATE`, type = 'trend',selectlags = 'AIC')
summary(pp)
pp = ur.df(y = database$`DOW JONES OIL INDEX`, type = 'trend',selectlags = 'AIC')
summary(pp)
pp = ur.df(y = database$`OPEN INTERESTS CRUDE OIL TOTAL`, type = 'trend',selectlags = 'AIC')
summary(pp)
pp = ur.df(y = database$Terrorism_attack, type = 'trend',selectlags = 'AIC')
summary(pp)



`Delta OPEC TOTAL SURPLUS`= diff(database$`OPEC TOTAL SURPLUS`, differences = 1)
pp = ur.df(y = `Delta OPEC TOTAL SURPLUS`, type = 'trend',selectlags = 'AIC')
summary(pp)

`Delta DOLLAR INDEX`= diff(database$`DOLLAR INDEX`, differences = 1)
pp = ur.df(y = `Delta DOLLAR INDEX`, type = 'trend',selectlags = 'AIC')
summary(pp)

`Delta SP500 INDEX`= diff(database$`SP500 INDEX`, differences = 1)
pp = ur.df(y = `Delta SP500 INDEX`, type = 'trend',selectlags = 'AIC')
summary(pp)


`Delta FEDERAL RATE`= diff(database$`FEDERAL RATE`, differences = 1)
pp = ur.df(y = `Delta FEDERAL RATE`, type = 'trend',selectlags = 'AIC')
summary(pp)

`Delta DOW JONES OIL INDEX`= diff(database$`DOW JONES OIL INDEX`, differences = 1)
pp = ur.df(y = `Delta DOW JONES OIL INDEX`, type = 'trend',selectlags = 'AIC')
summary(pp)

`Delta OPEN INTERESTS CRUDE OIL TOTAL`= diff(database$`OPEN INTERESTS CRUDE OIL TOTAL`, differences = 1)
pp = ur.df(y = `Delta OPEN INTERESTS CRUDE OIL TOTAL`, type = 'trend',selectlags = 'AIC')
summary(pp)

Delta_Terrorism_attack= diff(database$Terrorism_attack, differences = 1)
pp = ur.df(y = Delta_Terrorism_attack, type = 'trend',selectlags = 'AIC')
summary(pp)




# Update the new datasets
adj_database = database[-1, ]
adj_database$`OPEC TOTAL SURPLUS` = `Delta OPEC SPARE CAPACITY`
adj_database$`DOLLAR INDEX` = `Delta DOLLAR INDEX`
adj_database$`SP500 INDEX` = `Delta SP500 INDEX`
adj_database$`FEDERAL RATE` = `Delta FEDERAL RATE`
adj_database$`DOW JONES OIL INDEX` = `Delta DOW JONES OIL INDEX`
adj_database$`OPEN INTERESTS CRUDE OIL TOTAL` = `Delta OPEN INTERESTS CRUDE OIL TOTAL`
adj_database$Terrorism_attack = Delta_Terrorism_attack

colnames(adj_database)[c(4,5,6,9,10,11,12)] = c('OPEC TOTAL SURPLUS DIFF1',
                               'DOLLAR INDEX DIFF1',
                               'SP500 INDEX DIFF1',
                               'FEDERAL RATE DIFF1',
                               'DOW JONES OIL INDEX DIFF1',
                               'OPEN INTERESTS CRUDE OIL TOTAL DIFF1',
                               'Terrorism_attack DIFF1')


gather_dt = gather(adj_database[,2:13])
gather_dt$Date = rep(adj_database$Date, 12)
ggplot(data = gather_dt, aes(x = Date, y = value))+
        geom_line()+
        facet_wrap(facets = .~key ,nrow = 4,ncol = 3, scales = 'free_y')+
        theme_bw()
ggsave('2.tiff')


# build the model:
model = lm(data = adj_database[,-c(1,15,14)], basis ~ .)
summary(model)
par()$mar
par(mfrow = c(2,2))
plot(model)

train =  adj_database[,-c(1,15,14)]
x = train[,-12]
y = train$basis
predictions_ols <- predict(model, newdata = x)
sqrt(mean((predictions_ols - y)^2))
sqrt(mean((predictions_ols - y)^2))
eval_results(y, predictions_ols, train)

train = adj_database[,-c(1,15,14)]

library(tidyr); library(dplyr); library(ggplot2)
corMatrix = as.data.frame(cor(train[,-12]))
corMatrix$var1 = rownames(corMatrix)

corMatrix %>%
        gather(key=var2,value=r,1:11)%>%
        arrange(var1,desc(var2))%>%
        ggplot(aes(x=var1,y=reorder(var2, order(var2,decreasing=F)),fill=r))+
        geom_tile()+
        geom_text(aes(label=round(r,2)),size=3)+
        scale_fill_gradientn(colours = c('#d7191c','#fdae61','#ffffbf','#a6d96a','#1a9641'))+
        theme(axis.text.x=element_text(angle=75,hjust = 1))+xlab('')+ylab('')

library(car)
vif(model)
start_mod = lm(basis~.,data=train)
empty_mod = lm(basis~1,data=train)
full_mod = lm(basis~.,data=train)
backwardStepwise = step(start_mod,
                        scope=list(upper=full_mod,lower=empty_mod),
                        direction='backward')
summary(backwardStepwise)

train =  adj_database[,-c(1,15,14)]
x = train[,-12]
y = train$basis
predictions_aic <- predict(backwardStepwise, newdata = x)
sqrt(mean((predictions_aic - y)^2))
sqrt(mean((predictions_aic- y)^2))
eval_results(y, predictions_aic, train)

library(olsrr)
ols_step_backward_p(model, details = TRUE)


eval_results <- function(true, predicted, df) {
        SSE <- sum((predicted - true)^2)
        SST <- sum((true - mean(true))^2)
        R_square <- 1 - SSE / SST
        RMSE = sqrt(SSE/nrow(df))
        
        
        # Model performance metrics
        data.frame(
                RMSE = RMSE,
                Rsquare = R_square
        )
        
}

# ------------------------------ LASSO
library(glmnet)
x = model.matrix(basis~.,data=train)
y = train$basis
lassoModel = glmnet(x,y, alpha=1) 

plot(lassoModel,xvar='norm',label=T)
plot(lassoModel,xvar='lambda',label=T)
plot(lassoModel,xvar='dev',label=T)

set.seed(617)
cv.lasso = cv.glmnet(x,y,alpha=1) # 10-fold cross-validation
plot(cv.lasso)
coef(cv.lasso)

# Best 
lambda_best <-cv.lasso$lambda.min 
lambda_best

lasso_model <- glmnet(x, y, alpha = 1, lambda = lambda_best)
lasso_model$beta


x = model.matrix(basis~.,data=train)
y = train$basis
predictions_lasso <- predict(lasso_model, newx = x)
sqrt(mean((predictions_lasso - y)^2))
sqrt(mean((predictions_lasso - y)^2))
eval_results(y, predictions_lasso, train)
1 - ((1 - 0.2626794)*(227 - 1)/(227 -2-1))

# -----------------------------------------------

par(mfrow = c(1,1))

plot(train$basis, type = 'l')
lines(predictions_train, type = 'l', col = 'blue')
abline(h = 0, col = 'grey', lty = 2, cex =2)
grid(nx = NULL, ny = nx, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)


plot(adj_database$Date,train$basis, type = 'l', xlab = 'Date', ylab = 'LAST PRICE')
lines(adj_database$Date, predictions_lasso, type = 'l', col = 'blue')
abline(h = 0, col = 'grey', lty = 2, cex =2)
grid(nx = 25, ny = 0, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
legend(x = as.Date('2005-01-01', "%Y-%m-%d"), y = -2, legend = c('REAL BASIS SPREAD', 'LASSO Predicted BASIS'), col = 1:2, lty = 1)




