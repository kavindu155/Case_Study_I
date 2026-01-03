library(tidyverse)
library(faraway)
view(chredlin)




############
library(janitor)
duplicates<-get_dupes(chredlin)

duplicates
dim(duplicates)

duplicated(chredlin)

cleaned<-chredlin[!duplicated(chredlin),]
cleaned

new_data<-cleaned

new_data<-janitor::clean_names(new_data)  #cleaning the data frame col names



###to get how many missng datas available in certain data frame by columns

missing_count<-colSums(is.na(new_data))
missing_count
### to check wether are there any missing bvalues in the data frame
dim(missing_count)






par(mfrow=c(2,3))
for (i in 1:6) stripchart(chredlin[,i],main=names(chredlin) [i] vertical=TRUE,method="jitter")
  par(mfrow=c(1,1))
pairs(chredlin)  

summary(lm(involact~race,chredlin))
         

plot(fire~race,chredlin)
abline(lm(fire~race,chredlin))


full_model <- lm(involact ~race+fire+theft+age+log(income),chredlin)
summary(full_model)

plot(fitted(full_model),residuals(full_model), xlab = "Fitted",ylab = "Residuals")
abline(h=0)
qqnorm(residuals(full_model))
qqline(residuals(full_model))

f_model <- influence(full_model)
qqnorml(f_model$coef[,4])
halfnorm(cooks.distance(full_model))



## check model assumptions

chredlin1 <- as.numeric(chredlin$side,chredlin)
summary(chredlin)
view(chredlin)

##Assumption 1
model1<- lm(formula = involact ~ race + fire + theft + age + log(income), data = chredlin)
model1
model_res <- model1$residuals
model_res
hist(model_res)
qqnorm(model_res)
qqline(model_res)



range(rstudent(full_model))

chredlin[c(6,24),]

full_model <-lm(involact ~ race + fire + theft + age + log(income), chredlin, subset = c(6,24))
summary(full_model)





##assumption 3






library(ggcorrplot)
corr<-cor(model1)
corr
ggcorrplot(corr,hc.order = TRUE , title = "Correlation Matrix",lab = TRUE,digits = 4)

summary(model1)





