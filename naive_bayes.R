empdata <- read.csv()
colnames(empdata)

#Label
sal <- ifelse(empdata$Emp_Sal==">50k","High","Low")
empdata <- data.frame(empdata,Sal)
View(empdata)

#splitting
set.seed(2)
id <- sample(2,nrow(empdata),prob=c(0.7,0.3),replace = T)
emptrain <- empdata[id==1,]
emptest <- empdata[id==2,]

#Naive bayes
library(caret)
library(e1071)

emp_nb <- naiveBayes(Sal~Age_of_emp + Emp_Stat_type + Edu_of_Emp + Edu_Cat + Occ_of_Emp + work_hour_in_week + country_of_res,emptrain)
emp_nb

pree3 <- predict(emp_nb,emptest)
confusionMatrix(table(pree3,emptest$Sal))
