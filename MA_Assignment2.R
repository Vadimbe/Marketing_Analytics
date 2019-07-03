library(data.table)
library(RODBC)
library(Matrix)
library(foreach)
library(Matrix)
library(foreach)
library(glmnet)
library(nnet)

odbcDataSources()
db<-odbcConnect("mysqlodbc",uid="root",pwd=" ")
sqlQuery(db,"USE ma_charity_full")

# Solicitation for campaign C189 is made on the 05/15/2018
# query1 : last_don_sol : delay between last donation and the solicitation of C189

query1="SELECT  s.contact_id, 
        count(s.contact_id) as frequency, 
        ifnull(MAX(a.amount),0) as max_amount, ifnull(MIN(a.amount),0) as min_amount, ifnull(AVG(a.amount),0) as avg_amount,SUM(a.amount) as sum_amount,
        s.donation, s.amount, s.calibration,
        DATEDIFF(20180515, max(a.Act_Date)) as last_don_sol
        From assignment2 s
        LEFT JOIN  acts as a ON s.Contact_Id = a.Contact_Id
        GROUP BY 1"
data1=sqlQuery(db,query1)
dim(data1)

#query2 : Donation between 2012 and the solicitation of C189 
query2="Select s.contact_id, 
        ifnull(c.count_don,0) as count_don_2012, c.avg_amount as avg_amount_2012
        from assignment2 s
        Left join (
        Select contact_id, avg(amount) as avg_amount, count(amount) as count_don
        from acts 
        Where ( Year(act_date) >= 2012 ) and (Year(act_date) < 20180515 )
        group by contact_id ) as c
        on s.contact_id = c.contact_id;"
data2=sqlQuery(db,query2)
dim(data2)

data <- merge(data1,data2,by="contact_id", all.x = TRUE)

## query_test : 123716 rows for 123672 donors, 
#There are doublons, some people have been solicitated many times.
query_test="SELECT *
FROM actions
INNER JOIN assignment2 ON actions.contact_id = assignment2.contact_id
where actions.campaign_id='C189'
group by 1"
data_test=sqlQuery(db,query_test)
dim(data_test) # 123716 rows 

# Number of times that each donors has been reached by the solicitation of C189    
query3="Select contact_id, count(*) as nb_reache
from actions
where campaign_id Like 'C189'
group by contact_id"
data3=sqlQuery(db,query3)
dim(data3)

data <- merge(data,data3,by="contact_id", all.x = TRUE)

# query4:  Have donors made a donation in 2018 (whitout the PA) ?
query4="Select s.contact_id, ifnull(c.nb_don,0) as don_2018
from assignment2 s
left join (Select contact_id, Count(amount) as nb_don
           from acts 
           Where ( Year(act_date) = 2018 ) and (act_type_id like 'DO') and (NOT isnull(campaign_id))
           group by contact_id ) as c
on s.contact_id = c.contact_id"
data4=sqlQuery(db,query4)

data <- merge(data,data4,by="contact_id", all.x = TRUE)

# query5:  Prefix_id according the sex and the status, people tend to donate more or less. 
query5= "Select s.contact_id, ifnull(c.prefix_id,'NA') as prefix_id, c.active
From assignment2 s
Left join contacts c ON c.id = s.contact_id
group by 1"
data5=sqlQuery(db,query5)

# Dummy variables for prefix_id
# Male(MR), Female(MME,MLLE), Couple(MMME), Other_prefix(AU,NA,DR,ME)
# DR in Other_prefix because there are men and women, 0 people in ME.  

data5$Male <- sapply(1:length(data5$prefix_id), function(x) 
  if(data5[x,2] %like% "MR") {1}
  else {0})

data5$Female <- sapply(1:length(data5$prefix_id), function(x) 
  if(data5[x,2] %like% "MME") {1}
  else if (data5[x,2] %like% "MLLE") {1}
  else {0})
data5$Female [which (data5$prefix_id== "MMME")]= 0

data5$Couple <- sapply(1:length(data5$prefix_id), function(x) 
  if(data5[x,2] %like% "MMME") {1}
  else {0})

data5$Other_prefix <- sapply(1:length(data5$prefix_id), function(x) 
  if(data5[x,2] %like% "AU") {1}
  else if (data5[x,2] %like% "NA") {1}
  else if (data5[x,2] %like% "DR") {1}
  else {0})

data5 <- subset(data5, select = -c(prefix_id))

## DATASET FINAL ## 
data <- merge(data,data5,by="contact_id", all.x = TRUE)
View(data)

## Split Train/Test ## 

calib1= which(data$calibration==1)
calib0 = which(data$calibration==0)

data_train = data[calib1,]
data_test = data[calib0,]

y_train_don = data_train$donation
y_train_amount= data_train$amount

new_amount=which(!is.na(data_train$amount))
y_train_amount = y_train_amount[new_amount]

train_amount = data_train[new_amount,]

data_test <- subset(data_test, select = -c(donation,amount,calibration))

#View(data_train)
#View(data_test)
#View(y_train_amount)
#View(train_amount)

dim(data_train)
dim(data_test)
dim(train_amount)

# Likelihood of donation
model = multinom(formula = y_train_don ~ log(frequency) + log(max_amount) + log(min_amount) + log(avg_amount) + log(avg_amount_2012)
                 + last_don_sol + count_don_2012 + nb_reache + don_2018 + active + 
                   Male + Female + Couple + Other_prefix
                 ,data = data_train)
summary(model)

# Linear regression : most likely donation amount

lin_reg= lm(formula= log(y_train_amount) ~ log(max_amount) + log(min_amount)+ log(avg_amount) + 
              log(avg_amount_2012) + don_2018, data=train_amount)
summary (lin_reg)


x_train<- cbind(log(train_amount$max_amount) , log(train_amount$min_amount), log(train_amount$avg_amount) ,log(train_amount$frequency), log(train_amount$avg_amount_2012),log(train_amount$count_don_2012), train_amount$Male, train_amount$Female, train_amount$Couple, train_amount$Other_prefix)
y_train <- log(y_train_amount)
#x_train<- cbind(train_amount$max_amount, train_amount$min_amount, train_amount$avg_amount ,train_amount$frequency, train_amount$avg_amount_2012)
#y_train <- y_train_amount

set.seed(1)
## Ridge and Lasse ## With Lasso I obtain my best result. 

# Ridge 
#cv.ridge=cv.glmnet(x_train,y_train,alpha =0)
#plot(cv.ridge)
#bestlam_ridge=cv.ridge$lambda.min
#ridge.pred=predict(cv.ridge,s = bestlam_ridge,newx = x_train)

# Lasso 
cv.lasso=cv.glmnet(x_train,y_train,alpha=1)
plot(cv.lasso)
bestlam_lasso = cv.lasso$lambda.min
lasso.pred=predict(cv.lasso,s = bestlam_lasso,newx = x_train)

# X_test
x_test<-  cbind(log(data_test$max_amount) , log(data_test$min_amount), log(data_test$avg_amount), log(data_test$frequency), log(data_test$avg_amount_2012),log(data_test$count_don_2012),data_test$Male, data_test$Female, data_test$Couple, data_test$Other_prefix)

# Apply both models to the prediction data, to obtain expected revenue if solicited.
final = data.frame(contactid = data_test$contact_id)
final$probs  = predict(object = model, newdata = data_test, type = "probs")
final$amount = exp(predict(cv.lasso,s = bestlam_lasso,newx = x_test))
final$score  = final$probs * final$amount

# Final result: if expected revenue is superior to 2.00 €, solicit (=1); otherwise do not (=0).
resultat = data.frame(contact_id = data_test$contact_id)
resultat$solicit=0
resultat$solicit[which(final$score>2)]=1
resultat$solicit[which(final$score<=2)]=0
resultat = resultat[order(resultat$contact_id),]

# Export final 
write.table(resultat,"/Users/vadimbenichou/Desktop/MA_ASS2-3.txt",sep="\t",row.names=FALSE, col.names = FALSE)
