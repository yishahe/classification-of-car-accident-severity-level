
library(tidyr)
library(dplyr)
library(ggplot2)
library(purrr)
library(leaflet)
library(tidyverse)
library(scales)
library(lubridate)
library(plotly)
library(gridExtra)
library(tidytext)
library(modelr)
library(caret)
library(ROSE)
library(glmnet)
library(rpart)
library(rpart.plot)
library(randomForest)
library(tidyselect)

options(warn = -1)
df <- read_csv("US_accident.csv", col_types = cols(.default = col_character())) %>% 
  type_convert() %>%
  mutate(TMC = factor(TMC), Severity = factor(Severity), Year = factor(Year), Weekday = factor(Weekday)) %>%
  mutate_if(is.logical, factor) %>%
  mutate_if(is.character, factor)

##df graph

leaflet(df) %>% 
  addTiles() %>%
  setView(lng = -78.5080, lat = 38.0336, zoom = 7)%>% 
  addCircles(lng = df[df$Severity %in% c("1", "2"),]$Start_Lng, lat = df[df$Severity %in% c("1", "2"),]$Start_Lat, weight = 1.2, col="blue")%>%
  addCircles(lng = df[df$Severity %in% c("3", "4"),]$Start_Lng, lat = df[df$Severity %in% c("3", "4"),]$Start_Lat, weight = 1.2, col="orangered")%>%
  addLegend("topleft", title = "Accident Severity", colors = c("orangered", "blue"), labels = c("Severe", "Not Severe"))


df$Junction <- as.factor(df$Junction)
df$Traffic_Signal <- as.factor(df$Traffic_Signal)
df$Month <- factor(df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May",
                                        "Jun", "Jul", "Aug", "Sep", "Oct",
                                        "Nov", "Dec"))
##Check distribution of duration
hist(df$Duration)
##Take Logarithms of Duration
df$Duration <- log(df$Duration)
##filter out Weather Condition NA.
df <- df %>% filter(!(is.na(Weather_Condition)))



##Drop uncommon weather condition 
df %>% count(Weather_Condition) %>% filter(n < 20) %>% select(Weather_Condition, n)
drop_weather <- df %>% count(Weather_Condition) %>% filter(n < 20) %>% select(Weather_Condition)
drop_weather <- drop_weather$Weather_Condition %>% unlist()
df <- df %>% 
  filter(!(Weather_Condition %in% drop_weather)) %>% 
  mutate(Weather_Condition = factor(Weather_Condition))

###Merge some weather conditions
df$Weather_Condition <- as.character(df$Weather_Condition)
df$Weather_Condition[df$Weather_Condition %in% c("Cloudy", "Cloudy / Windy", "Mostly Cloudy", "Mostly Cloudy / Windy", "Partly Cloudy", "Partly Cloudy / Windy", "Scattered Clouds")]<-c("Cloudy/Windy")
df$Weather_Condition[df$Weather_Condition %in% c("Thunder", "Thunderstorm", "Thunderstorms and Rain", "Thunder in the Vicinity")]<- c("Thunder")
df$Weather_Condition[df$Weather_Condition %in% c("Haze", "Fog", "Patches of Fog", "Mist", "Shallow Fog", "Light Freezing Fog")]<- c("Haze/Mist/Fog")
df$Weather_Condition[df$Weather_Condition %in% c("Clear", "Fair", "Fair / Windy")]<-c("Clear/Fair")
df$Weather_Condition[df$Weather_Condition %in% c("Light Freezing Rain", "Light Thunderstorms and Rain","Light Rain", "Light Rain / Windy", "Light Drizzle", "Light Rain with Thunder", "Drizzle")]<- c("Rain")                
df$Weather_Condition[df$Weather_Condition %in% c("Heavy T-Storm", "Heavy Thunderstorms and Rain")]<- c("T-Storm")
df$Weather_Condition <- as.factor(df$Weather_Condition)
table(df$Weather_Condition)
##Weekday to 1, weekends to 0
df$Weekday <- as.character(df$Weekday)
df$Weekday[df$Weekday %in% c("Mon", "Tue", "Wed", "Thu", "Fri")]<- c("1")
df$Weekday[df$Weekday %in% c("Sat", "Sun")]<- c("0")
df$Weekday <- as.factor(df$Weekday)
##Drop uncommon TMC factor
df%>% count(TMC) %>% filter(n < 10)
drop_TMC <- df%>% count(TMC) %>% filter(n < 10) %>% select(TMC)
drop_TMC <- drop_TMC$TMC %>% unlist()
df <- df %>% filter(!TMC %in% drop_TMC) %>% mutate(TMC = factor(TMC))

##Merge severity 3 and 4 to be "Severe", and 1 2 to be "Not Severe"
df_label <- df %>%
  mutate("Status" = ifelse(Severity == "3" | Severity == "4", "Severe", "Not Severe"))
##Remove Near Zero Variance Predictors
nzv <- nearZeroVar(df_label, saveMetrics = T)
nzv[nzv$nzv,]
nzv_cols <- rownames(nzv[nzv$nzv,])
df_label <- subset(df_label, select = -c(State,Country,Visibility,Amenity,Bump,Crossing,Give_Way,No_Exit,Railway,Roundabout,Station,Stop,Traffic_Calming,Turning_Loop))

##Remove Street, start time, end time, zip code, 
df_location <- subset(df_label, select = -c(Street, Start_Time, End_Time, Zipcode, City, Severity, County, Sunrise_Sunset, Astronomical_Twilight, Nautical_Twilight))
df<- df_location
##rename some variables
df <-  df %>%
  rename("Latitude" = `Start_Lat`, "Longtitude" = `Start_Lng`, "Signal" = `Traffic_Signal`, 
         "Weather" = `Weather_Condition`)




#"Density of Accident Severity by Hour of Day"
df %>%
  ggplot(aes(x= Hour, fill = Status))+
  geom_density( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("steelblue", "orangered")) +
  theme_bw() +
  labs(fill="")




#title = "Counts of Accident by Month "
ggplot(data = df, aes(x= Month,  group = Status))+
  geom_line(stat = 'count',aes(color= Status), size = 0.8)+
  geom_point(stat = 'count', aes(color= Status)) +
  scale_color_manual(values=c("steelblue", "orangered")) +
  labs(y = "Count",
       x = NULL) +
  scale_y_continuous(labels = unit_format(unit = "K", scale = 1e-03))+
  theme_bw()

#title = "Severity Probability by Traffic Signal"
df %>%
  group_by(Signal, Status) %>%
  count()%>%
  group_by(Signal)%>%
  mutate(sum = sum(n))%>%
  mutate(Proportion = n/sum)%>%
  ggplot(aes(Signal, Proportion, group = Status))+
  geom_col(aes(fill = Status), position = "dodge")+
  scale_fill_manual(values=c("steelblue", "orangered")) +
  theme_bw()+
  geom_text(aes(label = scales::percent(Proportion)), position = position_dodge(0.9), vjust=-0.1) + 
  scale_y_continuous(labels = percent)+
  labs(x = "Traffic Signal",
       y = "Probability")
  
  

df %>%
  ggplot(aes(x= Side, fill = Status))+
  geom_bar( color="#e9ecef", alpha=0.4, position = 'identity') +
  scale_fill_manual(values=c("steelblue", "orangered")) +
  theme_bw() +
  labs(fill="")+
  ggtitle("Density of Accident Severity by Road Side")

##Subset a small sample for faster result
df <- df[sample(1:nrow(df), 5000, replace = F),]

##Partition data to train, validation, test
set.seed(1)
df_parts <- resample_partition(df, c(train = 0.8, test = 0.2))
train <- as_tibble(df_parts$train)
test <- as_tibble(df_parts$test)
##Logistic Regression
train$Status[train$Status=="Severe"]<- 1
train$Status[train$Status=="Not Severe"]<- 0
train$Status <- as.numeric(train$Status)
test$Status[test$Status=="Severe"]<- 1
test$Status[test$Status=="Not Severe"]<- 0
test$Status <- as.numeric(test$Status)




logit <- glm(Status ~., data = train, family = binomial)
logit_aic <- step(logit, trace = 0)
round(summary(logit_aic)$coef, dig=3)
logit_aic$anova[2:nrow(logit_aic$anova), c(1, 6)] %>% as_tibble() %>% mutate(Step = str_sub(Step, start = 3)) %>%
  rename("Vaiables to drop" = Step)
logit_aic$call


# prediction 
logit_pred = (predict(logit_aic, test[, 1:19])>0)
pred_table =table(logit_pred, test$Status)
rownames(pred_table)<- c("0", "1")
cm <-caret::confusionMatrix(pred_table)
tibble("Accuracy" = cm$overall[[1]], "Sensitivity" = cm$byClass[[1]],
       "Specificity" = cm$byClass[[2]], "Positive term" = cm$positive)

#Sparse Logistic Regression (Logistic with lasso penalty)
library(glmnet)
x<- model.matrix(Status~., data = train)
test.x <- model.matrix(Status~., data = test)

model_total <- glmnet(x, train$Status, family = "binomial")
plot(model_total)
model_lambda <- cv.glmnet(x, train$Status, family = "binomial")
plot(model_lambda)

model_total <- glmnet(x, train$Status, family = "binomial", lambda = model_lambda$lambda.min)
coef <- predict(model_total, type="coefficients")
sp.pred <- (predict(model_total, test.x)>0)
pred_table =table(sp.pred, test$Status)
rownames(pred_table)<- c("0", "1")
cm <-caret::confusionMatrix(pred_table)
tibble("Accuracy" = cm$overall[[1]], "Sensitivity" = cm$byClass[[1]],
       "Specificity" = cm$byClass[[2]], "Positive term" = cm$positive)


###boosting the logistic model
library(gbm)
gb <- gbm(formula = Status ~., distribution = "bernoulli", data = train, interaction.depth = 4, n.trees = 4500, shrinkage = 0.1, cv.folds = 3)
ntree_opt<- gbm.perf(gb, method = "cv")
print(gb)
gb_graph <- summary(gb)

gb_graph %>% ggplot(aes(x=rel.inf, y = reorder(var, rel.inf)))+
  geom_col(fill = "steelblue")+
  theme_bw()+
  labs(x = "Relative Influence", y = "Variables")


pred <- predict(object = gb, newdata = test, n.trees = 4800, type = "response")
gb.pred <- as.factor(ifelse(pred>0.5, 1, 0))
pred_table =table(gb.pred, test$Status)
cm <-caret::confusionMatrix(pred_table)
tibble("Accuracy" = cm$overall[[1]], "Sensitivity" = cm$byClass[[1]],
       "Specificity" = cm$byClass[[2]], "Positive term" = cm$positive)




##Decision Tree

model_decision <- rpart(Status ~ ., data = train, method = "class", minsplit = 10, cp = 0.022)
rpart.plot(model_decision, box.palette = "RdBu", shadow.col = "grey", tweak = 1.5)
plot(model_decision)
text(model_decision)
plotcp(model_decision)
model_decision$cptable 
prune(model_decision, cp = 0.041)
pred = predict(model_decision, test, type = "class")
pred_table =table(pred, test$Status)
cm <-caret::confusionMatrix(pred_table)
tibble("Accuracy" = cm$overall[[1]], "Sensitivity" = cm$byClass[[1]],
       "Specificity" = cm$byClass[[2]], "Positive term" = cm$positive)

##Random Forest
library(randomForest)
library(e1071)

model_rf <- tune.randomForest(x= train[ ,1:19], y= as.factor(train$Status), nodesize = seq(1,10,1), mtry = 7, ntree = 500, number = 5)
print(model_rf)
rf = randomForest(y=as.factor(train$Status), x= train[,1:19], mtry = 7, ntree = 500, nodesize = 1,random_state = 0)
rf.pred <- predict(rf, newdata = test[, 1:19])
pred_table =table(rf.pred, test$Status)
cm <-caret::confusionMatrix(pred_table)
tibble("Accuracy" = cm$overall[[1]], "Sensitivity" = cm$byClass[[1]],
       "Specificity" = cm$byClass[[2]], "Positive term" = cm$positive)
plot(rf, main = NULL)
rf.err <- rf$err.rate
ntrees <- seq(1:500)
rf.err <- as.data.frame(cbind(ntrees, rf.err))

rf.err <-  rf.err %>%
  rename("Not_Severe" = `0`, "Severe" = `1`)


rf.err %>% ggplot(aes(x=ntrees))+
  geom_line(aes(y = OOB), color = 'black', size = 1)+
  geom_line(aes(y=Not_Severe), color = 'steelblue',  size = 1)+
  geom_line(aes(y=Severe), color = 'orangered', size = 1)+
  theme_bw()+
  labs(x = "Number of Trees", y = "OOB Error", color = "Legend")+
  scale_color_manual(labels = c("OOB", "Severe", "Not_Severe"))


##MeanDecreaseGini value
rf.fit = randomForest(y=as.factor(train$Status), x= train[,1:19], mtry = 7, ntree = 500, nodesize = 10, importance = TRUE)
barplot(importance(rf.fit)[,4], ylab = "MeanDecreaseGini value", xlab = "Covariates", main = "Barplot of MeanDecreaseGini value")

importance <- as.data.frame(importance(rf.fit))
graph1 <- as.data.frame(cbind(Covariates=rownames(importance), MeanDecreaseGini =importance$MeanDecreaseGini))
graph1$MeanDecreaseGini <- as.numeric(as.character(graph1$MeanDecreaseGini))


graph1 %>%
  ggplot(aes( y = MeanDecreaseGini, x= reorder(Covariates, MeanDecreaseGini))) +
  geom_col(fill = "steelblue")+
  theme_bw()+
  labs(y = "Mean Decrease Gini", x = "Variables")+
  coord_flip()





###Linear SVM
library(e1071)
#Fit Linear SVM
lsvm <- svm(Status ~. , data = train,type='C-classification', kernel='linear',scale=FALSE, cost =0.1)
lsvm.pred <- predict(lsvm, test[, 1:19], decision.values = TRUE)
pred_table =table(lsvm.pred, test$Status)
cm <-caret::confusionMatrix(pred_table)
tibble("Accuracy" = cm$overall[[1]], "Sensitivity" = cm$byClass[[1]],
       "Specificity" = cm$byClass[[2]], "Positive term" = cm$positive)

#Fit Non-Linear SVM
nsvm <- svm(Status ~. , data = train,type='C-classification', kernel='radial',scale=FALSE, cost =1)
nsvm.pred <- predict(nsvm, test[, 1:19], decision.values = TRUE)
pred_table =table(nsvm.pred, test$Status)
cm <-caret::confusionMatrix(pred_table)
tibble("Accuracy" = cm$overall[[1]], "Sensitivity" = cm$byClass[[1]],
       "Specificity" = cm$byClass[[2]], "Positive term" = cm$positive)
