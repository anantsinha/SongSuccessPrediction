setwd("~/Developer/Analytics Edge/Week 3 Logistic Regression/Songs")
# Assignment for 15.071x Analytics Edge MIT
songs <- read.csv("songs.csv")
# Predict songs that would make it into Top 10 of the Billboard Hot 100 Chart
str(songs)
summary(songs)
# No. of Michael Jackson songs
nrow(subset(songs, artistname == "Michael Jackson"))
# No. of songs in 2010
nrow(subset(songs, year == 2010))

# Names of MJ songs that were in the Top 10
subset(songs, artistname == "Michael Jackson" & Top10 == 1)$songtitle

# No. of songs with each value of time signature 0,1,3,4,5 and 7
table(songs$timesignature)

# Song with Maximum tempo
songs$songtitle[which.max(songs$tempo)]

#splitting into train and test
train <- subset(songs, year <= 2009)
test <- subset(songs, year > 2009)
nrow(train)

nonvars <- c("year", "songtitle", "artistname", "songID", "artistID")
train <- train[, !(names(train) %in% nonvars)]
test <- test[, !(names(test) %in% nonvars)]

# first logistic regression model with all vars
model <- glm(Top10~., data = train, family = "binomial")
summary(model)
# summary shows -ve coefficient for energy - counterintuitive 
# check for correlation b/w loudness and energy
cor(train$loudness, train$energy)

# second logistic regression model with all vars except loudness, due to high correlation
model2 <- glm(Top10 ~ .-loudness, data = train, family = "binomial")
# AIC = 4937.8
summary(model2)

# second logistic regression model with all vars except energy, due to high correlation
model3 <- glm(Top10 ~ .-energy, data = train, family = "binomial")
summary(model3)
# AIC = 4848.7

# make predictions on test data
prediction <- predict(model3, test, type = "response")

# Generate confusion matrix
table(test$Top10, prediction > 0.45)