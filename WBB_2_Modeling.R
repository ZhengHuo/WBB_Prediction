# 
# Arthur Ye, Athena Huo, Helen Huang, Jonathan Xiao -- 08/23/2020
# NCAA Women's Basketball 2014-15 through 2018-19
# Prediction of Women's College Basketball Teams' Strengths and the "Home Court 
# Advantage" by Linear Model
#

### In this script, we will firstly build a basic model to predict the team 
# strength ignoring the "home court advantage" and then update the first model
# by including the "home court advantage" in it to form a final Model.

x <- read.csv("WBB_Cleaned.csv", as.is = TRUE)

## Due to the large amount of data in five seasons, We will choose season 
# 2018-2019 to do our analysis:
x <- x[x$season == "2018-2019", ]

## Using Dummy-coding Indicator Varibles and constructing a model matrix:
mm <- matrix(0, nrow(x), length(unique(c(x$teamname, x$opponents))))
colnames(mm) <- sort(unique(c(x$teamname, x$opponents)))

# In order to find the score differential, we assign 1 to each primary team and
# -1 to each opponent team.
for (i in 1:length(colnames(mm))) {
  mm[x$teamname == colnames(mm)[i], i] <- 1 
  mm[x$opponents == colnames(mm)[i], i] <- -1 
}

# Choose "A&M-Corpus Christi" (the first team) as the base line:
mm <- mm[, -1]

## Basic Linder Model:
lm.1 <- lm(x$scorediff ~ -1 + ., as.data.frame(mm))

sort(round(lm.1$coefficients, 2))
## Top three teams: Baylor 53.49, Notre Dame 52.44, Mississippi St. 50.49
## Last three teams: Chicago St. -24.84, Ark.-Pine Bluff -20.03, Alcorn -17.80

### Next, consider the home court advantage:

## Add one variable "place" to "mm", and the coefficient of "place" represents
# the home court advantage.
mm2 <- cbind(mm, 0)
colnames(mm2)[ncol(mm2)] <- "place"

# we assign 1 to each home game and -1 to each away.
# The neutral site game would be 0.
mm2[which(x$place == "home"), ncol(mm2)] <- 1
mm2[which(x$place == "away"), ncol(mm2)] <- -1

## Final Linder Model:
lm.2 <- lm(x$scorediff ~ -1 + ., as.data.frame(mm2))
round(lm.2$coefficients, 2)

# The home court advantage is about 2.95

## Predicting the score differential of each game:
x$predict <- predict(lm.2, as.data.frame(mm2))

## Check the distribution pattern of the model's residuals:
qqnorm(lm.2$residuals) 

# The residuals are normally distributed since it is nearly a straight line, 
# which indicates that our model is generally valid.

### Next, we fit the model only using games through the end of February, and
# then use this model to predict game results for March and April.

## Create earlyrows to identify all games played through the end of February:

x$Date <- as.Date(x$dates, "%m/%d/%Y")
table(x$Date < as.Date("2019-03-01"))

earlyrows <- x$Date < as.Date("2019-03-01")

lm.2early <- lm(x$scorediff[earlyrows] ~ -1 + ., 
                as.data.frame(mm2[earlyrows, ]))
x$predictearly <- round(predict(lm.2early, as.data.frame(mm2)), 2)

## The correlation between the score differential in each row in x and the
# predicted score differential of that row.
plot(x$scorediff[!earlyrows] ~ x$predictearly[!earlyrows], x, 
     xlab = "Predicted Results", ylab = "Score Difference")
lm.bb <- lm(scorediff[!earlyrows] ~ predictearly[!earlyrows], x)
abline(lm.bb, col = "red")
### This model could explain about 51% of the variability of the score 
# differences, and the predicted result might often be different from the real 
# result by approximately one or two standard deviations, or 11 to 22 points, 
# meaning the point on the plot could be 11 or 22 points above or below the red 
# line.