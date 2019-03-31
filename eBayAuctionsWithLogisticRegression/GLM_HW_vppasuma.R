install.packages("readxl")
install.packages("dummies")
install.packages("caTools")
install.packages("reshape")

library('dummies')
library("fastDummies")
library("readxl")
library('caTools')
library("reshape")
library('qcc')

groupingFunction <- function(pivot_table) {
  pivot_table_rows <- dim(pivot_table)[1]
  pivot_table['parent'] <- pivot_table[1]
  for(i in 1:(pivot_table_rows-1)){
    for(j in (i+1):pivot_table_rows){
      if(abs(pivot_table[i,2] - pivot_table[j,2]) < 0.025){
        pivot_table[j, 'parent'] <- pivot_table[i, 'parent']
      } 
    }
  }
  return (pivot_table)
}

replaceSubValues <- function(original_df, parentTable, coloum) {
  rows_df = dim(original_df)[1]
  rows_parent = dim(parentTable)[1] 
  for(i in 1:rows_df){
    for(j in 1:rows_parent){
      if(parentTable[j, coloum] == original_df[i, coloum]){
        original_df[i, coloum] = parentTable[j, 'parent']
      }
    }
  }
  
  return (original_df)
}

auctionData <- read_excel("eBayAuctions.xls")

# Reference -- http://www.datasciencemadesimple.com/melting-casting-r/
auctionDataMelt <- melt(auctionData, measure.vars = "Competitive?")
auctionDataCastCategory <- cast(auctionDataMelt, Category  ~ variable , mean)
auctionDataCastCurrency <- cast(auctionDataMelt, currency  ~ variable , mean)
auctionDataCastDuration <- cast(auctionDataMelt, Duration  ~ variable , mean)
auctionDataCastEndDay <- cast(auctionDataMelt, endDay  ~ variable , mean)

auctionDataCastCategoryPub= groupingFunction(auctionDataCastCategory)
auctionDataCastCurrencyPub= groupingFunction(auctionDataCastCurrency)
auctionDataCastDurationPub= groupingFunction(auctionDataCastDuration)
auctionDataCastEndDayPub= groupingFunction(auctionDataCastEndDay)

auctionData <- replaceSubValues(auctionData, auctionDataCastCategoryPub, 'Category')
auctionData <- replaceSubValues(auctionData, auctionDataCastCurrencyPub, 'currency')
auctionData <- replaceSubValues(auctionData, auctionDataCastDurationPub, 'Duration')
auctionData <- replaceSubValues(auctionData, auctionDataCastEndDayPub, 'endDay')

# Performance: as.factor > factor when input is a factor
# Performance: as.factor > factor when input is integer
# Unused levels or NA levels

auctionData$Category <- as.factor(auctionData$Category)
auctionData$currency <- as.factor(auctionData$currency)
auctionData$Duration <- as.factor(auctionData$Duration)
auctionData$endDay <- as.factor(auctionData$endDay)

auctionDataDummy <- dummy_cols(auctionData)
auctionDataDummy <- auctionDataDummy[ , -which(names(auctionDataDummy) %in% c("Category","currency", "Duration", "endDay"))]

set.seed(135)
auctionDataDummy$spl=sample.split(auctionDataDummy$OpenPrice, SplitRatio=0.6)
train=subset(auctionDataDummy, spl==TRUE) 
test=subset(auctionDataDummy, spl==FALSE)

fit.all = glm(`Competitive?` ~ ., data = train, family = "binomial")
fit.all$coefficients
summary(fit.all)

## Question 1 ##
highestPredictorEstimate <- max(abs(fit.all$coefficients),na.rm = TRUE)

# The value 2.26 corresponds to `Category_Health/Beauty`. Therefore, fitting the model manually using `Category_Health/Beauty` variable
fit.single = glm(`Competitive?` ~ `Category_Health/Beauty` + currency_GBP + , data = train, family = "binomial")
fit.single$coefficients

## Question 2 ##
fit.four = glm(`Competitive?` ~ `Category_Health/Beauty` + currency_GBP + `Category_Coins/Stamps` + Category_EverythingElse , data = train, family = "binomial")
fit.four$coefficients
summary(fit.four)

## Question 4 ##
fit.reduced = glm(`Competitive?` ~ ClosePrice + OpenPrice + currency_GBP + endDay_Mon + `Category_Health/Beauty` , data = train, family = "binomial")
fit.reduced$coefficients
summary(fit.reduced)

## Reference - https://stats.stackexchange.com/questions/53312/comparing-two-models-using-anova-function-in-r
anova(fit.all,fit.reduced,test="Chisq")

## Question 5 ##
## Reference - https://www.rdocumentation.org/packages/qcc/versions/2.6/topics/qcc.overdispersion.test
tempSize <- rep(length(train$`Competitive?`), length(train$`Competitive?`))
qcc.overdispersion.test(train$`Competitive?`, size=tempSize, type="binomial")
