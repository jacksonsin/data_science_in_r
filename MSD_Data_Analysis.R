#import libraries
#install.packages("xtable")
#install.packages("htmltools")
#install.packages("htmlTable")
#install.packages("lubridate")
#install.packages("dplyr")                        
#install.packages("plotly") 
library(xtable)
library(htmltools)
library(htmlTable)
library(ggplot2)
library(lubridate)
library(dplyr) 
library(plotly)
library(scales)

# Set working directory
setwd("C:/Users/Jackson/Desktop/DataScientistAssessment")

# Load dataset
sales <- read.csv("data_20160405.csv")

# Display first 5 rows
head(sales, n=5)

# Summary stats for all variables in dataset
summary(sales) 

# Check for missing values
sum(is.na(sales)) # no missing value

# Number of records with negative sales
sum(sales$sales<0) #12

# Number of records with positive or 0 sales
sum(sales$sales>=0) # 2964

# Remove 12 records of negative sales
sales <- sales[sales$sales>=0,]
nrow(sales) # 2964

# Drop the 2013 time period 
new_sales  <- sales[!grepl("2013", sales$month),]
nrow(new_sales) #2594

# Aggregate sales, strategy1 to 3 by month
data_aggr <- new_sales %>%                         
  group_by(month) %>% 
  dplyr::summarize(sales=sum(sales),strategy1=sum(strategy1),strategy2=sum(strategy2),strategy3=sum(strategy3),compBrand=max(compBrand)) %>%
  as.data.frame()
data_aggr

# Slide 1 - Average marginal impact of each marketing strategy
total_sales <- sum(data_aggr$sales)
strategy1 <- sum(data_aggr$strategy1)
strategy2 <- sum(data_aggr$strategy2)
strategy3 <- sum(data_aggr$strategy3)
marketing_expediture <- sum(data_aggr$strategy1+data_aggr$strategy2+data_aggr$strategy3)

# ROI of strategy 1+2+3
ROI0 = (total_sales - marketing_expediture) / marketing_expediture
ROI0

# ROI of strategy 1
ROI1 = total_sales / strategy1 - 1
ROI1

# ROI of strategy 2
ROI2 = total_sales / strategy2 - 1
ROI2

# ROI of strategy 3
ROI3 = total_sales / strategy3 - 1
ROI3

# Plot bar graph - Marketing ROI
Strategy <- factor(c("Strategy 1","Strategy 2","Strategy 3"))
ROI <- c(ROI1,ROI2,ROI3)
df <- data.frame(Strategy, ROI)

fig <- plot_ly(df, x = Strategy[1], y = ROI[1], type = 'bar', main = "ROI from Marketing Expenditure", name = 'Strategy 1', text = round(ROI[1],1), textposition = 'top center')
fig <- fig %>% add_trace(x = Strategy[2], y = ROI[2], name = 'Strategy 2', text = round(ROI[2],1), textposition = 'top center')
fig <- fig %>% add_trace(x = Strategy[3], y = ROI[3], name = 'Strategy 3', text = round(ROI[3],1), textposition = 'top center')
fig <- fig %>% layout(yaxis = list(title = 'ROI', barmode = 'group')) 
fig <- fig %>% layout(title = list(text='ROI from Marketing Strategies', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig

# Plot bar graph - Marketing Expediture
Strategy <- factor(c("Strategy 1","Strategy 2","Strategy 3"))
Expenditure <- c(strategy1,strategy2,strategy3)
df <- data.frame(Strategy, Expenditure)

fig <- plot_ly(df, x = Strategy[1], y = Expenditure[1], type = 'bar', main = "ROI from Marketing Expenditure", name = 'Strategy 1', text = dollar(Expenditure[1]), textposition = 'top center')
fig <- fig %>% add_trace(x = Strategy[2], y = Expenditure[2], name = 'Strategy 2', text = dollar(Expenditure[2]), textposition = 'top center')
fig <- fig %>% add_trace(x = Strategy[3], y = Expenditure[3], name = 'Strategy 3', text = dollar(Expenditure[3]), textposition = 'top center')
fig <- fig %>% layout(yaxis = list(title = 'Marketing Expenditure', barmode = 'group')) 
fig <- fig %>% layout(title = list(text='Marketing Expenditure', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig

# Average marginal impact strategy 1
#Margin = (total_sales - strategy1) / total_sales
#Margin
Average_Margin1 = (total_sales - strategy1) / nrow(data_aggr)

# Average marginal impact strategy 2
#Margin = (total_sales - strategy2) / total_sales
#Margin
Average_Margin2 = (total_sales - strategy2) / nrow(data_aggr)

# Average marginal impact strategy 3
#Margin = (total_sales - strategy3) / total_sales
#Margin
Average_Margin3 = (total_sales - strategy3) / nrow(data_aggr)

# Plot bar graph - Marketing Expediture
Strategy <- factor(c("Strategy 1","Strategy 2","Strategy 3"))
Average_Margin <- c(Average_Margin1,Average_Margin2,Average_Margin3)
df <- data.frame(Strategy, Average_Margin)

fig <- plot_ly(df, x = Strategy[1], y = Average_Margin[1], type = 'bar', main = "Average Marginal Impact", name = 'Strategy 1', text = dollar(Average_Margin[1]), textposition = 'top center')
fig <- fig %>% add_trace(x = Strategy[2], y = Average_Margin[2], name = 'Strategy 2', text = dollar(Average_Margin[2]), textposition = 'top center')
fig <- fig %>% add_trace(x = Strategy[3], y = Average_Margin[3], name = 'Strategy 3', text = dollar(Average_Margin[3]), textposition = 'top center')
fig <- fig %>% layout(yaxis = list(title = 'Margin', barmode = 'group')) 
fig <- fig %>% layout(title = list(text='Average Marginal Impact', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig

# Plot line chart - Marketing and Sales
Year <- ymd(data_aggr$month)
Sales <- as.numeric(data_aggr$sales) 
Strategy1 <- as.numeric(data_aggr$strategy1)
Strategy2 <- as.numeric(data_aggr$strategy2)
Strategy3 <- as.numeric(data_aggr$strategy3)
df <- data.frame(Year, Sales,Strategy1,Strategy2,Strategy3)

fig <- plot_ly(df, x = Year, y = Sales, type = 'scatter', mode = 'lines', name="Sales")
fig <- fig %>% add_trace(y = Strategy1, name = 'Strategy1', yaxis = "y2", mode = 'lines') 
fig <- fig %>% add_trace(y = Strategy2, name = 'Strategy2', yaxis = "y2", mode = 'lines') 
fig <- fig %>% add_trace(y = Strategy3, name = 'Strategy3', yaxis = "y2", mode = 'lines') 
fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
fig <- fig %>% layout(yaxis = list(rangemode = "tozero"),yaxis2 = list(rangemode = "tozero"))
fig <- fig %>% layout(yaxis = list(title = 'Sales', barmode = 'line')) 
fig <- fig %>% layout(yaxis2 = list(title = 'Marketing Expediture', barmode = 'line')) 
fig <- fig %>% layout(title = list(text='Sales vs Marketing Expediture', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig

# Plot line chart - Marketing Strategy 1 and Sales
fig <- plot_ly(df, x = Year, y = Sales, type = 'scatter', mode = 'lines', name="Sales")
fig <- fig %>% add_trace(y = Strategy1, name = 'Strategy1', yaxis = "y2", mode = 'lines') 
fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
fig <- fig %>% layout(yaxis = list(rangemode = "tozero"),yaxis2 = list(rangemode = "tozero"))
fig <- fig %>% layout(yaxis = list(title = 'Sales', barmode = 'line')) 
fig <- fig %>% layout(yaxis2 = list(title = 'Marketing Expediture', barmode = 'line')) 
fig <- fig %>% layout(title = list(text='Sales vs Marketing Expediture', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig

# Plot line chart - Marketing Strategy 2 and Sales
fig <- plot_ly(df, x = Year, y = Sales, type = 'scatter', mode = 'lines', name="Sales")
fig <- fig %>% add_trace(y = Strategy2, name = 'Strategy2', yaxis = "y2", mode = 'lines') 
fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
fig <- fig %>% layout(yaxis = list(rangemode = "tozero"),yaxis2 = list(rangemode = "tozero"))
fig <- fig %>% layout(yaxis = list(title = 'Sales', barmode = 'line')) 
fig <- fig %>% layout(yaxis2 = list(title = 'Marketing Expediture', barmode = 'line')) 
fig <- fig %>% layout(title = list(text='Sales vs Marketing Expediture', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig

# Plot line chart - Marketing Strategy 3 and Sales
fig <- plot_ly(df, x = Year, y = Sales, type = 'scatter', mode = 'lines', name="Sales")
fig <- fig %>% add_trace(y = Strategy3, name = 'Strategy3', yaxis = "y2", mode = 'lines') 
fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
fig <- fig %>% layout(yaxis = list(rangemode = "tozero"),yaxis2 = list(rangemode = "tozero"))
fig <- fig %>% layout(yaxis = list(title = 'Sales', barmode = 'line')) 
fig <- fig %>% layout(yaxis2 = list(title = 'Marketing Expediture', barmode = 'line')) 
fig <- fig %>% layout(title = list(text='Sales vs Marketing Expediture', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig


# Slide 2 - New entrant
# Plot Sales Trend
Year <- ymd(data_aggr$month)
Sales <- as.numeric(data_aggr$sales)
compBrand <-as.numeric(data_aggr$compBrand)


df <- data.frame(Year, Sales, compBrand)
fig <- plot_ly(df, x = Year, y = Sales, type = 'scatter', mode = 'lines', name="Sales")
fig <- fig %>% add_trace(y = compBrand, name = 'compBrand', yaxis = "y2", mode = 'markers') 
fig <- fig %>% layout(yaxis2 = list(overlaying = "y", side = "right"))
fig <- fig %>% layout(yaxis = list(rangemode = "tozero"),yaxis2 = list(rangemode = "tozero"))
fig <- fig %>% layout(yaxis = list(title = 'Sales', barmode = 'line')) 
fig <- fig %>% layout(yaxis2 = list(title = 'No of Competitor', barmode = 'line')) 
fig <- fig %>% layout(title = list(text='Sales Trend with Competitor', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig

# Slide 3 - Seasonality YoY comparison
# Calculate total sales in 2014 Jan to May
sales_2014_jan_may <- data_aggr$sales[4:8]
sales_2014_jan_may <- sum(sales_2014_jan_may)
sales_2014_jan_may

# Calculate total sales in 2014 Jun to Sep
sales_2014_jun_sep <- data_aggr$sales[9:12]
sales_2014_jun_sep <- sum(sales_2014_jun_sep)
sales_2014_jun_sep

# Calculate total sales in 2015 before new competition
sales_2015_jan_may <- data_aggr$sales[16:20]
sales_2015_jan_may <- sum(sales_2015_jan_may)
sales_2015_jan_may

# Calculate total sales in 2015 after new competition
sales_2015_jun_sep <- data_aggr$sales[21:24]
sales_2015_jun_sep <- sum(sales_2015_jun_sep)
sales_2015_jun_sep

# YoY Growth before new competition
Growth_before_competition <- round(sales_2015_jan_may/sales_2014_jan_may-1,2)
Growth_before_competition
# YoY Growth after new competition 
Growth_after_competition <- round(sales_2015_jun_sep/sales_2014_jun_sep-1,2)
Growth_after_competition

# Plot bar graph - Sales Before New Competitor
Year <- factor(c("2014_Jan-May","2015_Jan-May"))
Sales <- c(sales_2014_jan_may,sales_2015_jan_may)
df <- data.frame(Year, Sales)

fig <- plot_ly(df, x = Year[1], y = Sales[1], type = 'bar', main = "Sales Before New Competiton", name = '2014 Sales ', text = dollar(Sales[1]), textposition = 'top center')
fig <- fig %>% add_trace(x = Year[2], y = Sales[2], name = '2015 Sales', text = dollar(Sales[2]), textposition = 'top center')
fig <- fig %>% layout(yaxis = list(title = 'Sales', barmode = 'group')) 
fig <- fig %>% layout(title = list(text='Sales Before New Competiton', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig

# Plot bar graph - Sales After New Competiton
Year <- factor(c("2014_Jun-Sep","2015_Jun-Sep"))
Sales <- c(sales_2014_jun_sep,sales_2015_jun_sep)
df <- data.frame(Year, Sales)

fig <- plot_ly(df, x = Year[1], y = Sales[1], type = 'bar', main = "Sales After New Competiton", name = '2014 Sales ', text = dollar(Sales[1]), textposition = 'top center')
fig <- fig %>% add_trace(x = Year[2], y = Sales[2], name = '2015 Sales', text = dollar(Sales[2]), textposition = 'top center')
fig <- fig %>% layout(yaxis = list(title = 'Sales', barmode = 'group')) 
fig <- fig %>% layout(title = list(text='Sales After New Competiton', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig

# Slide 4
# Calculate total sales in 2014
sales_2014 <- data_aggr$sales[4:15]
sales_2014 <- sum(sales_2014)
sales_2014

# Estimate Potential loss of sales 
loss_sales <- sales_2014*(Growth_after_competition-Growth_before_competition)
loss_sales

# Estimate Full Year Sales Before New Competition
FYE_no_new_competition <- sales_2014*(Growth_before_competition+1)
FYE_no_new_competition

# Estimate Full Year Sales After New Competition
FYE_new_competition <- sales_2014*(Growth_after_competition+1)
FYE_new_competition

# Plot bar graph - Sales After New Competiton
Year <- factor(c("Without New Entrant","With New Entrant"))
Sales <- c(FYE_no_new_competition,FYE_new_competition)
df <- data.frame(Year, Sales)
fig <- plot_ly(df, x = Year[1], y = FYE_no_new_competition, type = 'bar', main = "Full Year Estimate", name = 'Without New Entrant', text = dollar(FYE_no_new_competition), textposition = 'top center')
fig <- fig %>% add_trace(x = Year[2], y = FYE_new_competition, name = 'New Entrant', text = dollar(FYE_new_competition), textposition = 'top center')
fig <- fig %>% layout(yaxis = list(title = 'Sales', barmode = 'group')) 
fig <- fig %>% layout(title = list(text='Full Year Estimate', y = 0.95, x = 0.5, xanchor = 'center', yanchor =  'top'))
fig <- fig %>% layout(xaxis = list(categoryorder = "total descending"))
fig

# Compute correlation
corr_result <- round(cor(data_aggr[c("sales","strategy1","strategy2","strategy3")]),2)
print(htmlTable(corr_result), type="html")

#Correlation between sales and strategy 1
x <- data_aggr$sales
y <- data_aggr$strategy1
qplot(x,y)

#Correlation between sales and strategy 2
x <- data_aggr$sales
y <- data_aggr$strategy2
qplot(x,y) # Strong correlation

#Correlation between sales and strategy 3
x <- data_aggr$sales
y <- data_aggr$strategy3
qplot(x,y)

#Correlation between strategy 1 and strategy 2
x <- data_aggr$strategy1
y <- data_aggr$strategy2
qplot(x,y)
