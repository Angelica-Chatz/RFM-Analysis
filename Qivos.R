rm(list = ls(all = TRUE))
gc(reset = TRUE)
par(mar = c(1,1,1,1))
options(scipen = 999) # remove scientific annotation



library(data.table)

getwd()

paste0(getwd(),"/Qivos")
(DataFolderPath = paste0(getwd(),"/Qivos"))

# Check if our folder exists
file.exists(DataFolderPath)

# Get the files that are included in our folder path
list.files(DataFolderPath,pattern = ".csv")
rfm <- list.files(DataFolderPath,pattern = ".csv")

# create file path
rfmPath <- paste0(DataFolderPath,"/",rfm)

# load dataset
rfm <- fread(rfmPath, sep = ";", stringsAsFactors = FALSE)

# Descriptives summary
str(rfm)

summary(rfm)

# checking for NAs
sum(is.na(rfm))

# turning timestamp into Date feature
library(lubridate)
rfm$InvoiceDate <- date(ymd_hms(rfm$InvoiceDate))
 
# creating also Year-Month feature
rfm$ym <- format((rfm$InvoiceDate), "%Y-%m")

str(rfm)

# 'Daily Revenue' barplot
library(ggplot2)

DailySales <- ggplot(rfm, aes(InvoiceDate, GrandTotal)) + geom_bar(stat = "identity", fill = "purple") +
              theme_minimal() + ggtitle("Daily Sales")

DailySales


Sales_dist <- ggplot(rfm, aes(GrandTotal)) + geom_histogram(binwidth = 30, color = "black", fill = "purple") +
              ggtitle("Sales Distribution") + theme_minimal() 

Sales_dist



# Time Period of sales data 
max(rfm$InvoiceDate) - min(rfm$InvoiceDate) 

# RFM Analysis 
# Recency- The Last time a customer has bought the product from the company
# Frequency - The number of times the customer has purchased from the company
# Monetary - The amount he has spent in the given time period 


# we can use the current date as a reference point and perform several segmentations 

today <- as.Date("2017-06-19", "%Y-%m-%d")

daily_table <- rfm[,list(
  Totalamount = sum(GrandTotal)),
  by = "InvoiceDate"
  ]

Month_table <- rfm[,list(
  TotalOrder = length(InvoiceId),
  Totalamount = sum(GrandTotal),
  AVGamount = round(mean(GrandTotal), digits = 2)),
  by = "ym"
  ]


TotalSales <- ggplot(data = Month_table, aes(ym, Totalamount, group = 1)) +
  geom_line(color = "purple") +  geom_point() + ylim(0, 3600000) + theme_minimal() + ggtitle("Monthly Sales")

TotalSales


AvgSales <- ggplot(Month_table, aes(ym, AVGamount)) + geom_bar(stat = "identity", fill = "purple") +
  theme_minimal() + ggtitle("Monthly avgSales")

AvgSales

TotalOrders <- ggplot(data = Month_table, aes(ym, TotalOrder, group = 1)) +
  geom_line(color = "purple") +  geom_point() + ylim(0, 100000) + theme_minimal() + ggtitle("Monthly Orders")

TotalOrders


RFM_table <- rfm[,list(
  R = as.numeric(today - max(InvoiceDate)),
  F = length(InvoiceId),
  M = sum(GrandTotal)),
  by = "LoyaltyMemberId"
  ]



# rankR 1 is very recent while rankR 5 is least recent
RFM_table$rankR <- copy(RFM_table$R)
RFM_table$rankR <- findInterval(RFM_table$rankR, quantile(RFM_table$rankR, c(0.0, 0.25, 0.50, 0.75, 1.0)))
table(RFM_table$rankR)

# rankF 1 is least frequent while rankF 5 is most frequent
RFM_table$rankF <- copy(RFM_table$F)
RFM_table$rankF <- findInterval(RFM_table$rankF, quantile(RFM_table$rankF, c(0.0, 0.25, 0.50, 0.75, 1.0)))
table(RFM_table$rankF)

#rankM 1 is lowest sales while rankM 5 is highest sales
RFM_table$rankM <- copy(RFM_table$M)
RFM_table$rankM <- findInterval(RFM_table$rankM, quantile(RFM_table$rankM, c(0.0, 0.25, 0.50, 0.75, 1.0)))
table(RFM_table$rankM)



# weighted RFM Score
RFM_table$RFMScore <- as.numeric(0.2*RFM_table$rankR + 0.3*RFM_table$rankF + 0.5*RFM_table$rankM)
table(RFM_table$RFMScore)

# Final RFM segmentation of customers according to instructions given
RFM_table$RFMsegment <- copy(RFM_table$RFMScore)
RFM_table$RFMsegment <- findInterval(RFM_table$RFMsegment, quantile(RFM_table$RFMsegment, c(0.152, 0.292, 0.592, 0.842, 0.992)))

# RFM_table$RFMsegment2 <- findInterval(RFM_table$RFMScore2, quantile(RFM_table$RFMScore2, c(0.152, 0.292, 0.592, 0.842, 0.992),names = TRUE))
(counts <- table(RFM_table$RFMsegment))

RFM_table2 <- RFM_table[,list(
  sumGrandTotal = sum(M),
  sumTransactions = sum(F),
  AVGbasket = round(mean(M), digits = 2)),
  by = "RFMsegment"
  ]

# Customer Segments Pie chart
colors <- c("white", "yellow", "brown", "grey", "gold", "green") 
lbls <- paste(c("WHITE", "YELLOW", "BRONZE", "SILVER", "GOLD", "STAR"), "\n", counts, sep = "")
pie(counts, labels = lbls,  col = colors,
    main = "Pie Chart of Customer Segments")

# getting customer ID and characteristics for each segment
RFM_star <- RFM_table[RFM_table$RFMsegment == 5]
RFM_gold <- RFM_table[RFM_table$RFMsegment == 4]
RFM_silver <- RFM_table[RFM_table$RFMsegment == 3]
RFM_bronze <- RFM_table[RFM_table$RFMsegment == 2]
RFM_yellow <- RFM_table[RFM_table$RFMsegment == 1]
RFM_white <- RFM_table[RFM_table$RFMsegment == 0]

# checking the main characteristics that each group shares

### STAR ###
table(RFM_star$rankR)
table(RFM_star$rankF)
table(RFM_star$rankM)

### GOLD ###
table(RFM_gold$rankR)
table(RFM_gold$rankF)
table(RFM_gold$rankM)

### SILVER ###
table(RFM_silver$rankR)
table(RFM_silver$rankF)
table(RFM_silver$rankM)

### BRONZE ###
table(RFM_star$rankR)
table(RFM_star$rankF)
table(RFM_star$rankM)

### YELLOW ###
table(RFM_yellow$rankR)
table(RFM_yellow$rankF)
table(RFM_yellow$rankM)

### WHITE ###
table(RFM_white$rankR)
table(RFM_white$rankF)
table(RFM_white$rankM)
