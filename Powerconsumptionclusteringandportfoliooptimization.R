---
title: "Power Consumption Clustering and Portfolio Optimization: An R Analysis"
author: "Dhruv Sharma"
date: "2025-05-13"
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:


```{r}
#
# Electricity patterns
# Time series, one day, equal spacing, 30 mins per sample
# Starts at midnight
#

# Load the data from CSV (no header in file)
p <- read.csv("power.csv", header = FALSE)

# Since first column is the household ID, make it row names and remove it from data
rownames(p) <- p[,1]                     # Household ID as row names
p <- p[,2:ncol(p)]                       # Remove ID column

# Rename columns to time points (1 to 48, each representing 30-minute interval)
colnames(p) <- seq(from = 1, to = 48, by = 1)

# Plotting: show both the aggregate boxplot and one individual household usage
par(mfrow = c(1, 2))                     # 1 row, 2 columns of plots
par(mar = c(5, 5, 1, 1))                 # Margins: bottom, left, top, right

# Boxplot of usage across all households at each time slot
boxplot(p,
        xlab = "Time",
        ylab = "Electricity Usage (kWh)",
        main = "All Households")

# Line plot of usage for one household (e.g., the second one)
plot(as.numeric(p[2,]),
     type = "l",
     xlab = "Time",
     ylab = "Electricity Usage (kWh)",
     main = "One Household")

# Reset plot window
par(mfrow = c(1, 1))


```


#The power.R script loads half-hourly electricity usage data for 241 households and produces a boxplot that summarizes usage at each 30-minute interval over a 24-hour period. Each box in the plot shows the median, quartiles, and spread of usage across households at a given time of day.

#From the boxplot, a clear diurnal pattern is visible:

#Lowest usage occurs between midnight and early morning (around 2–5 AM).

#Usage starts increasing around 6–7 AM, reflecting morning activity (e.g., heating, breakfast).

#There's a midday dip, likely when households are unoccupied.

#A second and more pronounced peak appears in the evening hours, around 5–9 PM, consistent with people returning home.

#After 10 PM, usage declines again, tapering off toward midnight.

#This pattern indicates two daily peaks in residential electricity demand: one in the morning, and a larger one in the evening.

#The line plot of a single household shows how an individual household’s usage aligns (or doesn't) with the overall pattern, and can vary in intensity or timing.






```{r}
# ---------------------------------------------
# Load electricity usage data
# ---------------------------------------------
power.csv <- read.csv("power.csv", header = FALSE)
rownames(power.csv) <- power.csv[, 1]           # Household IDs as row names
power.csv <- power.csv[, 2:ncol(power.csv)]     # Remove ID column
colnames(power.csv) <- seq(1, 48, 1)            # Label columns 1 to 48
p <- power.csv                                  # Optional alias

# ---------------------------------------------
# Function to apply a function to selected columns
# ---------------------------------------------
simple.fn <- function(x, fn, cols = 1:length(x)) {
  fn(x[cols])
}

# ---------------------------------------------
# Feature builder using time blocks
# ---------------------------------------------
build.table <- function(p) {
  tab <- apply(p[,1:48], 1, simple.fn, mean)                           # Average
  tab <- cbind(tab, apply(p[,1:48], 1, simple.fn, var))               # Variance
  tab <- cbind(tab, apply(p[,1:48], 1, simple.fn, sum))               # Total
  tab <- cbind(tab, apply(p[,14:18], 1, simple.fn, var))              # Morning peak var
  tab <- cbind(tab, apply(p[,35:42], 1, simple.fn, var))              # Evening peak var
  tab <- cbind(tab, apply(p[,c(43:48, 1:6)], 1, simple.fn, var))      # Off-peak 1 var
  tab <- cbind(tab, apply(p[,19:33], 1, simple.fn, var))              # Off-peak 2 var
  tab <- cbind(tab, apply(p[,14:18], 1, simple.fn, sum))              # Morning peak sum
  tab <- cbind(tab, apply(p[,35:42], 1, simple.fn, sum))              # Evening peak sum
  tab <- cbind(tab, apply(p[,c(43:48, 1:6)], 1, simple.fn, sum))      # Off-peak 1 sum
  tab <- cbind(tab, apply(p[,19:33], 1, simple.fn, sum))              # Off-peak 2 sum
  
  colnames(tab) <- c("Average consumption",
                     "Variation in consumption",
                     "Total consumption",
                     "Variation in consumption during morning peak hour", 
                     "Variation in consumption during evening peak hour",
                     "Variation in consumption during first off-peak hour",
                     "Variation in consumption during second off-peak hour",
                     "Total consumption during morning peak hour", 
                     "Total consumption during evening peak hour",
                     "Total consumption during first off-peak hour",
                     "Total consumption during second off-peak hour")
  
  as.data.frame(tab)
}

# ---------------------------------------------
# Build feature table
# ---------------------------------------------
power.csv.tab <- build.table(p)

# ---------------------------------------------
# Visualise constructed features
# ---------------------------------------------
head(power.csv.tab)

# Boxplot of all features (scaled)
boxplot(scale(power.csv.tab),
        main = "Boxplots of Scaled Features",
        las = 2)

# Pairwise scatterplot of all features (scaled)
pairs(scale(power.csv.tab),
      main = "Pairwise Scatterplots of Features")

```
```{r}
# -------------------------------------------------------------
# Q3: K-Means Clustering and Usage Pattern Visualization
# -------------------------------------------------------------

# -------------------------------------------------------------
# Step 1: Define the clustering function
# -------------------------------------------------------------
do.cluster <- function(power.table, num.clusters = 6) {
  kmeans(scale(power.table), centers = num.clusters)
}

# -------------------------------------------------------------
# Step 2: Apply clustering to feature table
# -------------------------------------------------------------
power.csv.cluster <- do.cluster(power.csv.tab)
power.csv.cluster.df <- as.data.frame(power.csv.cluster$cluster)

# View distribution of households across clusters
hist(power.csv.cluster$cluster,
     xlab = "Cluster",
     main = "Number of households in each cluster")

# -------------------------------------------------------------
# Step 3: Assign cluster labels to raw usage data
# -------------------------------------------------------------
# Use rownames to match clusters back to original usage rows
cluster1 <- as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster == 1)])
cluster1.explanatory <- power.csv[row.names(cluster1),]

cluster2 <- as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster == 2)])
cluster2.explanatory <- power.csv[row.names(cluster2),]

cluster3 <- as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster == 3)])
cluster3.explanatory <- power.csv[row.names(cluster3),]

cluster4 <- as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster == 4)])
cluster4.explanatory <- power.csv[row.names(cluster4),]

cluster5 <- as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster == 5)])
cluster5.explanatory <- power.csv[row.names(cluster5),]

cluster6 <- as.data.frame(power.csv.cluster$cluster[which(power.csv.cluster$cluster == 6)])
cluster6.explanatory <- power.csv[row.names(cluster6),]

# -------------------------------------------------------------
# Step 4: Boxplots for each cluster’s raw usage
# -------------------------------------------------------------
par(mfrow = c(3, 2))
boxplot(cluster1.explanatory, main = "Cluster 1",
        xlab = "Time Slot", ylab = "kWh", ylim = c(0, 3))
boxplot(cluster2.explanatory, main = "Cluster 2",
        xlab = "Time Slot", ylab = "kWh", ylim = c(0, 3))
boxplot(cluster3.explanatory, main = "Cluster 3",
        xlab = "Time Slot", ylab = "kWh", ylim = c(0, 3))
boxplot(cluster4.explanatory, main = "Cluster 4",
        xlab = "Time Slot", ylab = "kWh", ylim = c(0, 3))
boxplot(cluster5.explanatory, main = "Cluster 5",
        xlab = "Time Slot", ylab = "kWh", ylim = c(0, 3))
boxplot(cluster6.explanatory, main = "Cluster 6",
        xlab = "Time Slot", ylab = "kWh", ylim = c(0, 3))
par(mfrow = c(1, 1))

# -------------------------------------------------------------
# Step 5: Line plots of average usage patterns for each cluster
# -------------------------------------------------------------
par(mfrow = c(3, 2))
plot(apply(cluster1.explanatory, 2, mean), type = "l", main = "Cluster 1",
     xlab = "Time Slot", ylab = "Avg kWh", ylim = c(0, 2))
plot(apply(cluster2.explanatory, 2, mean), type = "l", main = "Cluster 2",
     xlab = "Time Slot", ylab = "Avg kWh", ylim = c(0, 2))
plot(apply(cluster3.explanatory, 2, mean), type = "l", main = "Cluster 3",
     xlab = "Time Slot", ylab = "Avg kWh", ylim = c(0, 2))
plot(apply(cluster4.explanatory, 2, mean), type = "l", main = "Cluster 4",
     xlab = "Time Slot", ylab = "Avg kWh", ylim = c(0, 2))
plot(apply(cluster5.explanatory, 2, mean), type = "l", main = "Cluster 5",
     xlab = "Time Slot", ylab = "Avg kWh", ylim = c(0, 2))
plot(apply(cluster6.explanatory, 2, mean), type = "l", main = "Cluster 6",
     xlab = "Time Slot", ylab = "Avg kWh", ylim = c(0, 2))
par(mfrow = c(1, 1))

```
```{r,width=10,fig.height=6,echo=FALSE}
#do correlation between explanatories 
library(tibble)
library(corrplot)
library(gclus)
library(grid)
library(gridExtra)
library(ggcorrplot)

power.tab.cor <- abs(cor(power.csv.tab))  # Take absolute value of correlation
power.tab.colors <- dmat.color(power.tab.cor)
power.tab.order <- order.single(cor(power.csv.tab))  # Order by correlation


power.correlation<-ggcorrplot(power.tab.cor,
           type = "lower",
           outline.color = "black",
           lab= "TRUE") +
          labs(title = "Correlation matrix for power consumption across 250 households") +
  theme(title = element_text(size = 20)) +
  theme(axis.title.x = element_text(size = 10)) 
power.correlation
#pick the one that is not highly correlated
#1) 0.31- Variation in consumption during morning peak hour and average consumption during everning peak hour
#2) 0.20- Variation consumption during evening peak hour and average consumption during second off-peak hour
```

```{r,echo=FALSE}
#colouring each point by cluster number
power.csv.cluster.df<-as.data.frame(power.csv.cluster$cluster)
power.csv.cluster.df <- cbind(power.csv.cluster.df,power.csv.tab$`Variation in consumption during second off-peak hour`)
power.csv.cluster.df <- cbind(power.csv.cluster.df,power.csv.tab$`Variation in consumption during first off-peak hour`)
colnames(power.csv.cluster.df)<- c("cluster", "Variation consumption during second off-peak hour", "Variation consumption during first off-peak hour")
power.csv.cluster.df$cluster<-as.factor(power.csv.cluster.df$cluster)
#ggplot
weak.correlation<- ggplot(data=power.csv.cluster.df, aes(x = `Variation consumption during second off-peak hour`, y =`Variation consumption during first off-peak hour`)) + geom_point(aes(color=cluster))
weak.correlation
``` 


##Question 2 


```{r}
# -------------------------------
# Load and label investment data
# -------------------------------
invest <- read.table("invest.tab", header = TRUE)

# Label Type column
invest$Type <- factor(invest$Type, levels = c(1, 2, 3), labels = c("Stock", "Bond", "Cash"))

# ROI vs Risk Scatterplot
library(ggplot2)

ggplot(invest, aes(x = ROI, y = Risk, color = Type)) +
  geom_point(size = 3) +
  labs(title = "ROI vs Risk by Investment Type") +
  theme_minimal()

# -------------------------------
# Load and validate correlation matrix
# -------------------------------
correlation_tab <- read.table("corr.tab", header = FALSE)
correlation_mat <- as.matrix(correlation_tab)

# Apply labels
colnames(correlation_mat) <- rownames(correlation_mat) <- paste0("Inv", 1:ncol(correlation_mat))

# Plot with improved labels
library(ggcorrplot)

ggcorrplot(correlation_mat,
           type = "lower",
           lab = TRUE,
           lab_size = 2.5,
           outline.color = "white",
           colors = c("blue", "white", "red"),
           title = "Correlation Matrix for Investment Portfolio") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

```
#Part 2 

```{r}
###########################################
# ASSN 3
# invest.R
###########################################
#
# Build a multi-objective constrained model
# for an investment portfolio
###########################################

#install.packages("mco")
library(mco)

# The 30 investments that will be used to select
# a portfolio are in "invest.tab"
#
invest <- read.table("invest.tab")
#
# Determine number of options from the invest table
#
numberOptions <- nrow(invest)
#
# The format of invest is:
# Each row is an option
# Columns are : ROI (return on investment), Risk, Type
#              where Type = 1 (Stock), 2 (bond), 3 (cash)
#
#############################################################
# Each option from the table must either not be selected
# OR must be between an amount
#   minAMOUNT <= amount <= maxAMOUNT
#
# We will assume that anything < minAMOUNT
# is equivalent to zero (and therefore not included)
#############################################################
minAMOUNT = 0.05
maxAMOUNT = 0.2

##########################################
# CONSTRAINTS
##########################################
# The sum of the portfolios must be between
# 0.95 and 1.0
# Constraints are satisfied by being >= 0
#############################################
portfolioSUM <- function(x) {
  selected <- which(x >= minAMOUNT)
  sumx = sum(x[selected])
  if ((sumx >= 0.95) && (sumx <= 1.0)) return(1)  # OK
  return(-1)  # Fails constraint
}

###########################################################################
# Constraint - each selected option must be  >= minAMOUNT
#              and <= maxAMOUNT
# We ignore options with value < minAMOUNT because they
# aren't part of the final selection of investments.
###########################################################################
portfolioRANGE <- function(x) {
  selected <- which(x >= minAMOUNT)  # Only count if at least minimum
  over <- which(x[selected] > maxAMOUNT)
  if (length(over) > 0) return(-1)
  return(1)
}

##################################################################
# Constraints on the minimum and maximum number of selected
# stocks/bonds/cash
##################################################################
minNumber = 8
maxNumber = 12

portfolioNUM <- function(x) {
  numselected <- length(which(x >= minAMOUNT))
  if (numselected < minNumber) return(-1)
  if (numselected > maxNumber) return(-1)
  return(numselected)
}

############################################################
# Functions to be minimised/maximised
############################################################
###############################
# Return on Investment (ROI)
###############################
# This wants to be MAXIMISED
# Only include options that are greater than the minAMOUNT
#############################################################
ROI <- function(x) {
  selected <- which(x >= minAMOUNT)
  roi <- sum(invest$ROI[selected] * x[selected])
  return(-roi)  # Since nsgaII minimises we take the negative
}

###############################
# Placeholder for custom correlation function
###############################
mycorr <- function(x) {
  selected <- which(x >= minAMOUNT)
  # Add logic here if needed
}

#################################
# Risk (RISK)
#################################
# This is to be MINIMISED
# Only include options that are greater than the minAMOUNT
##############################################################
RISK <- function(x) {
  selected <- which(x >= minAMOUNT)
  risk <- sum(invest$Risk[selected] * x[selected])
  return(risk)
}

##################################################
# Here are the functions that are to be minimised
# Note ROI is actually maximised, while RISK is
# minimised.
###################################################
funs <- function(x) {
  return(c(ROI(x), RISK(x)))
}

######################################################
# Here are the constraints
# Since nsga2 assumes a single constraint function, we
# call each constraint in turn, and return the results
# of all the constraints as a concatenated list
######################################################
constraintFNS <- function(x) {
  psum = portfolioSUM(x)
  prange = portfolioRANGE(x)
  pnum = portfolioNUM(x)
  return(c(prange, pnum, psum))
}

#############################################################
# Set the lower and upper bounds for
# each investment option
# The lower bound is 0; upper bound is maxAMOUNT
#############################################################
lower = rep(0, numberOptions)
upper = rep(maxAMOUNT, numberOptions)

# Set seed for reproducibility
set.seed(1)

###########################################################
# CALL nsga2 to find the pareto optimal solutions
###########################################################
portfolio <- nsga2(funs,
                   idim = numberOptions,   # inputs for each option
                   odim = 2,               # outputs (ROI, RISK)
                   popsize = 52,
                   generations = 500,
                   lower.bounds = lower,
                   upper.bounds = upper,
                   constraints = constraintFNS,
                   cdim = 3)               # 3 constraints

######## Plot the pareto front using default plotting
plot(portfolio,
     xlab = "-ROI (%)",
     ylab = "RISK",
     main = "Objective Space")

```


```{r}
###########################################
# INFO424 Assignment 3 - Question 2
# Multi-objective Portfolio Optimization
###########################################

# Load packages
if (!require("mco")) install.packages("mco", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("gridExtra")) install.packages("gridExtra", dependencies = TRUE)

library(mco)
library(ggplot2)
library(gridExtra)

# Load data
invest <- read.table("invest.tab", header = TRUE)

# Convert Type to factor for readability
invest$Type <- factor(invest$Type, levels = c(1, 2, 3), labels = c("Stock", "Bond", "Cash"))

# Constants
minAMOUNT <- 0.05
maxAMOUNT <- 0.2
numberOptions <- nrow(invest)

# Constraints
portfolioSUM <- function(x) {
  selected <- which(x >= minAMOUNT)
  total <- sum(x[selected])
  if (total >= 0.95 && total <= 1.0) return(1)
  return(-1)
}
portfolioRANGE <- function(x) {
  selected <- which(x >= minAMOUNT)
  if (any(x[selected] > maxAMOUNT)) return(-1)
  return(1)
}
portfolioNUM <- function(x) {
  n <- length(which(x >= minAMOUNT))
  if (n < 8 || n > 12) return(-1)
  return(1)
}

# Objective functions
ROI <- function(x) {
  selected <- which(x >= minAMOUNT)
  return(-sum(invest$ROI[selected] * x[selected]))  # Maximize
}
RISK <- function(x) {
  selected <- which(x >= minAMOUNT)
  return(sum(invest$Risk[selected] * x[selected]))  # Minimize
}

funs <- function(x) c(ROI(x), RISK(x))
constraintFNS <- function(x) c(portfolioRANGE(x), portfolioNUM(x), portfolioSUM(x))

# Bounds
lower <- rep(0, numberOptions)
upper <- rep(maxAMOUNT, numberOptions)

# Optimize using NSGA-II
set.seed(42)
portfolio <- nsga2(funs,
                   idim = numberOptions,
                   odim = 2,
                   popsize = 52,
                   generations = 500,
                   lower.bounds = lower,
                   upper.bounds = upper,
                   constraints = constraintFNS,
                   cdim = 3)

# Initialize result table
Pareto.front.investmentBlends <- as.data.frame(matrix(nrow = 52, ncol = 9))

# Loop through solutions to compute composition
for (i in 1:52) {
  selected <- which(portfolio$par[i, ] > 0.05)
  x <- invest[selected, ]
  
  # Count each asset type
  Pareto.front.investmentBlends[i, 1] <- sum(x$Type == "Stock")
  Pareto.front.investmentBlends[i, 2] <- sum(x$Type == "Bond")
  Pareto.front.investmentBlends[i, 3] <- sum(x$Type == "Cash")
  
  # Composition percentages
  total_selected <- sum(Pareto.front.investmentBlends[i, 1:3])
  Pareto.front.investmentBlends[i, 4] <- (Pareto.front.investmentBlends[i, 1] / total_selected) * 100
  Pareto.front.investmentBlends[i, 5] <- (Pareto.front.investmentBlends[i, 2] / total_selected) * 100
  Pareto.front.investmentBlends[i, 6] <- (Pareto.front.investmentBlends[i, 3] / total_selected) * 100
  
  # Total ROI and Risk
  Pareto.front.investmentBlends[i, 7] <- sum(x$ROI)
  Pareto.front.investmentBlends[i, 8] <- sum(x$Risk)
}

# Set column names
colnames(Pareto.front.investmentBlends) <- c(
  "Stock_Count", "Bond_Count", "Cash_Count",
  "Stock_Percentage", "Bond_Percentage", "Cash_Percentage",
  "Total_ROI", "Total_Risk", "Risk_Level"
)

# Assign Risk Level using quantiles
risk_quantiles <- quantile(Pareto.front.investmentBlends$Total_Risk, probs = c(1/3, 2/3))
Pareto.front.investmentBlends$Risk_Level <- cut(
  Pareto.front.investmentBlends$Total_Risk,
  breaks = c(-Inf, risk_quantiles[1], risk_quantiles[2], Inf),
  labels = c("Low", "Medium", "High"),
  right = TRUE
)

# Convert to factor
Pareto.front.investmentBlends$Risk_Level <- factor(Pareto.front.investmentBlends$Risk_Level)

# Preview
head(Pareto.front.investmentBlends)

# ROI vs Risk scatterplot
ggplot(Pareto.front.investmentBlends, aes(x = Total_ROI, y = Total_Risk, color = Risk_Level)) +
  geom_point(size = 3) +
  labs(title = "Pareto Front: ROI vs Risk by Risk Category",
       x = "Total ROI",
       y = "Total Risk") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Histograms for asset types
p1 <- ggplot(Pareto.front.investmentBlends, aes(x = Stock_Count)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "black") +
  xlab("Number of Stocks") + ylab("Frequency") + xlim(0, 12) +
  theme_minimal()

p2 <- ggplot(Pareto.front.investmentBlends, aes(x = Bond_Count)) +
  geom_histogram(binwidth = 1, fill = "darkgreen", color = "black") +
  xlab("Number of Bonds") + ylab("Frequency") + xlim(0, 12) +
  theme_minimal()

p3 <- ggplot(Pareto.front.investmentBlends, aes(x = Cash_Count)) +
  geom_histogram(binwidth = 1, fill = "orange", color = "black") +
  xlab("Number of Cash Assets") + ylab("Frequency") + xlim(0, 12) +
  theme_minimal()

# Combine plots
grid.arrange(p1, p2, p3, ncol = 3, top = "Investment Types Across Pareto Solutions")

```
#Part-3 run check
```{r}
# Extracting the Pareto Front investment blends
# Assuming 'portfolio' is the result of nsga2 from previous step

# Risk levels in portfolio - we categorize into Low, Medium, High risk
risk_quantiles <- quantile(Pareto.front.investmentBlends$Total_Risk, probs = c(1/3, 2/3))

# Select low, medium, and high-risk portfolios
low_risk <- Pareto.front.investmentBlends[which(Pareto.front.investmentBlends$Total_Risk <= risk_quantiles[1]), ]
medium_risk <- Pareto.front.investmentBlends[which(Pareto.front.investmentBlends$Total_Risk > risk_quantiles[1] & Pareto.front.investmentBlends$Total_Risk <= risk_quantiles[2]), ]
high_risk <- Pareto.front.investmentBlends[which(Pareto.front.investmentBlends$Total_Risk > risk_quantiles[2]), ]

# Choose one portfolio from each category (just pick the first solution in each)
low_risk_solution <- low_risk[1,]
medium_risk_solution <- medium_risk[1,]
high_risk_solution <- high_risk[1,]

# Presenting the blends of stocks, bonds, and cash for each risk level
low_risk_solution[, c("Stock_Percentage", "Bond_Percentage", "Cash_Percentage")]
medium_risk_solution[, c("Stock_Percentage", "Bond_Percentage", "Cash_Percentage")]
high_risk_solution[, c("Stock_Percentage", "Bond_Percentage", "Cash_Percentage")]

```



```{r}
# Function to plot pie chart with cleaner labels, and improved styling
plot_pie <- function(data, risk_level) {
  # data = vector of percentages for stocks, bonds, and cash
  # risk_level = low, medium, or high
  pie_data <- data.frame(
    Type = c("Stocks", "Bonds", "Cash"),
    Percentage = c(data[1], data[2], data[3])
  )
  
  # Add percentage labels
  pie_data$Label <- paste0(pie_data$Type, "\n", round(pie_data$Percentage, 1), "%")
  
  ggplot(pie_data, aes(x = "", y = Percentage, fill = Type)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = Label), position = position_stack(vjust = 0.5), size = 5, fontface = "bold", color = "black") +  # Cleaned up labels
    labs(title = paste(risk_level, "Risk Portfolio")) +
    theme_void() +  # No axes or grid lines for cleaner look
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 10)),
          legend.position = "none") +  # Remove legend
    scale_fill_manual(values = c("red", "blue", "green"))  # Contrasting colors for each asset
}

# Plot pie charts for each risk level
low_risk_pie <- plot_pie(c(low_risk_solution$Stock_Percentage, low_risk_solution$Bond_Percentage, low_risk_solution$Cash_Percentage), "Low")
medium_risk_pie <- plot_pie(c(medium_risk_solution$Stock_Percentage, medium_risk_solution$Bond_Percentage, medium_risk_solution$Cash_Percentage), "Moderate")
high_risk_pie <- plot_pie(c(high_risk_solution$Stock_Percentage, high_risk_solution$Bond_Percentage, high_risk_solution$Cash_Percentage), "High")

# Arrange pie charts in a grid with cleaner spacing
grid.arrange(low_risk_pie, medium_risk_pie, high_risk_pie, ncol = 3, 
             top = "Investment Portfolio Composition by Risk Level")

```

```{r}
#Quick Test

# Step 1: Print quantiles of Total_Risk
risk_quantiles <- quantile(Pareto.front.investmentBlends$Total_Risk, probs = c(1/3, 2/3))
print(risk_quantiles)

# Step 2: Count the number of portfolios in each risk level
risk_level_counts <- table(Pareto.front.investmentBlends$Risk_Level)
print(risk_level_counts)

# Step 3: Verify the range of Total_Risk within each category
low_risk_range <- range(Pareto.front.investmentBlends$Total_Risk[Pareto.front.investmentBlends$Risk_Level == "Low"])
medium_risk_range <- range(Pareto.front.investmentBlends$Total_Risk[Pareto.front.investmentBlends$Risk_Level == "Medium"])
high_risk_range <- range(Pareto.front.investmentBlends$Total_Risk[Pareto.front.investmentBlends$Risk_Level == "High"])

cat("Low risk range: ", low_risk_range, "\n")
cat("Medium risk range: ", medium_risk_range, "\n")
cat("High risk range: ", high_risk_range, "\n")

```

#Part-4 

```{r}
# Assuming Pareto.front.investmentBlends is the data frame from nsga2 optimization

# Sort the Pareto front based on ROI (percentage return) from least to greatest
sorted_pareto <- Pareto.front.investmentBlends[order(Pareto.front.investmentBlends$Total_ROI), ]

# Create a new data frame to hold the sorted ROI and their corresponding asset percentages
plot_data <- data.frame(
  ROI = sorted_pareto$Total_ROI,  # Sorted ROI
  Stock_Percentage = sorted_pareto$Stock_Percentage,
  Bond_Percentage = sorted_pareto$Bond_Percentage,
  Cash_Percentage = sorted_pareto$Cash_Percentage
)

# Calculate the total percentage for each portfolio (should be close to 100%)
plot_data$Total_Percentage = plot_data$Stock_Percentage + plot_data$Bond_Percentage + plot_data$Cash_Percentage

# Plotting the data using ggplot2
library(ggplot2)

ggplot(plot_data, aes(x = ROI)) +
  geom_line(aes(y = Stock_Percentage, color = "Stocks"), size = 1) +
  geom_line(aes(y = Bond_Percentage, color = "Bonds"), size = 1) +
  geom_line(aes(y = Cash_Percentage, color = "Cash"), size = 1) +
  geom_line(aes(y = Total_Percentage, color = "Total%"), linetype = "dashed", size = 1) +
  scale_color_manual(values = c("Stocks" = "green", "Bonds" = "red", "Cash" = "blue", "Total%" = "black")) +
  labs(title = "Portfolio Blend Across Pareto Front",
       x = "% Return",
       y = "% Blend",
       color = "Asset Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

```
#Part-5

```{r}
# ASSN 3 - Investment Section - Question 5
# Multi-objective Genetic Algorithm with Correlation Minimization

# Load required libraries
if (!require("mco")) install.packages("mco")
if (!require("ggcorrplot")) install.packages("ggcorrplot")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")
library(mco)
library(ggcorrplot)
library(ggplot2)
library(dplyr)

# Load data
df <- invest <- read.table("invest.tab")
correlation_tab <- as.matrix(read.table("corr.tab", header = FALSE))
numberOptions <- nrow(invest)

# Convert type to labeled factor
invest$Type <- factor(invest$Type, levels = c(1, 2, 3), labels = c("Stock", "Bond", "Cash"))

# Constants
minAMOUNT = 0.05
maxAMOUNT = 0.2
minNumber = 8
maxNumber = 12
lower = rep(0, numberOptions)
upper = rep(maxAMOUNT, numberOptions)

# Constraints
portfolioSUM <- function(x) {
  selected <- which(x >= minAMOUNT)
  sumx <- sum(x[selected])
  if (sumx >= 0.95 && sumx <= 1.0) return(1)
  return(-1)
}

portfolioRANGE <- function(x) {
  selected <- which(x >= minAMOUNT)
  if (any(x[selected] > maxAMOUNT)) return(-1)
  return(1)
}

portfolioNUM <- function(x) {
  numselected <- length(which(x >= minAMOUNT))
  if (numselected < minNumber || numselected > maxNumber) return(-1)
  return(1)
}

# Objectives
ROI <- function(x) {
  selected <- which(x >= minAMOUNT)
  roi <- sum(invest$ROI[selected] * x[selected])
  return(-roi)
}

RISK <- function(x) {
  selected <- which(x >= minAMOUNT)
  risk <- sum(invest$Risk[selected] * x[selected])
  return(risk)
}

min_correlation <- function(x) {
  selected <- which(x >= minAMOUNT)
  if (length(selected) < 2) return(0)
  correlation_sum <- 0
  count <- 0
  for (i in 1:length(selected)) {
    for (j in 1:length(selected)) {
      if (i != j) {
        correlation_sum <- correlation_sum + correlation_tab[selected[i], selected[j]]
        count <- count + 1
      }
    }
  }
  return(correlation_sum / count)
}

funs <- function(x) {
  return(c(ROI(x), RISK(x), min_correlation(x)))
}

constraintFNS <- function(x) {
  return(c(portfolioRANGE(x), portfolioNUM(x), portfolioSUM(x)))
}

# Run nsga2
set.seed(123)
portfolio <- nsga2(funs,
                   idim = numberOptions,
                   odim = 3,
                   popsize = 52,
                   generations = 500,
                   lower.bounds = lower,
                   upper.bounds = upper,
                   constraints = constraintFNS,
                   cdim = 3)

# Helper to get row indices
Return.row <- function(df, x) {
  sapply(x, function(name) which(row.names(df) == name))
}

# Visualize one solution correlation matrix
i <- 5
solution1 <- invest[which(portfolio$par[i, ] > 0.05), ]
corr.vector <- Return.row(df = invest, row.names(solution1))
sol1.matrix <- matrix(ncol = length(corr.vector), nrow = length(corr.vector))

for (i in 1:length(corr.vector)) {
  for (j in 1:length(corr.vector)) {
    sol1.matrix[i, j] <- correlation_tab[corr.vector[i], corr.vector[j]]
  }
}
colnames(sol1.matrix) <- row.names(solution1)
rownames(sol1.matrix) <- row.names(solution1)

sol1.correlation <- ggcorrplot(sol1.matrix,
                               type = "lower",
                               outline.color = "white",
                               lab = TRUE) +
  labs(title = "Correlation matrix for one of the solutions on Pareto Front")
print(sol1.correlation)

# Create Pareto front dataframe
Pareto.front.investmentBlends <- as.data.frame(matrix(nrow = 52, ncol = 10))
colnames(Pareto.front.investmentBlends) <- c("Stock", "Bond", "Cash", "Stock_Percentage", "Bond_Percentage", "Cash_Percentage", "ROI", "Risk", "Risk_level", "Correlation_Sum")

for (i in 1:52) {
  selected <- which(portfolio$par[i, ] > 0.05)
  x <- invest[selected, ]
  x_types <- as.character(x$Type)
  Pareto.front.investmentBlends[i, 1] <- sum(x_types == "Stock")
  Pareto.front.investmentBlends[i, 2] <- sum(x_types == "Bond")
  Pareto.front.investmentBlends[i, 3] <- sum(x_types == "Cash")
  total_selected <- sum(Pareto.front.investmentBlends[i, 1:3], na.rm = TRUE)
  Pareto.front.investmentBlends[i, 4] <- (Pareto.front.investmentBlends[i, 1] / total_selected) * 100
  Pareto.front.investmentBlends[i, 5] <- (Pareto.front.investmentBlends[i, 2] / total_selected) * 100
  Pareto.front.investmentBlends[i, 6] <- (Pareto.front.investmentBlends[i, 3] / total_selected) * 100
  Pareto.front.investmentBlends[i, 7] <- -portfolio$value[i, 1]
  Pareto.front.investmentBlends[i, 8] <- portfolio$value[i, 2]
  Pareto.front.investmentBlends[i, 10] <- portfolio$value[i, 3]
}

risk_quantiles <- quantile(Pareto.front.investmentBlends$Risk, probs = c(1/3, 2/3), na.rm = TRUE)
Pareto.front.investmentBlends$Risk_level <- cut(Pareto.front.investmentBlends$Risk,
                                                breaks = c(-Inf, risk_quantiles[1], risk_quantiles[2], Inf),
                                                labels = c("low risk", "medium risk", "high risk"),
                                                right = TRUE)

# Plot 1: ROI vs Correlation with Risk coloring
question5.1 <- ggplot(Pareto.front.investmentBlends, aes(x = Correlation_Sum, y = ROI)) +
  geom_point(aes(color = Risk_level)) +
  labs(title = "Objective Space-ROI vs Correlation")

# Plot 2: Correlation vs Investment Blend Composition
question5.3 <- ggplot(Pareto.front.investmentBlends, aes(x = Correlation_Sum)) +
  geom_line(aes(y = Stock_Percentage, color = "% Stock Blend")) +
  geom_line(aes(y = Bond_Percentage, color = "% Bond Blend")) +
  geom_line(aes(y = Cash_Percentage, color = "% Cash Blend")) +
  ylab("Percentage of different types of investment blend") +
  ggtitle("Percentage of stock blend vs Correlation")

# Boxplot by risk levels
low.risk.df <- filter(Pareto.front.investmentBlends, Risk_level == "low risk")
medium.risk.df <- filter(Pareto.front.investmentBlends, Risk_level == "medium risk")
high.risk.df <- filter(Pareto.front.investmentBlends, Risk_level == "high risk")

plot1 <- boxplot(low.risk.df$Correlation_Sum, medium.risk.df$Correlation_Sum, high.risk.df$Correlation_Sum,
                 main = "Distribution of correlation sum across different risk levels",
                 names = c("Low Risk", "Medium Risk", "High Risk"),
                 ylab = "Correlation sum")

# Show plots
print(question5.1)
print(question5.3)
plot1


```
```{r}
library(dplyr)

# Find rows with highest stock percentage
top_stock_allocations <- Pareto.front.investmentBlends %>%
  arrange(desc(Stock_Percentage)) %>%
  head(5)

print(top_stock_allocations)

```

