#Stored files in the working directory
file_data <- read.csv("data_brexit_referendum.csv")
head(file_data)

#what data type is it
class(file_data)
#structure of the data frame 
str(file_data)

#Dealing with missing data 
#the original data has missing values that are represented as -1
#we are going to serach and replace then with NA

#Count the no of values
sum(file_data$Leave[file_data$Leave ==-1])

#Replace it with NA
file_data$Leave[file_data$Leave == -1] <- NA

#Verify that  is replaced
sum(file_data$Leave[file_data$Leave ==-1])


#View the records with NA in them 
na_records <- file_data[!complete.cases(file_data),]
na_records

#count the number of rows with NA
nrow(na_records)

#Display the column containing missing value
library(VIM)
missing_values <- aggr(file_data, prop = FALSE, numbers = TRUE)

#lets look at the pecentage of leave voters
file_data$percent_leave <- file_data$Leave / file_data$NVotes
file_data$percent_leave

#store the result in vote column
attach(file_data)
file_data$Vote[file_data$percent_leave <= 0.5] <- "Remain"
file_data$Vote[file_data$percent_leave > 0.5] <- "Leave"
detach(file_data)

file_data$RegionName <- as.character(RegionName)
attach(file_data)
file_data$RegionName[RegionName == "London"] <- "L"
file_data$RegionName[file_data$RegionName == "North West"] <- "NW"
file_data$RegionName[file_data$RegionName == "North East"] <- "NE"
file_data$RegionName[file_data$RegionName == "South West"] <- "SW"
file_data$RegionName[file_data$RegionName == "East Midlands"] <- "EM"
file_data$RegionName[file_data$RegionName == "West Midlands"] <- "WM"
file_data$RegionName[file_data$RegionName == "South East"] <- "SE"
file_data$RegionName[file_data$RegionName == "East of England"] <- "EE"
file_data$RegionName[file_data$RegionName == "Yorkshire and the Humber"] <- "Y"

#we can see what this shows relevant summaries of the data 
#and is tailored for each type
summary(file_data)

#Check if each variable is numeric
is.numeric(file_data$percent_leave)
is.numeric(file_data$RegionName)

#Use sapply function to check whether each variable 
#is numeric or not
numeric_variable_list <- sapply(file_data, is.numeric)
numeric_variable_list

#we can sue this logic to create the subset of the data 
numerical_data <- file_data[numeric_variable_list]
colnames(numerical_data)


#remove ID filed as it has no meaning 
numeric_variable_list["ID"] <- FALSE
numerical_data <- file_data[numeric_variable_list]


#lapply() function returns a named list
#where each list has corresponding name
lapply(numerical_data, summary)


cbind(lapply(numerical_data, summary))

numerical_summary <- do.call(cbind(lapply(numerical_data, summary)))

#Need to resolve these issues with "NA" before we can continue
#we could drope NA from this data frame but that will not resolve the issue 
#Instead we will drop them from the original data fra,e
file_data <- file_data[complete.cases(file_data),]
numerical_data <- file_data[numeric_variable_list]
numerical_summary <- do.call(cbind, lapply(numerical_data, summary))
numerical_summary

#we would likeo see the maximum percent_leave value
numerical_summary["Max.", "percent_leave"]
numerical_summary["Min.", "percent_leave"]
numerical_summary["Max.", "percent_leave"] - numerical_summary["Min.", "percent_leave"]


display_variables <- c("NoQuals", "percent_leave", "AdultMeanAge", "L4Quals_plus", "RegionName")
file_data[which.max(file_data$percent_leave), display_variables]

file_data[which.min(file_data$percent_leave), display_variables]

table(file_data$RegionName)
prop.table(table(file_data$RegionName))
barplot(height = prop.table(table(file_data$RegionName)), main = "vote proportion by region", ylab = "Frequency", xlab = "Region", col = "white")


