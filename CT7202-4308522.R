lib = c('readr', 'hash', 'tidyverse', 'data.table', 'zoo', 'dplyr', 'ggmap', 'skimr',
        'devtools', 'visdat', 'DataExplorer', 'inspectdf', 'ggplot2', 'ggcorrplot', 'corrplot', 'reshape2',
        'factoextra', 'e1071', 'forecast', 'mice', 'randomForest', 'reshape2', 'lubridate', 'ggmap', 'GGally',
        'viridis', 'hrbrthemes', 'gganimate', 'caTools', 'caret', 'Rtsne', 'cluster', 'nnet')
#Install All the libraries and dependencies we need using the for loop
for (item in lib){
  install.packages(item)
}


library(tidyverse)
library(cluster)
library(readr)
library(ggmap)
library(GGally)
library(dplyr)
library(visdat)
library(lubridate)
library(data.table)
library(zoo)
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(reshape2)
library(factoextra)
library(e1071)
library(randomForest)
library(forecast)
library(mice)
library(skimr)
library(DataExplorer)
library(inspectdf)
library(devtools)
library(glue)
library(repr)
library(viridis)
library(hrbrthemes)
library(gganimate)
library(caTools)
library(caret)
library(Rtsne)
library(nnet)

setwd("/Users/PAMELA/Documents/myFiles/")

path =  "/Users/PAMELA/Documents/myFiles"


#extracting month and year from file name

# Define a function to extract month and year from a filename
extractDetails = function(filename)
{
  parts = strsplit(filename, "_")[[1]]
  
  # Extract the month from the second-to-last part of the filename
  month = tolower(parts[length(parts) - 1])
  
  # Extract the year from the last part of the filename, removing ".csv" if present
  year = gsub(".csv$", "", parts[length(parts)], ignore.case = TRUE)
  
  # Return a list containing the extracted month and year
  return(list(month = month, year = year))
}

# Process only files with modified names
modifiedFiles <- list.files(path, pattern = "^modified_.*\\.csv$", full.names = TRUE)
# Initialize an empty list to store data frames and file paths
mergedList <- list()

# Loop through each CSV file in the list
for (file in files)
{
  # Extract month and year from the filename using the defined function
  month_year = extractDetails(basename(file))
  
  # Read the CSV file into a data frame
  df = read.csv(file)
  
  # Add columns "Month" and "Year" to the data frame with values from the filename
  df = df %>% mutate(Month = month_year$month, Year = month_year$year)
  
  # Store the data frame and file path in the mergedList
  mergedList[[length(mergedList) + 1]] <- list(df = df, file = file)
  
  # Construct the output file path with a modified filename based on month and year
  output_file = file.path(path, paste0("modified_", month_year$month, "_", month_year$year, ".csv"))
  
  # Write the modified data frame to a new CSV file
  write.csv(df, output_file)
}

# Read all modified files into a list of data frames
#merged <- lapply(modifiedFiles, read.csv)

# Combine the data frames into a single data frame
#merged_data <- do.call(rbind, merged)
library(dplyr)

# Combine the data frames from mergedList using dplyr::bind_rows
merged_data <- bind_rows(lapply(mergedList, function(x) x$df))

# View the merged data frame
View(merged_data)


#merged_data <- bind_rows(mergedList)

# View the merged data frame
#View(merged_data)





write.csv(merged_data, "/Users/PAMELA/Documents/myFiles/merged_data.csv", row.names = FALSE)
View(merged_data)

#DATA EXPLORATION

df <- merged_data
#rate
rate <- merged_data
View(df)
View(rate)
str(df)
dim(df)
summary(df)



#DATA CLEANING

#delete unnecessary columns
df <- df%>% select(-X)
#deleting the first two columns
df <- df[, -c(1,2)]
rate <- rate[, -c(1,2,3)]
#df <- df[, -c(6)]
#df <- df[, -c(29,30)]

#RENAME THE NEW COLUMN
names(df)
names(df)[1] = "county"
names(rate)[1] = "county"
#names(df)[28] = "year"

View(df)


#remove fullstop from column names
colnames(df) <- gsub("\\.", " ", colnames(df))
View(df)

colnames(rate) <- gsub("\\.", " ", colnames(rate))
View(rate)

#Remove percentage column
drop_percentage_columns <- function(df){
  df <- df %>% select(-starts_with("per"))
  return(df)
}
df <- drop_percentage_columns(df)
View(df)

#Remove extra characters
#Create a function for dropping extera characters
drop_extra_chars <- function(df, except_cols) {
  #Rename columns in the dataframe based on a condition
  colnames(df) <- ifelse(colnames(df) %in% except_cols,
                         colnames(df), #Keep the original column names if they are in 'except_cols'
                         substring(colnames(df), 11)) #Extract substring from the 11th character for other columns
  return(df)  #Return the modified dataframe
}

    # Deploy it and update the dataset
    df <- drop_extra_chars(df, c('Year', 'Month', 'county'))
    head(df)
    
#CHECK FOR MISSING OR NULL VALUES
#check for missing values count
missing_value_counts <- colSums(is.na(df))
print(missing_value_counts)


unique(df$ Month)

unique_months <- unique(df$Month)
print(unique_months)


change_months <- function(df){
  df$Month <- gsub("january","jan",df$Month)
  df$Month <- gsub("february","feb",df$Month)
  df$Month <- gsub("march","mar",df$Month)
  df$Month <- gsub("april","apr",df$Month)
  df$Month <- gsub("may","may",df$Month)
  df$Month <- gsub("june","jun",df$Month)
  df$Month <- gsub("july","jul",df$Month)
  df$Month <- gsub("august","aug",df$Month)
  df$Month <- gsub("september","sep",df$Month)
  df$Month <- gsub("october","oct",df$Month)
  df$Month <- gsub("november","nov",df$Month)
  df$Month <- gsub("december","dec",df$Month)
  df$Month <- gsub("dececember","dec",df$Month)
  
  # Return the modified data frame
  return(df)
}
df <- change_months(df)
rate <- change_months(rate)
print(unique_months)




library(dplyr)


#CREATING NEW REGION COLUMN

county_region <- list(
  "National" = "All",
  "Avon and Somerset" = "South West",
  "Bedfordshire" = "East of England",
  "Cambridgeshire" = "East of England",
  "Cheshire" = "North West",
  "Cleveland" = "North East",
  "Cumbria" = "North West",
  "Derbyshire" = "East Midlands",
  "Devon and Cornwall" = "South West",
  "Dorset" = "South West",
  "Durham" = "North East",
  "Dyfed Powys" = "Wales",
  "Essex" = "East of England",
  "Gloucestershire" = "South West",
  "GreaterManchester" = "North West",
  "Gwent" = "Wales",
  "Hampshire" = "South East",
  "Hertfordshire" = "East of England",
  "Humberside" = "Yorkshire and the Humber",
  "Kent" = "South East",
  "Lancashire" = "North West",
  "Leicestershire" = "East Midlands",
  "Lincolnshire" = "East Midlands",
  "Merseyside" = "North West",
  "Metropolitan and City" = "Greater London",
  "Norfolk" = "East of England",
  "Northamptonshire" = "East Midlands",
  "Northumbria" = "North East",
  "North Wales" = "Wales",
  "North Yorkshire" = "Yorkshire and the Humber",
  "Nottinghamshire" = "East Midlands",
  "South Wales" = "Wales",
  "South Yorkshire" = "Yorkshire and the Humber",
  "Staffordshire" = "West Midlands",
  "Suffolk" = "East of England",
  "Surrey" = "South East",
  "Sussex" = "South East",
  "Thames Valley" = "South East",
  "Warwickshire" = "West Midlands",
  "West Mercia" = "West Midlands",
  "West Midlands" = "West Midlands",
  "West Yorkshire" = "Yorkshire and the Humber",
  "Wiltshire" = "South West"
)
#Convert county names to lowercase
#df$county <- tolower(df$county)
#county_region_names <- tolower(names(county_region))

# Add a new column "region" based on county_region
df <- df %>% 
  mutate(region = county_region[match(county, names(county_region))])

# Add a new column "region" based on county_region
rate <- rate %>% 
  mutate(region = county_region[match(county, names(county_region))])

# Display the modified data frame
print(df)
View(df)


#CREATE NEW COLUMN DEMO(DEMOGRAGH)
county_demo <- list(
  "National" = "All",
  "Avon and Somerset" = "Urban",
  "Bedfordshire" = "Urban",
  "Cambridgeshire" = "Urban",
  "Cheshire" = "Urban",
  "Cleveland" = "Urban",
  "Cumbria" = "Urban",
  "Derbyshire" = "Urban",
  "Devon and Cornwall" = "Urban",
  "Dorset" = "Urban",
  "Durham" = "Urban",
  "Dyfed Powys" = "Rural",
  "Essex" = "Urban",
  "Gloucestershire" = "Urban",
  "GreaterManchester" = "Urban",
  "Gwent" = "Rural",
  "Hampshire" = "Urban",
  "Hertfordshire" = "Urban",
  "Humberside" = "Urban",
  "Kent" = "Urban",
  "Lancashire" = "Urban",
  "Leicestershire" = "Urban",
  "Lincolnshire" = "Urban",
  "Merseyside" = "Urban",
  "Metropolitan and City" = "Urban",
  "Norfolk" = "Urban",
  "Northamptonshire" = "Urban",
  "Northumbria" = "Urban",
  "North Wales" = "Rural",
  "North Yorkshire" = "Urban",
  "Nottinghamshire" = "Urban",
  "South Wales" = "Rural",
  "South Yorkshire" = "Urban",
  "Staffordshire" = "Urban",
  "Suffolk" = "Urban",
  "Surrey" = "Urban",
  "Sussex" = "Urban",
  "Thames Valley" = "Urban",
  "Warwickshire" = "Urban",
  "West Mercia" = "Urban",
  "West Midlands" = "Urban",
  "West Yorkshire" = "Urban",
  "Wiltshire" = "Urban"
)



# Add a new column "region" based on county_region
df <- df %>% 
  mutate(demo = county_demo[match(county, names(county_demo))])

# Add a new column "region" based on county_region
rate <- rate %>% 
  mutate(demo = county_demo[match(county, names(county_demo))])

# Display the modified data frame
print(df)
View(df)
add_date_column <- function(df){
  # Convert month abbreviations to lowercase
  df$Month <- tolower(df$Month)
  
  # Create the date column using the correct format
  add_date <- as.Date(paste(df$Month, df$Year, "01", sep = "-"), format = "%b-%Y-%d")
  
  # Add the date column to the data frame
  df$date <- add_date
  
  return(df)
}
#subset(df, Month == "sept" & is.na(date))
unique(df$Month)


df <- add_date_column(df)
df <- add_date_column(df)
rate <- add_date_column(rate)
View(df)

#Shifting new columns created to the 2,3,4,5,6 indexes
df <- df[, c(1, (ncol(df) - 4):(ncol(df)), 2:(ncol(df) - 5))]


view(df1)
view(rate1)

#splitting my data set
df1 <- df

#rate
percentage <- grep("Percentage", names(rate), value = TRUE)

#AD COUNTY MONTH AND YEAR TO PERCENTAGE
columns_to_add <- c("county","demo", "region", "Month", "Year","date", percentage )

rate <- rate[, columns_to_add]



crime <- grep("Convictions", names(df), value = TRUE)

#ADd COUNTY MONTH AND YEAR TO PERCENTAGE
columns_to_add <- c("county","demo", "region", "Month", "Year","date", crime )

crime_df <- df[, columns_to_add]
view(crime_df)


crime_un <- grep("Unsuccessful", names(df), value = TRUE)

#ADd COUNTY MONTH AND YEAR TO PERCENTAGE
columns_to_add <- c("county","demo", "region", "Month", "Year","date", crime_un )

crime_un_df <- df[, columns_to_add]
view(crime_un_df)

crime_df1 <- crime_df
crime_un_df1 <- crime_un_df
rate1 <- rate
str(crime_df1)

#converting REGION and DEMO to character
# List of dataframes
#dataframes <- list(crime_df1, crime_un_df1, rate1)

# Loop through each dataframe
#for (df in dataframes) {
  # Modify the 'region' column
 # df$region <- sapply(df$region, function(x) x[[1]])
 # df$region <- as.character(df$region)
  
  # Modify the 'demo' column
  # Replace 'demo' with the actual column name if it's different
  #df$demo <- sapply(df$demo, function(x) x[[1]])
  #df$demo <- as.character(df$demo)
#}


#converting REGION and DEMO to character
# List of dataframes

crime_df$demo <- sapply(crime_df$demo, function(x) x[[1]])

# Convert the 'demo' column to character
crime_df$demo <- as.character(crime_df$demo)

crime_df$region <- sapply(crime_df$region, function(x) x[[1]])

# Convert the 'region' column to character
crime_df$region <- as.character(crime_df$region)





rate$demo <- sapply(rate$demo, function(x) x[[1]])

# Convert the 'demo' column to character
rate$demo <- as.character(rate$demo)

rate$region <- sapply(rate$region, function(x) x[[1]])

# Convert the 'region' column to character
rate$region <- as.character(rate$region)



crime_un_df$demo <- sapply(crime_un_df$demo, function(x) x[[1]])

# Convert the 'demo' column to character
crime_un_df$demo <- as.character(crime_un_df$demo)

crime_un_df$region <- sapply(crime_un_df$region, function(x) x[[1]])

# Convert the 'region' column to character
crime_un_df$region <- as.character(crime_un_df$region)




df$demo <- sapply(df$demo, function(x) x[[1]])

# Convert the 'demo' column to character
df$demo <- as.character(df$demo)

df$region <- sapply(df$region, function(x) x[[1]])

# Convert the 'region' column to character
df$region <- as.character(df$region)




str(crime_df)
str(crime_un_df)
str(rate)


write.csv(df, "/Users/PAMELA/Documents/myFiles/df.csv", row.names = FALSE)
View(df)


write.csv(crime_df, "/Users/PAMELA/Documents/myFiles/crime_df.csv", row.names = FALSE)
View(crime_df)

write.csv(rate, "/Users/PAMELA/Documents/myFiles/rate.csv", row.names = FALSE)
View(rate)

write.csv(crime_un_df, "/Users/PAMELA/Documents/myFiles/crime_un_df.csv", row.names = FALSE)
View(crime_un_df)


csv_file <- "/Users/PAMELA/Documents/myFiles/rate.csv"

# Read the CSV file into a data frame
rate <- read.csv(csv_file)


csv_file <- "/Users/PAMELA/Documents/myFiles/crime_df.csv"

# Read the CSV file into a data frame
crime_df1 <- read.csv(csv_file)

csv_file <- "/Users/PAMELA/Documents/myFiles/crime_un_df.csv"

# Read the CSV file into a data frame
crime_un_df <- read.csv(csv_file)

# View the data frame
View(rate)
View(crime_un_df)
#removing the percentage sign
# Assuming 'rate' is your data frame
deletePercent <- function(row){
  if(is.character(row)){
    row <- gsub("%", "", row)
    return(row)
  }
  return(row)
}

# Apply the function to each row of the data frame
rate <- apply(rate, 2, deletePercent)
# Assuming 'rate' is your data frame
cols_to_convert <- 7:31

# Convert the specified columns to numeric
rate[, cols_to_convert] <- apply(rate[, cols_to_convert, drop = FALSE], 2, as.numeric)

#replace "-" with 0.0
rate[rate == 'NA'] = 0.0
view(df)
View(rate)

#visualization


#visualizing my dataframe
install.packages("devtools")
library(devtools)

devtools::install_github("ropensci/visdat")

library(visdat)


vis_miss(df)
vis_dat(df)


vis_miss(crime_df)
vis_dat(crime_df)

#creating a report of my dataframe
install.packages("DataExplorer")
library(DataExplorer)

DataExplorer::create_report(crime_df)

install.packages("inspectdf")
library(inspectdf)
view(df)
view(crime_df)
view(crime_un_df)


update.packages(ask = FALSE, checkBuilt = TRUE)

# Create a new library folder
new_lib_path <- "C:/Users/PAMELA/Documents/R/win-library/4.3"

# Set R to use the new library path
.libPaths(new_lib_path)



env0 ="C:/Users/PAMELA/Documents/myFiles/merged_data.csv"
env1 = "C:/Users/PAMELA/Documents/myFiles/crime_df.csv"

env2 = "C:/Users/PAMELA/Documents/myFiles/crime_un_df.csv"

env3 = "C:/Users/PAMELA/Documents/myFiles/rate.csv"

merged_data <- read.csv(env0)
crime_df <- read.csv(env1)
View(crime_df)

grp_by_year <- function(crime_df){
  crime_df <- crime_df[,-c(1,2,3,4,6)]
  crime_df <-crime_df[crime_df$Year != "ALL", ]
  crime_df <- group_by(crime_df,Year)
  summarise_all(crime_df, list(sum))
}
#call the grp_by_year function with the result_df data frame
result_df_year <- grp_by_year(crime_df)
#Melt the data frame to long format for easy plotting
result_df_long <- tidyr:: gather(result_df_year, key= "crime_type", value= "count", -Year)

#plot the bar chart
ggplot(result_df_long, aes(x = Year, y = count, fill = crime_type)) +
   geom_bar(stat = "identity", position = "dodge")+
   labs(title = "Total Convicted Crimes per Year",
        x = "Year",
        y = "Count")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Set3")


grp_by_year <- function(df){
  df <- df[,-c(1,2,3,4,6)]
  df <-df[df$Year != "ALL", ]
  df <- group_by(df,Year)
  summarise_all(df, list(sum))
}
#call the grp_by_year function with the result_df data frame
result_df_year <- grp_by_year(crime_df)
result_df_year2 <- grp_by_year(crime_un_df)
#result_df_year <- grp_by_year(crime_df)
#Melt the data frame to long format for easy plotting
print(result_df_long,n=Inf)

result_df_long <- tidyr:: gather(result_df_year, key= "crime_type", value= "count", -Year)

#plot the bar chart
ggplot(result_df_long, aes(x = Year, y = count, fill = crime_type)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Total Convicted Crimes per Year",
       x = "Year",
       y = "Count")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Set3")

str(crime_df)




grp_by_month <- function(df){
  df <- df[,-c(1,2,3,5,6)]
  df <-df[df$Month != "All", ]
  df <- group_by(df,Month)
  summarise_all(df, list(sum))
}
#call the grp_by_year function with the result_df data frame
result_df_month <- grp_by_month(crime_df)
result_df_year2 <- grp_by_year(crime_un_df)
#result_df_year <- grp_by_year(crime_df)
#Melt the data frame to long format for easy plotting
print(result_df_long,n=Inf)

result_df_long <- tidyr:: gather(result_df_month, key= "crime_type", value= "count", -Month)

#plot the bar chart
ggplot(result_df_long, aes(x = Month, y = count, fill = crime_type)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Total Convicted Crimes per Month",
       x = "Month",
       y = "Count")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Set3")


grp_by_region <- function(df){
  df <- df[,-c(1,2,4,5,6)]
  df <-df[df$region != "All", ]
  df <- group_by(df,region)
  summarise_all(df, list(sum))
}
#call the grp_by_year function with the result_df data frame
result_df_region <- grp_by_region(crime_df)
result_df_year2 <- grp_by_year(crime_un_df)
#result_df_year <- grp_by_year(crime_df)
#Melt the data frame to long format for easy plotting
print(result_df_long,n=Inf)

result_df_long <- tidyr:: gather(result_df_region, key= "crime_type", value= "count", -region)

#plot the bar chart
ggplot(result_df_long, aes(x = region, y = count, fill = crime_type)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Total Convicted Crimes per Region",
       x = "Region",
       y = "Count")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Set3")


grp_by_demo <- function(df){
  df <- df[,-c(1,3,4,5,6)]
  df <-df[df$demo != "All", ]
  df <- group_by(df,demo)
  summarise_all(df, list(sum))
}
#call the grp_by_year function with the result_df data frame
result_df_demo <- grp_by_demo(crime_df)
result_df_year2 <- grp_by_year(crime_un_df)
#result_df_year <- grp_by_year(crime_df)
#Melt the data frame to long format for easy plotting
print(result_df_long,n=Inf)

result_df_long <- tidyr:: gather(result_df_demo, key= "crime_type", value= "count", -demo)

#plot the bar chart
ggplot(result_df_long, aes(x = demo, y = count, fill = crime_type)) +
  geom_bar(stat = "identity", position = "dodge")+
  labs(title = "Total Convicted Crimes per Demography",
       x = "Demography",
       y = "Count")+
  theme_minimal()+
  theme(axis.text.x= element_text(angle = 45, hjust = 1))+
  scale_fill_brewer(palette = "Set3")



#SORTING THE MONTHS
month_sorted <- function(df){
  months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  # Convert the "Month" column to a factor with the custom ordering
  df$Month <- factor(df$Month, levels = months)
  # Sort the dataframe based on the custom ordering of the "Month" column
  df <- df[order(df$Month), ]
  return (df)
}

    result_df_month <- month_sorted(result_df_month)
    View(result_df_month)
    
    
#normalizing the month
#This function gets the result_df_month, and divide all the month by the number of month
#This will get an average crime number for each month in each segment
# one February, one March, two April, two May, and two June are missed.
divide_rows_by_freq <- function(dataframe) {
      
      # this is the number of month for each month. 
      num_months_in_3year <- c(3, 2, 2, 1, 1, 1, 3, 3, 3, 3, 3, 3)
      
      dataframe[, -1] <- sweep(dataframe[, -1], 1, num_months_in_3year, "/")
      
      return (dataframe)
    }

        norm_df_month <- divide_rows_by_freq(result_df_month)
        View(norm_df_month)
        
        library(reshape2)       
        
        # Deploy it
norm_df_month <- melt(norm_df_month , id.vars = 'Month', variable.name = 'successful_crimes')
options(repr.plot.width = 20, repr.plot.height =10)
        ggplot(norm_df_month,aes(x = Month, y = value)) +
          geom_bar(aes(fill = successful_crimes),stat = "identity",position = "dodge", width = 0.8) +
          theme(text = element_text(size = 18), element_line(linewidth =1))
        
 
        
   
        
        install.packages("conflicted")
        library(conflicted)
        conflicted::conflict_prefer("filter", "dplyr")
        conflicted::conflict_prefer("lag", "dplyr")
        
        
        
 grp_by_month <- function(df){
  df <- df[,-c(1,2,3,5,6)]
  df <-df[df$Month != "All", ]
  df <- group_by(df,Month)
  summarise_all(df, list(sum))
}
#call the grp_by_year function with the result_df data frame
result_df_month1 <- grp_by_month(crime_df1)

#create a function to add new column 
sum_crime <- function(df){
  #get the column names from column 7 to the last
  columns_to_sum <- names(df[7:ncol(df)])
  
  #sum the values in the selected columns
  df$ALL <- rowSums(df[columns_to_sum])
  
  #return the modified dataframe
  return(df)
}
        crime_df1 <-sum_crime(crime_df)
        View(crime_df1)
        
#SORTING THE MONTHS
month_sorted <- function(df){
  months <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
  # Convert the "Month" column to a factor with the custom ordering
  df$Month <- factor(df$Month, levels = months)
  # Sort the dataframe based on the custom ordering of the "Month" column
  df <- df[order(df$Month), ]
  return (df)
}

    result_df_month1 <- month_sorted(result_df_month1)
    View(result_df_month1)
    
    
#normalizing the month
#This function gets the result_df_month, and divide all the month by the number of month
#This will get an average crime number for each month in each segment
# one February, one March, two April, two May, and two June are missed.
divide_rows_by_freq <- function(dataframe) {
      
      # this is the number of month for each month. 
      num_months_in_3year <- c(3, 2, 2, 1, 1, 1, 3, 3, 3, 3, 3, 3)
      
      dataframe[, -1] <- sweep(dataframe[, -1], 1, num_months_in_3year, "/")
      
      return (dataframe)
    }

        norm_df_month1 <- divide_rows_by_freq(result_df_month1)
        View(norm_df_month1)
                
        
doughnut_plotter <- function(s) {
# Compute percentages
  s$fraction <- s$ALL / sum(s$ALL)
# Compute the cumulative percentages (top of each rectangle)
  s$ymax <- cumsum(s$fraction)
# Compute the bottom of each rectangle
  s$ymin <- c(0, head(s$ymax, n=-1))
# Compute label position
  s$labelPosition <- (s$ymax + s$ymin) / 2
# Creating the percentage
  s$percentage <- round(s$ALL/sum(s$ALL), digits = 4)
# Compute a good label
  s$label <- paste0(s$Month, "\n %", 100*s$percentage)
# Define the custom palette with 12 dark colors
  custom_palette <- c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd",
                              "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf",
                              "#1a1a1a", "#757575")
          
    options(repr.plot.width = 20, repr.plot.height = 10)
          
  # Make the plot
ggplot(s, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = Month)) +
            geom_rect() +
            geom_text(x = 2.3, aes(y = labelPosition, label = label, color = Month), size = 6) +
            scale_fill_manual(values = custom_palette) + # Use the custom fill palette
            scale_color_manual(values = custom_palette) + # Use the custom color palette
            coord_polar(theta = "y") +
            xlim(c(-1, 4)) +
            theme_void() +
            theme(legend.position = "none")
        }
        
    
 #Plot doughnut plot
doughnut_plotter(norm_df_month1)

#plot the doughnut chart
#plot the doughnut chart
# Plot the doughnut chart
ggplot(result_df_long, aes(x = "", y = count, fill = crime_type)) +
  geom_bar(stat = "identity", position = "stack", width = 1) +
  coord_polar(theta = "y") +
  labs(
    title = "Total convicted crimes per month",
    x = NULL,
    y = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_brewer(palette = "Set3")  # Separate scale specification




#STACKED AREA PLOT
library(viridis)
# Assuming your date column is in a format that can be converted to Date
crime_df1$date <- as.Date(crime_df1$date)


options(repr.plot.width = 25.2, repr.plot.height = 15)
ggplot(crime_df1, aes(x = date, y = ALL, fill = region)) +
  geom_area(alpha = 0.6, linewidth = 0.5, colour = "white") +
  scale_x_date(date_labels = "%b %d, %Y", date_breaks = "1 month") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Stacked Area Chart for Successful Crimes with Respect to the Date and Region") +
  theme(plot.title = element_text(size = 30, hjust = 0.5, vjust = 1.5),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 18),
        axis.text = element_text(angle = 90, hjust = 1, size = 15),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
  )



#Define a correlation visualiser function
library(GGally)
correlation_maker <- function(data){
  options(repr.plot.width = 25.2, repr.plot.height = 15)
  plot <- ggpairs(data[,-c(1,2,3,4,5,6)] , title="correlogram with ggpairs()")
  return (plot)
}

    # Correlation of crime categories for convictions
    correlation_maker(crime_df)
    
    

# Extracting the relevant columns for correlation computation
correlation_data <- crime_df[, -c(1, 2, 3, 4, 5, 6)]

# Computing the correlation matrix
cor_matrix <- cor(correlation_data)

# Viewing the correlation matrix
print(cor_matrix)



#computing correlation matrix for successful crimes
successful_corr <- cor(crime_df[, 7:18])
print("Correlation Between Successful Crimes: ")
print(successful_corr)


#computing correlation matrix for successful crimes
unsuccessful_corr <- cor(crime_un_df[, 7:19])
print("Correlation Between Unsuccessful Crimes: ")
print(unsuccessful_corr)



#computing covariance matrix for unsuccessful crimes
successful_cov <- cov(crime_df[, 7:18])
print("Correlation Between Successful Crimes: ")
print(successful_cov)

#computing covariance matrix for unsuccessful crimes
unsuccessful_cov <- cov(crime_un_df[, 7:19])
print("Correlation Between Unsuccessful Crimes: ")
print(unsuccessful_cov)



# converting correlation and covariance matrices to long format
successful_corr_long <- melt(successful_corr)
unsuccessful_corr_long <- melt(unsuccessful_corr)
successful_cov_long <- melt(successful_cov)
unsuccessful_cov_long <- melt(unsuccessful_cov)

#plotting correlation heatmap for successful crimes
ggplot(successful_corr_long, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Correlation Heatmap - Successful Crimes")

#plotting correlation heatmap for unsuccessful crimes
ggplot(unsuccessful_corr_long, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Correlation Heatmap - Unsuccessful Crimes")

#plotting covariance heatmap for successful crimes
ggplot(successful_cov_long, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Covariance Heatmap - Successful Crimes")

#plotting covariance heatmap for unsuccessful crimes
ggplot(unsuccessful_cov_long, aes(Var2, Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(title = "Covariance Heatmap - Unsuccessful Crimes")


View(crime_df)





combined_df <- function(crime_df, crime_un_df) {
  crime_df <- crime_df[crime_df$county == "National", ]
  crime_un_df <- crime_un_df[crime_un_df$county == "National", ]
  
  numeric_columns_crime_df <- dplyr::select(crime_df, -c("county", "Year", "Month" ,"date", "region","demo"))
  crime_df$no_of_crime_df <- rowSums(numeric_columns_crime_df)
  
  numeric_columns_crime_un_df <- dplyr::select(crime_un_df, -c("county", "Year", "Month" ,"date", "region","demo"))
  crime_un_df$no_of_crime_un_df <- rowSums(numeric_columns_crime_un_df)
  
  crime_df <- dplyr::select(crime_df, c("date", "no_of_crime_df"))
  crime_un_df <- dplyr::select(crime_un_df, c("date", "no_of_crime_un_df"))
  
  merged_df <- merge(crime_df, crime_un_df, by.x = "date", by.y = "date")
  
  return(merged_df)
}
grpd_df <- combined_df(crime_df, crime_un_df)
ggplot(grpd_df, aes(x=no_of_crime_un_df, y=no_of_crime_df, group=1)) + geom_line()



lr_split_data <- function(dataframe){
  dataframe <- dataframe[dataframe$county == "National", ]
  dataframe$date <- as.Date(dataframe$date)
  
  numeric_columns_df <- dplyr::select(dataframe, -c("county", "Year", "Month" ,"date", "region","demo"))
  dataframe$crimes <- rowSums(numeric_columns_df)
  
  test <- dataframe[dataframe$Year == 2018,]
  train <- dataframe[dataframe$Year != 2018,]
  
  test <- dplyr::select(test, -c("county", "Year", "Month" , "region","demo"))
  train <- dplyr::select(train, -c("county", "Year", "Month" ,"region","demo"))
  
  return(list(test, train))
}

linear_regression <- function(test, train){
  # training the model   
  model <- lm(crimes ~ date, data = train)
  
  # using the model for predictions on test data
  preds <- predict(model, test, type = "response")
  actuals_preds <- data.frame(cbind(actuals=test$crimes, predicted=preds))
  
  # calcultaing correaltion   
  correlation_accuracy <- cor(actuals_preds)
  
  return(list(model, actuals_preds, correlation_accuracy))
}


crime_test_train = lr_split_data(crime_df)
crime_un_test_train = lr_split_data(crime_un_df)

# Train Dataset
summary(dplyr::select(crime_test_train[[2]], c("date", "crimes")))

# Test Dataset
summary(dplyr::select(crime_test_train[[1]], c("date", "crimes")))


# Test Dataset
summary(dplyr::select(crime_test_train[[1]], c("date", "crimes")))


# Running linear regression
lr_res = linear_regression(crime_test_train[[1]], crime_test_train[[2]])

lr_model = lr_res[[1]]
lr_actual_preds = lr_res[[2]]
lr_accuracy = lr_res[[3]]


scatter.smooth(x=lr_actual_preds$predicted, y=lr_actual_preds$actuals, main="Linear Regression Predicted vs. Actual Values")



# Check column names of train data
column_names <- names(crime_test_train[[2]])
print(column_names)

lr_model
lr_accuracy
lr_actual_preds



#HYPOTHESIS ONE

# Calculate total number of crimes for each row
total_crimes <- rowSums(crime_df[, 7:ncol(crime_df)])

# Add the TotalCrimes column to the dataframe
crime_df$TotalCrimes <- total_crimes

names(crime_df1)
#PERFECTLY showing the total numbr of crimes took place so far 

# Select the crime variables from your dataset
crime_variables <- c("Homicide.Convictions", "Offences.Against.The.Person.Convictions", "Sexual.Offences.Convictions",
                     "Burglary.Convictions", "Robbery.Convictions", "Theft.And.Handling.Convictions",
                     "Fraud.And.Forgery.Convictions", "Criminal.Damage.Convictions", "Drugs.Offences.Convictions",
                     "Public.Order.Offences.Convictions",  "All.Other.Offences..excluding.Motoring..Convictions",
                     "Motoring.Offences.Convictions" )

# Calculate total number of crimes for each offense
offense_totals <- colSums(crime_df[, crime_variables])

# Create a data frame with offense names and their total frequencies
offense_data <- data.frame(Offense = names(offense_totals), TotalCrimes = offense_totals)

# Rank offenses based on total frequencies
ordered_crimes <- offense_data[order(offense_data$TotalCrimes, decreasing = TRUE), ]

# Display ranked offenses and their total frequencies
print(ordered_crimes)

#cities wise crime 
# Display ranked offenses and their total frequencies
print(ordered_crimes[, c("Offense", "TotalCrimes")])

#print(ordered_crimes[, c("county", "TotalCrimes")], n = Inf)





#HYPOTHESIS 1 - If the crime rate is increasing or decreasing in the rural areas

# Subsetting the relevant variables
#data_subset <- crime_df[, c("Year", "TotalCrimes", "demo")]
# Subsetting the relevant variables and excluding rows with "All" in the "demo" column
data_subset <- subset(crime_df, demo != "All", select = c("Year", "TotalCrimes", "demo"))


# Grouping the data by year and region
data_grouped <- aggregate(TotalCrimes ~ Year + demo, data_subset, mean)

# Creating a new variable for rural regions
data_grouped$Rural <- ifelse(data_grouped$demo == "Rural", 1, 0)

# Performing regression analysis
model <- lm(TotalCrimes ~ Year + Rural, data = data_grouped)
summary(model)

crime_df$TotalCrimes

# Creating a line plot
ggplot(data_grouped, aes(x = Year, y = TotalCrimes, color = demo, group = demo)) +
  geom_line() +
  labs(x = "Year", y = "Average Crime Rate", color = "demo") +
  theme_minimal()





#hypothesis two

split_data <- function(dataframe){
  dataframe <- dataframe[dataframe$county != "National", ]
  dataframe$date <- as.Date(dataframe$date)
  
  numeric_columns_df <- dplyr::select(dataframe, -c("county", "Year", "Month" ,"date", "region","demo"))
  dataframe$crimes <- rowSums(numeric_columns_df)
  
  test <- dataframe[dataframe$Year == 2018,]
  train <- dataframe[dataframe$Year != 2018,]
  
  
  return(list(test, train))
}


multiple_regression <- function(test, train){
  # training the model   
  model <- lm(crimes ~ county + Year + Month, data = train)
  
  # using the model for predictions on test data
  preds <- predict(model, test, type = "response")
  actuals_preds <- data.frame(cbind(actuals=test$crimes, predicted=preds))
  
  # calcultaing correaltion   
  correlation_accuracy <- cor(actuals_preds)
  
  return(list(model, actuals_preds, correlation_accuracy))
}


crime_test_train = split_data(crime_df)
uscrime_test_train = split_data(uscrime_df)


summary(dplyr::select(crime_test_train[[2]], c("county", "Year", "Month", "crimes")))

# Test Dataset
summary(dplyr::select(crime_test_train[[1]], c("county", "Year", "Month", "crimes")))

mr_res = multiple_regression(crime_test_train[[1]], crime_test_train[[2]])

mr_model = mr_res[[1]]
mr_actual_preds = mr_res[[2]]
mr_accuracy = mr_res[[3]]

summary(mr_model)

scatter.smooth(x=mr_actual_preds$predicted, y=mr_actual_preds$actuals, main="Multi Regression Predicted vs. Actual Values")




Clustering
# -- kmeans, and etc
# The goal of the clustering is to group different number of crimes in to clusters.
# Un-supervised 

remove_non_numeric_cols <- function(dataframe) {
  dataframe <- dplyr::select(dataframe, -c("county", "Year", "Month" ,"date", "region","demo"))
  return(dataframe)
}

#KMeans
kmeans_clustering <- function(dataframe, clusters){
  dataframe <- scale(dataframe)
  model <- kmeans(dataframe, centers = clusters, nstart = 25)
  return(model)
}

km_data = remove_non_numeric_cols(crime_df)
summary(km_data)
 
# Clusters == 3
km_model = kmeans_clustering(km_data, 3)
summary(km_model)
library(factoextra)


fviz_cluster(km_model, data = km_data)


View(crime_df1)
# Identify rows where any column has the value 'all'
rows_to_remove <- apply(crime_df1, 1, function(row) any(row == 'All'))

# Remove rows with 'all' values
crime_df1 <- crime_df1[!rows_to_remove, ]

# Now you can use the cleaned crime_df in your k-means clustering functions


# Create a function for evaluating the WCSS
kmean_withinss <- function(k) {
 cluster <- kmeans(crime_df1[,-c(1,2,3,4,5,6)], k)
      return (cluster$tot.withinss)
    }
   
 # Create a function to perform k-means clustering
perform_kmeans_clustering <- function(data, k) {
numeric_data <- data[,-c(1,2,3,4,5,6)]
kmeans_result <- kmeans(numeric_data, centers = k) 
  return(kmeans_result)
}

library(Rtsne)
View(crime_df1)
      
# Create a function to visualize clusters using PCA
 plot_pca_clusters <- function(data, k) {
  pca_result <- prcomp(data, scale. = TRUE)
  reduced_data <- as.data.frame(pca_result$x[, 1:2])
   kmeans_result <- kmeans(reduced_data, centers = k)
   cluster_labels <- kmeans_result$cluster
  data_with_clusters <- cbind(reduced_data, Cluster = as.factor(cluster_labels))
   ggplot(data_with_clusters, aes(x = PC1, y = PC2, color = Cluster)) +
   geom_point(size=2) +
    labs(title = "K-means Clustering (PCA)", x = "Principal Component 1", y = "Principal Component 2") +
    theme_minimal()+
                theme(
                  axis.text = element_text(size = 21),
                  axis.title = element_text(size = 21),
                  plot.title = element_text(size = 30),
                  axis.text.x = element_text(size = 18),
                  axis.text.y = element_text(size = 18),
                  legend.text = element_text(size = 17)
                )
            }
          
                # Create a function to visualize clusters using t-SNE
   plot_tsne_clusters <- function(data, k) {
   data <- data[!duplicated(data), ]
     kmeans_result <- kmeans(data, centers = k)
     cluster_labels <- kmeans_result$cluster
    data_with_clusters <- cbind(data, Cluster = as.factor(cluster_labels))
    tsne_result <- Rtsne(data, dims = 2, perplexity = 30, verbose = TRUE)
    tsne_data <- as.data.frame(tsne_result$Y)
      tsne_data$Cluster <- as.factor(cluster_labels)
        ggplot(tsne_data, aes(x = V1, y = V2, color = Cluster)) +
           geom_point(size=1.8) +
        labs(title = "K-means Clustering (t-SNE)", x = "Dimension 1", y = "Dimension 2") +
                    theme_minimal()+
                    theme(
                      axis.text = element_text(size = 21),
                      axis.title = element_text(size = 21),
                      plot.title = element_text(size = 30),
                      axis.text.x = element_text(size = 18),
                        axis.text.y = element_text(size = 18),
                      legend.text = element_text(size = 17)
                    )
                }
                    # Set maximum cluster 
max_k <-10
wss <- sapply(2:max_k, kmean_withinss)
elbow <-data.frame(2:max_k, wss)
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
 geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))




model_k_3 <- perform_kmeans_clustering(crime_df1, 3)

    print("The Number of Instances belong to Each Cluster:")
    print(table(model_k_3$cluster))
    cat("\n\n\n\n")
    print(model_k_3[-c(1)])
   
 
          plot_pca_clusters(crime_df1[,-c(1,2,3,4,6)], k = 4)
          plot_tsne_clusters(crime_df1[,-c(1,2,3,4,6)], k = 5)
          
#hierarchical clustering
group_by_county <- function(dataframe){
            dataframe <- dplyr::select(dataframe, -c("Year", "Month" ,"date", "region", "demo"))
            dataframe <- dataframe[dataframe$county != "National",]
            dataframe <- group_by(dataframe, county)
            dataframe <- summarise_all(dataframe, funs(sum))
            dataframe2 <- dataframe[,-1]
            rownames(dataframe2) <- dataframe$county
            dataframe2 <- scale(dataframe2)
            return(dataframe2)
          }
 hc_clustering <- function(dataframe) {
            distance_mat <- dist(dataframe, method = 'euclidean')
            set.seed(240)
            model <- hclust(distance_mat, method = "average")
            return(list(distance_mat, model))
          }
          
hc_data = group_by_county(crime_df)
          
summary(hc_data)
    
hc_res = hc_clustering(hc_data)
hc_matrix = hc_res[[1]]
hc_model = hc_res[[2]]
plot(hc_model)
          
          # Clusters == 2
 plot(hc_model)
 rect.hclust(hc_model, k = 2, border = "red")
          
          # Clusters == 3
plot(hc_model)
rect.hclust(hc_model, k = 3, border = "red")
          
          # Clusters == 5
plot(hc_model)
rect.hclust(hc_model, k = 5, border = "red")
          
          # Clusters == 8
plot(hc_model)
rect.hclust(hc_model, k = 8, border = "red")
          
          # Clusters == 11
plot(hc_model)
rect.hclust(hc_model, k = 11, border = "red")

hc_res
hc_matrix
hc_model



#CLASSIFICATION


classify_split_data <- function(dataframe){
  dataframe <- dataframe[dataframe$county != "National", ]
  dataframe$date <- as.Date(dataframe$date)
  dataframe$region <-  as.factor(dataframe$region)
  dataframe$county <-  as.factor(dataframe$county)
  dataframe$demo <-  as.factor(dataframe$demo)
  
  numeric_columns_df <- dplyr::select(dataframe, -c("county", "Year", "Month" ,"date", "region", "demo"))
  dataframe$no_of_crimes <- rowSums(numeric_columns_df)
  
  test <- dataframe[dataframe$Year == 2018,]
  train <- dataframe[dataframe$Year != 2018,]
  
  test <- dplyr::select(test, c("county", "date" ,"no_of_crimes" ,"region","demo"))
  train <- dplyr::select(train, c("county", "date", "no_of_crimes" ,"region","demo"))  
  
  
  return(list(test, train))
}

svm_test_train <- classify_split_data(crime_df)
# Train
summary(svm_test_train[[2]])
                                                   
# Test
summary(svm_test_train[[1]])
                                                   
svm_model_generate <- function(train_df, test_df) {
  set.seed(123)
  
  svm_model <- svm(region ~ county + date + no_of_crimes, data = train_df, type = 'C-classification', kernal = "radial", gamma = 0.1, cost = 1)
  
  test_predictions <- predict(svm_model, test_df)
  confusion_matrix <- confusionMatrix(as.factor(test_predictions), as.factor(test_df$region))
  model_roc = multiclass.roc(test_df$region ~ as.numeric(as.factor(test_predictions)), plot=TRUE, print.auc = TRUE)
  
  return(list(svm_model, confusion_matrix))
}

library(e1071)
library(caret)
library(pROC)



svm_res = svm_model_generate(svm_test_train[[2]], svm_test_train[[1]])
svm_model = svm_res[[1]]
svm_creport = svm_res[[2]]
svm_model
svm_creport




#Random Forest
rf_test_train <- classify_split_data(crime_df)
# Train
summary(rf_test_train[[2]])
                                           
rf_model_generate <- function(train_df, test_df) {
  set.seed(123)
  
  rf_model <- randomForest(region ~ county + date + no_of_crimes, data = train_df, proximity=TRUE)
  
  test_predictions <- predict(rf_model, test_df)
  confusion_matrix <- confusionMatrix(as.factor(test_predictions), as.factor(test_df$region))
  model_roc = multiclass.roc(test_df$region ~ as.numeric(as.factor(test_predictions)), plot=TRUE, print.auc = TRUE)
  
  return(list(rf_model, confusion_matrix))
}

library(randomForest)

rf_res <- rf_model_generate(rf_test_train[[2]], rf_test_train[[1]])
rf_model = rf_res[[1]]
rf_creport = rf_res[[2]]




plot(rf_model)

summary(rf_model)






rf_creport

