## Created by Kunhong Yu (444447)
# Loading packages first
library(dplyr)
library(readxl)
library(glue)
library(stringr)
library(Hmisc)

################## I. Set path ####################
path <- '/Users/kunhongyu/Desktop/DSBA/Year2/Semester4/RR/RRcourse2023/6. Coding and documentation'
setwd(path)
task_data_path <- paste(path, '/Data/onet_tasks.csv', sep = '')
isco_data_path <- paste(path, '/Data//Eurostat_employment_isco.xlsx', sep = '')

################## II. Reading data ####################
## MODIFICATION: Include reading data in a function
read_data <- function(data_path, sheet_name = NULL){
  # Reading data
  # Args : 
  #   --data_path: absolute path of data
  #   --sheet_name: if it is excel file, specify sheet_name rather than NULL
  # return : 
  #   --read_data
  names <- strsplit(data_path, split = '\\.')[[1]]
  suffix <- names[length(names)]
  stopifnot(suffix %in% c('csv', 'xlsx'))
  if (suffix == 'xlsx'){
    stopifnot(!is.null(sheet_name))
  }
  if (suffix == 'csv'){ # csv
    data <- read.csv(data_path, stringsAsFactors = F)
  } else{ # xlsx
    data <- read_xlsx(data_path, sheet = sheet_name)
  }
  
  return (data)
}
## MODIFICATION: Read task data using defined function
# Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.
task_data <- read_data(task_data_path, sheet_name = NULL)
head(task_data)
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)
## MODIFICATION: Read Eurostat data sets, using defined function and a for loop
iscos <- list()
for (i in 1 : 9) {
  file <- read_data(isco_data_path, sheet_name = paste("ISCO", i, sep = ''))
  iscos[[i]] <- file
}
print(glue('Length of iscos: {length(iscos)}.'))
                    
################## III. Data Preprocessing ####################
## MODIFICAITON: Add a function to merge tables
merge_tables <- function(data_list, selected_countries){
  # Merging input tables
  # Args : 
  #   --data_list: data list, containing all read data tables
  #   --selected_countries, list of strings, containing country names
  # return : 
  #   --merged_data
  # for loop to compute 'total' statistics
  # This will calculate worker totals in each of the chosen countries.
  totals <- list()
  for (country in selected_countries){
    total <- 0.
    for (i in 1 : length(data_list)){
      total <- total + data_list[[i]][[country]]
      # We need a column that stores the occupation categories
      data_list[[i]][['ISCO']] <- i
    }
    totals[[glue('total_{country}')]] <- total
  }
  
  # now merging happens and this gives us one large file with employment in all occupations
  # in columns.
  merged_data <- data_list[[1]]
  for (i in 2 : length(data_list)){
    merged_data <- rbind(merged_data, data_list[[i]])
  }
  
  # We have 9 occupations and the same time range for each, so we can add the totals by
  # adding a vector that is 9 times the previously calculated totals
  columns <- names(totals)
  for (i in 1 : length(totals)){
    value <- totals[[columns[i]]]
    merged_data[[columns[i]]] <- rep(value, length(data_list))
    # And this will give us shares of each occupation among all workers in a period-country
    country <- strsplit(columns[i], split = '_')[[1]]
    country <- country[length(country)]
    merged_data[[glue('share_{country}')]] <- merged_data[[country]] / merged_data[[columns[i]]]
  }
  return (merged_data)
}
selected_countries <- c('Belgium', 'Spain', 'Poland')
merged_data <- merge_tables(data_list = iscos, 
                            selected_countries = selected_countries)
head(merged_data)
View(merged_data)

# Now let's look at the task data. We want the first digit of the ISCO variable only
task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()
# And we'll calculate the mean task values at a 1-digit level 
# (more on what these tasks are below)
aggdata <- aggregate(task_data, by = list(task_data$isco08_1dig),
                     FUN = mean, na.rm = TRUE)
aggdata$isco08 <- NULL
# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor.

#These are the ones we're interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others
#Let's combine the data.
combined <- left_join(merged_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Traditionally, the first step is to standardise the task values using weights 
# defined by share of occupations in the labour force. This should be done separately
# for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
# Let's do this for each of the variables that interests us:
## MODIFICATION: Add normalize_data function to perform z-score preprocessing
normalize_data <- function(data, column_name, weight_column_names){
  # Nomalize data set using z-score
  # Args : 
  #   --data: input combined data
  #   --column_name
  #   --weight_column_names: a list contains all weight columns
  # return : 
  #   --one-column-normalized data
  for (weight_column_name in weight_column_names){ # loop through weights
    country <- strsplit(weight_column_name, '_')[[1]]
    country <- country[length(country)]
    meanVal <- wtd.mean(data[[column_name]], 
                        data[[weight_column_name]])
    sdVal <- wtd.var(data[[column_name]], data[[weight_column_name]], method = 'ML') %>% sqrt()
    if (grepl(country, column_name, fixed = TRUE)){ # check string existance
      new_column_name <- glue('std_{column_name}')
    } else{
      new_column_name <- glue('std_{country}_{column_name}')
    }
    data[[new_column_name]] <- (data[[column_name]] - meanVal) / sdVal # add new normalized column with its weight
  }
  return (data)
}
## MODIFICATION: For loop through all possible columns 
columns <- c('t_4A2a4', 't_4A2b2', 't_4A4a1')
weight_columns <- c('share_Belgium', 'share_Poland', 'share_Spain')
combined2 <- combined
for (column_name in columns){ # loop through columns
  print(glue('Normalizing {column_name}...'))
  combined2 <- normalize_data(combined2, column_name = column_name,
                              weight_column_names = weight_columns) # normalize
}
head(combined2)
View(combined2)

# The next step is to calculate the `classic` task content intensity, i.e.
# how important is a particular general task content category in the workforce
# Here, we're looking at non-routine cognitive analytical tasks, as defined
# by David Autor and Darron Acemoglu:
## MODIFICATION: Using a for loop here
for (country in selected_countries){ # loop through countries
  combined2[[glue('{country}_NRCA')]] <- 0.
  for (column_name in columns){ # loop every column we chose before
    combined2[[glue('{country}_NRCA')]] <- combined2[[glue('{country}_NRCA')]] + 
      combined2[[glue('std_{country}_{column_name}')]] 
  }
}
head(combined2)
View(combined2)

## MODIFICATION: Use defined normalize_data function again and for loop
columns <- c('Belgium_NRCA', 'Poland_NRCA', 'Spain_NRCA')
weight_columns <- c('share_Belgium', 'share_Poland', 'share_Spain')
combined3 <- combined2
for (i in 1 : length(columns)){ # loop through columns
  column_name <- columns[i]
  weight_column = weight_columns[i]
  print(glue('Normalizing {column_name}...'))
  combined3 <- normalize_data(combined3, column_name = column_name,
                              weight_column_names = c(weight_column)) # normalize
}
head(combined3)
View(combined3)

# Finally, to track the changes over time, we have to calculate a country-level mean
# Step 1: multiply the value by the share of such workers.
## MODIFICATION: Use for loop
for (country in selected_countries){
  combined3[[glue('multip_{country}_NRCA')]] <- combined3[[glue('std_{country}_NRCA')]] * combined3[[glue('share_{country}')]]
}
# Step 2: sum it up (it basically becomes another weighted mean)
## MODIFICATION: Use for loop
aggs <- list()
for (country in selected_countries) {
  temp <- aggregate(combined3[[glue('multip_{country}_NRCA')]], by = list(combined3$TIME),
                    FUN = sum, na.rm = TRUE)
  aggs[[country]] <- temp
}
print(length(aggs))

################## IV. Plotting ####################
## MODIFICATION: Use for loop
for (i in 1 : length(aggs)){
  agg <- aggs[[i]]
  country <- selected_countries[i]
  # We can plot it now!
  plot(agg$x, xaxt = "n")
  axis(1, at = seq(1, 40, 3), labels = agg$Group.1[seq(1, 40, 3)])
}
