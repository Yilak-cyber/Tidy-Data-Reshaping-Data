#############
# Tidy Data #
#############

#...........................................................................#
# In tidy data, each row represents an observation and each column represents a 
# different variable.

# In wide data, each row includes several observations and 
# one of the variables is stored in the header.
#...........................................................................#
# Open a deataset from dslabs package
library(tidyverse)
library(dslabs)
data(gapminder)

#Explore data
str(gapminder)
head(gapminder)
view(gapminder)

# Create a subset tidy data frame and inspect &   
# 
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)%>%
  arrange(country)

head(tidy_data)
str(tidy_data)
view(tidy_data)

# 
# Plot the tidy data: plotting tidy data is simple

tidy_data %>% 
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# import and inspect example of original Gapminder data in wide format
filename <-"fertility-two-countries-example.csv"
path <- system.file("extdata", package="dslabs")
fullpath <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(fullpath)
view(wide_data)

# Subset wide_data
fertility<-select(wide_data, country, `1960`:`1967`)
view(fertility)

##################
# Reshaping Data #
##################

#...........................................................................#
# The tidyr package includes several functions that are useful for tidying data.
# The gather() function converts wide data into tidy data.
# The spread() function converts tidy data to wide data.
#...........................................................................#

# let us create a new tidy from the wide data for practice
# gather wide data to make new tidy data

# <<<<<<<< Reshaping Data Wide to Long >>>>>>>>>

# Using gather() function
new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)
view(new_tidy_data)
# Alternatively we can define the gather function with"-"
# gather all columns except country(another way of using gather)
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)%>%
  arrange(country)
#...........................................................................#
      # The gather function
# The first argument of gather function sets the name of the column that 
# will hold the variable that are currently kept in the wide data column names.

# The second argument sets the column name for the column
# that will hold the values in the column cells.
#...........................................................................#

xtabs(~country+year, data=gapminder )
xtab= as.data.frame(xtabs(~country+year, data=gapminder ))
view(xtab)
# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

#...........................................................................#
          # Using pivot_longer() instead of  gather()
#pivot_longer() is an updated approach to gather(), designed to be both simpler
# to use and to handle more use cases. We recommend you use pivot_longer() for 
# new code; gather() isn't going away but is no longer under active development.
# Reference https://tidyr.tidyverse.org/reference/pivot_longer.html
# Using pivot_longer() function
new_tidy_data_2 <- wide_data %>% 
  pivot_longer(`1960`:`2015`, names_to = "year", values_to = "fertility")
head(new_tidy_data)
head(new_tidy_data_2)
#...........................................................................#
# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

# <<<<<<<< Reshaping Data Wide to Long >>>>>>>>>
# Converting tidy data to the wide format using spread()
new_wide_data<-new_tidy_data %>% spread(year,fertility)
select(new_wide_data, country, '1960': '1967')
view(new_wide_data)
# Using pivot_wider() instead of  gather()
new_wide_data_2 <- new_tidy_data %>% 
  pivot_wider(names_from = year, values_from = fertility)
select(new_wide_data, country, `1960`:`1967`)
#...........................................................................#               
                # The spread function
# The first argument tells spread which variables will be used as the column names.
# The second argument specifies which variables to use to fill out the cells.
#...........................................................................#

######################
# Separate and Unite #
######################
# import data
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

# gather all columns except country
dat <- raw_dat %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores
dat %>% separate(key, c("year", "variable_name"), "_")
dat %>% separate(key, c("year", "variable_name"))

#...........................................................................# 
                  # Separate and Unite 
# The separate() function splits one column into two or more columns at a 
# specified character that separates the variables.

# When there is an extra separation in some of the entries, use fill="right" 
# to pad missing values with NAs, or use extra="merge" to keep extra 
# elements together.

# The unite() function combines two columns and adds a separating character.

# The separate function takes three arguments-
       #-the name of the column to be separated, 
       #-the names to be used for the new columns, and 
       #-the character that separates the variables.
#...........................................................................# 

# split on all underscores, pad empty cells with NA
dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge")

# separate then spread
dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value) 

# separate then unite
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_")

# full code for tidying data
dat %>% 
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)


###########################################
# Assessment Part 2: Reshaping Data
###########################################
# >>>>>>>>Question 9<<<<<<<<<<<<<<<<

#Examine the built-in dataset co2. This dataset comes with base R, not dslabs - 
# just type co2 to access the dataset.
co2
co2_raw<-co2
view(co2)
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
head(co2_wide)
co2_tidy<-gather(co2_wide,month,co2,-year)
view(co2_tidy)
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

library(tidyverse)
library(dslabs)

data(admissions)
dat <- admissions %>% select(-applicants)
view(dat)
dat_tidy <- spread(dat, gender, admitted)
view(dat_tidy)

# >>>>>>>>Question 13<<<<<<<<<<<<<<<<

# Now use the admissions dataset to create the object tmp, which has columns
# major, gender, key and value:
tmp <- gather(admissions, key, value, admitted:applicants)
tmp
# Combine the key and gender and create a new column called column_name to get 
#a variable with the following values: admitted_men, admitted_women, 
#applicants_men and applicants_women. Save the new data as tmp2.
# Which command could help you to wrangle the data into the desired format?
tmp
  #   major gender        key value
# 1      A    men   admitted    62
# 2      B    men   admitted    63
# 8      B  women   admitted    68
# 9      C  women   admitted    34
# Syntax for tmp2 Column_name with values: men_admitted, women_admitted, 
# men_applicants and women_applicants(Not the answer)
tmp2 <- unite(tmp, column_name, c(gender, key))
tmp2
# major      column_name value
# 1      A     men_admitted    62
# 2      B     men_admitted    63
# 8      B   women_admitted    68
# 9      C   women_admitted    34

# Column_name with values: admitted_men, admitted_women, 
# applicants_men and applicants_women(Answer)
tmp3 <- unite(tmp, column_name, c(key, gender))
tmp3
# major      column_name value
# 1      A     admitted_men    62
# 2      B     admitted_men    63
# 8      B   admitted_women    68
# 9      C   admitted_women    34
# >>>>>>>>Question 14<<<<<<<<<<<<<<<<
# Which function can reshape tmp2 to a table with six rows and five columns 
# named major, admitted_men, admitted_women, applicants_men and applicants_women?
tmp3%>%spread(column_name,value)
# Answer: spread()