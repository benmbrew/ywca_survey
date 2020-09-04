library(tidyverse)

# read in data
dat <- read.csv('Gun Violence Impact Survey  (Responses) - Form responses 1.csv')
dat$Timestamp <- NULL

# create question dictionary 
question_names <- names(dat)
question_names <- gsub('.', ' ', question_names, fixed = TRUE)

# rename columns 
names(dat) <- paste0('V',seq(1,37,1))

# create a data dictionary 
# dat_dict <- data_frame(original_names = question_names, new_names = names(dat))

# seprate data into strings and categories 
string_cols <- c('V10', 'V11', 'V13', 'V14', 'V16', 
                 'V17', 'V19', 'V20', 'V24', 'V25', 
                 'V26', 'V28', 'V30', 'V32', 'V33', 
                 'V34', 'V36', 'V37')
cat_cols <- names(dat)[!names(dat) %in% string_cols]

# subset data by each group
cat_dat <- dat[ ,cat_cols]
string_dat <- dat[ ,string_cols]

rm(dat)

######### ----------------
# clean data

# loop through columns and create new variables accordingly based on if multiple answers
i = 10
column_names <- names(dat)
for(i in 1:ncol(dat)){
  this_column <- column_names[i]
  sub_col <- dat[,this_column]
  
}