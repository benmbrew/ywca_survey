library(tidyverse)


# read in data
dat <- read.csv('Gun Violence Impact Survey  (Responses) - Form responses 1.csv', stringsAsFactors = FALSE)
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

# count how many commas
#apply(cat_dat, 2, function(x) length(which(grepl(',', x, fixed = TRUE))))

# recode column to make all unique answers without commas 
#  Racialized (Black, West Asian, North African, East Asian, South Asian, Latinx, etc.),
# Indigenous (First Nations, Inuit, Metis)
cat_dat$V5 <- gsub('Racialized (Black, West Asian, North African, East Asian, South Asian, Latinx, etc.)', 'Racialized', cat_dat$V5, fixed = TRUE)
cat_dat$V5 <- gsub('Indigenous (First Nations, Inuit, Metis)', 'Indigenous', cat_dat$V5, fixed = TRUE)

# find other variables with same issue and fix


######### ----------------
# clean data

# loop through columns and create new variables accordingly based on if multiple answers
i = 5
column_names <- names(cat_dat)
for(i in 1:ncol(cat_dat)){
  this_column <- column_names[i]
  sub_col <- cat_dat[,this_column]
  is_multiple <- length(which(grepl(',', sub_col, fixed = TRUE))) > 5
  if(is_multiple){
    sub_col = ifelse(grepl('primarily boys', sub_col), 'prim')
  }
}