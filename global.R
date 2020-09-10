library(tidyverse)
library(reshape2)

# read in data
dat <- read.csv('Gun Violence Impact Survey  (Responses) - Form responses 1.csv', stringsAsFactors = FALSE)
dat$Timestamp <- NULL
dat$id <- 1:nrow(dat)

# rename columns 
names(dat) <- paste0('V',seq(1,38,1))

# seprate data into strings and categories 
string_cols <- c('V3','V10', 'V11', 'V13', 'V14', 'V16', 
                 'V17', 'V19', 'V20', 'V24', 'V25', 
                 'V26', 'V28', 'V30', 'V32', 'V33', 
                 'V34', 'V36', 'V37')
cat_cols <- names(dat)[!names(dat) %in% string_cols]

# subset data by each group
cat_dat <- dat[ ,cat_cols]
string_dat <- dat[ ,string_cols]

rm(dat)

# count how many commas
apply(cat_dat, 2, function(x) length(which(grepl(',', x, fixed = TRUE))))

# recode column to make all unique answers without commas 
# Racialized (Black, West Asian, North African, East Asian, South Asian, Latinx, etc.),
# Indigenous (First Nations, Inuit, Metis)
cat_dat$V5 <- gsub('Racialized (Black, West Asian, North African, East Asian, South Asian, Latinx, etc.)', 
                   'Racialized', 
                   cat_dat$V5,
                   fixed = TRUE)
cat_dat$V5 <- gsub('Indigenous (First Nations, Inuit, Metis)', 
                   'Indigenous', 
                   cat_dat$V5, 
                   fixed = TRUE)

# recode V15.
# Toolkit on how to navigate trauma, deal with media, find community supports, etc.
cat_dat$V15 <- gsub('Toolkit on how to navigate trauma, deal with media, find community supports, etc.',
                    'Toolkit',
                    cat_dat$V15,
                    fixed = TRUE)

######### ----------------
# clean data
cat_dat$V4 <- cat_dat$V9 <- NULL

# loop through columns and create new variables accordingly based on if multiple answers
i = 8
column_names <- names(cat_dat)
dat_list <- list()
for(i in 1:ncol(cat_dat)){
  this_column <- column_names[i]
  sub_col <- cat_dat[,this_column]
  sub_col <- as.data.frame(sub_col)
  names(sub_col) <- this_column
  sub_col[,this_column] <- as.character(sub_col[,this_column])
  names(sub_col) <- 'V'
  temp <- sub_col %>% mutate(ind = row_number()) %>%
    separate_rows(V, sep=",") %>%
    mutate(V = ifelse(is.na(V),0, V)) %>%
    count(ind, V) %>%
    spread(V, n, fill = 0) %>%
    as.data.frame()
  # names(temp) <- paste0(this_column,'_', names(temp))
  temp <- melt(temp, id.vars = 'ind')
  temp$question <- this_column
  names(temp)[2] <- 'answer'
  temp <- temp[, c('ind', 'question', 'answer', 'value')]
  names(temp)[1] <- 'id'
  dat_list[[i]] <- temp
  
}

# combine data
dat <- do.call(rbind, dat_list)
dat <- dat %>% filter(value == 1)

# function for plottng summary of one variable
plot_summarise <- function(plot_dat){
  
}

# function for exploring relationships between multiple variables 
explore_comparisons <- function(temp,
                                question_list,
                                answer_filter_1, 
                                answer_filter_2, 
                                answer_filter_3){
  
  # group by question list 
  temp <- temp %>% filter(question %in% question_list)
  temp$answer <- as.character(temp$answer)
  temp <-temp %>% filter(answer %in% c(answer_filter_1, answer_filter_2, answer_filter_3))
  temp <- spread(temp, key = question, value = answer)
  names(temp)[3:5] <- paste0('V', 1:3)
  temp <- temp %>% group_by(V1, V2, V3) %>% summarise(counts = n())
  names(temp)[1:3] <- question_list
  
  return(temp)
  
}

explore_comparisons(temp = dat,
                    question_list =c('V1', 'V5', 'V6'),
                    answer_filter_1 = 'Social worker',
                    answer_filter_2 = 'Women',
                    answer_filter_3 = 'Yes' )
