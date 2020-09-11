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
i = 4
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

# clean columns
dat$answer <-trimws(dat$answer, which = 'both')


# function for plottng summary of one variable
plot_dat <- dat
var_name = 'V5'
plot_summarise <- function(plot_dat, var_name){

  plot_dat <- plot_dat %>% filter(question == var_name) %>%
    group_by(answer) %>% summarise(counts = n())
  plot_dat$total <- sum(plot_dat$counts)
  plot_dat$per <- round((plot_dat$counts/plot_dat$total)*100, 2)
  plot_dat$answer <- as.character(plot_dat$answer)
  return(plot_dat)
}

dat_v5 <- plot_summarise(plot_dat = dat, var_name = 'V5')
dat_v12 <- plot_summarise(plot_dat = dat, var_name = 'V12')
dat_v7 <- plot_summarise(plot_dat = dat, var_name = 'V7')
dat_v31 <- plot_summarise(plot_dat = dat, var_name = 'V31')
dat_v15 <- plot_summarise(plot_dat = dat, var_name = 'V15')


# subset by counts
dat_v5 <- dat_v5 %>% filter(counts > 1)
# recode other
# and a mix of other communities. 
dat_v5$answer <- ifelse(grepl('and a mix of other communit|Mental|homeless|gender|violence|
                              Any Person who identifies|We see and serve|Any Person who|mental health|Children|who identifies', dat_v5$answer), 
                        'Other', dat_v5$answer)

dat_v15$answer <- ifelse(grepl('All that are listed |another record|ensure basic needs|Families and|Financial|goal/passion|Support/treatment', dat_v15$answer), 
                        'Other', dat_v15$answer)
  
# get unique counts 
dat_v5 <- dat_v5 %>% group_by(answer) %>% summarise(counts = sum(counts))
dat_v5$tot <- sum(dat_v5$counts)
dat_v5$per <- round((dat_v5$counts/dat_v5$tot)*100,2)

dat_v15 <- dat_v15 %>% group_by(answer) %>% summarise(counts = sum(counts))
dat_v15$tot <- sum(dat_v15$counts)
dat_v15$per <- round((dat_v15$counts/dat_v15$tot)*100,2)


# make bar plot
ggplot(dat_v5, aes(answer, per)) + geom_bar(stat = 'identity')


get_table <- function(temp_dat, column_names){
  temp_dat <- temp_dat %>% filter(question %in% column_names)
  temp_dat <- spread(temp_dat, key=question, value=answer)
  names(temp_dat)[3:4] <- paste0('V', 1:2)
  temp_dat <- temp_dat %>% group_by(V1, V2) %>% summarise(counts=n())
  names(temp_dat)[1:2] <- column_names
  return(temp_dat)
}

# v2 and v6 have 
dat_v7v12 <- get_table(temp_dat = dat, column_names = c('V7', 'V12'))



# # function for exploring relationships between multiple variables 
# explore_comparisons <- function(temp,
#                                 question_list,
#                                 answer_filter_1, 
#                                 answer_filter_2){
#   
#   # group by question list 
#   temp <- temp %>% filter(question %in% question_list)
#   temp$answer <- as.character(temp$answer)
#   temp <-temp %>% filter(answer %in% c(answer_filter_1, answer_filter_2))
#   temp <- spread(temp, key = question, value = answer)
#   names(temp)[3:4] <- paste0('V', 1:2)
#   temp <- temp %>% group_by(V1, V2) %>% summarise(counts = n())
#   names(temp)[1:2] <- question_list
#   
#   return(temp)
#   
# }



# temp1 <- explore_comparisons(temp = dat,
#                     question_list =c('V2', 'V6'),
#                     answer_filter_1 = '1',
#                     answer_filter_2 = 'Yes')
