library(tidyverse)
library(reshape2)
library(DT)
library(wordcloud2)
library(tm)
library(wordcloud)

# read in data
dat <- read.csv('Gun Violence Impact Survey  (Responses) - Form responses 1.csv', stringsAsFactors = FALSE,na.strings=c(na.strings=c(""," ","NA")))
dat$Timestamp <- NULL
dat$id <- 1:nrow(dat)

# get cat cols index 
string_ind <- c(3,10, 11, 13, 14, 16, 
                17, 19, 20, 24, 25, 
                26, 28, 30, 32, 33, 
                34, 36, 37)
cat_ind <- c(1:38)[! c(1:38) %in% string_ind ]

# subset data by cat 
cat_dat <- dat[, cat_ind]
names(cat_dat) <- gsub("\\.", " ", names(cat_dat), perl=TRUE)
names(cat_dat) <- trimws(names(cat_dat), which = 'both')

# get string data
string_dat <- dat[, string_ind]
names(string_dat) <- gsub("\\.", " ", names(string_dat), perl=TRUE)
names(string_dat) <- trimws(names(string_dat), which = 'both')
string_dat <- string_dat[,-1]


# # store list of real question names 
# q_names <- names(dat)
# q_names <- gsub("\\.", " ", q_names, perl=TRUE)


# rename columns 
# names(dat) <- paste0('V',seq(1,38,1))

# # seprate data into strings and categories 
# string_cols <- c('V3','V10', 'V11', 'V13', 'V14', 'V16', 
#                  'V17', 'V19', 'V20', 'V24', 'V25', 
#                  'V26', 'V28', 'V30', 'V32', 'V33', 
#                  'V34', 'V36', 'V37')
# cat_cols <- names(dat)[!names(dat) %in% string_cols]



# subset data by each group
# cat_dat <- dat[ ,cat_cols]
# string_dat <- dat[ ,string_cols]

rm(dat)

# count how many commas
apply(cat_dat, 2, function(x) length(which(grepl(',', x, fixed = TRUE))))

# HERE RECODE CORRECTLY WITH NEW DATA (REPLACE V5 WITH REAL COLUMN NAMES)
# recode column to make all unique answers without commas 
# Racialized (Black, West Asian, North African, East Asian, South Asian, Latinx, etc.),
# Indigenous (First Nations, Inuit, Metis)
cat_dat$`In your area of work  what community ies  do you serve` <- gsub('Racialized (Black, West Asian, North African, East Asian, South Asian, Latinx, etc.)', 
                   'Racialized', 
                   cat_dat$`In your area of work  what community ies  do you serve`,
                   fixed = TRUE)
cat_dat$`In your area of work  what community ies  do you serve` <- gsub('Indigenous (First Nations, Inuit, Metis)', 
                   'Indigenous', 
                   cat_dat$`In your area of work  what community ies  do you serve`, 
                   fixed = TRUE)

# recode V15.REPLACE V15 WITH REAL COLUMN NAME
# Toolkit on how to navigate trauma, deal with media, find community supports, etc.
cat_dat$`In supporting survivors who lost loved ones to gun violence  especially women  what kind of supports do you think would benefit your clients and their families  and why` <- gsub('Toolkit on how to navigate trauma, deal with media, find community supports, etc.',
                    'Toolkit',
                    cat_dat$`In supporting survivors who lost loved ones to gun violence  especially women  what kind of supports do you think would benefit your clients and their families  and why`,
                    fixed = TRUE)

# remove trailing/leading spaces in all columns
cat_dat <- as.data.frame(apply(cat_dat, 2, function(x) trimws(x, which = 'both')), stringsAsFactors = FALSE)


######### ----------------
# clean data
cat_dat$`What is the address and or postal code of your` <- cat_dat$`Additional comments on  What have you noticed are the greatest impacts of gun violence among witnesses  victims survivors of gun violence you serve` <- NULL

# loop through columns and create new variables accordingly based on if multiple answers
i = 6

column_names <- names(cat_dat)
dat_list <- list()
for(i in 1:ncol(cat_dat)){
  this_column <- column_names[i]
  sub_col <- cat_dat[,this_column]
  sub_col <- as.data.frame(sub_col)
  names(sub_col) <- this_column
  sub_col[,this_column] <- as.character(sub_col[,this_column])
  names(sub_col) <- 'V'
  sub_col <- sub_col %>% filter(V!='')
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
  print(i)
  
}

# combine data
dat <- do.call(rbind, dat_list)
dat <- dat %>% filter(value == 1)
dat$answer <- as.character(dat$answer)

# replace string responses with other
other_strings <- "settlement worker|service coordinator|Settlement/Employment Counsellor|community development worker|mental health and addiction justice case management|Women's Centre/Charitable Organization,Non profit Organization|Front Desk at Women's Clinic|Combination of Family and Settlement work|Women's Centre/Charitable Organization|Administrator|Program Coordinator- Employment|survivors of gender based violence|Homeless and those struggling with mental health and addictions|Any Person who identifies as a Woman|and a mix of other communities.|Mothers who lost children to gun violence and ex-offenders who have done the crime and done the time and now want to make a difference|We see and serve those not indicated as well|Racism|EAP|All that are listed above have no meaning unless the social structure is weighed and changed.|ensure basic needs are being met and long term goals being acheived.|goal/passion development- one mom started a catering company, another record production."

dat$answer <- ifelse(grepl(other_strings, dat$answer), 'Other', dat$answer)
dat$answer <- trimws(dat$answer, which = 'both')

# get "other" for each question 
# q1: settlement worker, settlement coordinator, Settlement/Employment Counsellor, community development worker, mental health and addiction justice case management, Women's Centre/Charitable Organization,Non profit Organization,  Front Desk at Women's Clinic
# 
# # q1 `How would you classify your field of work`
# q1_other <- "settlement worker|service coordinator|Settlement/Employment Counsellor|community development worker|mental health and addiction justice case management|Women's Centre/Charitable Organization,Non profit Organization|Front Desk at Women's Clinic|Combination of Family and Settlement work|Women's Centre/Charitable Organization|Administrator|Program Coordinator- Employment"
# 
# # In your area of work, what communities do you serve
# q4_other <- "survivors of gender based violence|Homeless and those struggling with mental health and addictions|Any Person who identifies as a Woman|and a mix of other communities.|Mothers who lost children to gun violence and ex-offenders who have done the crime and done the time and now want to make a difference|We see and serve those not indicated as well"
# 
# # What have you noticed are the greatest impacts of gun violence among witnesses/ victims/ survivors of gun violence you serve? 
# q7_other <- 'Racism'
# 
# # `In supporting survivors who lost loved ones to gun violence  especially women  what kind of supports do you think would benefit your clients and their families  and why`
# q10_other <- 'All that are listed above have no meaning unless the social structure is weighed and changed.|ensure basic needs are being met and long term goals being acheived.|goal/passion development- one mom started a catering company, another record production.'
# 
# # Did you access services that were helpful to you
# q13_other <- 'EAP'

# Have been involved in activism or advocacy around gun violence and in what forms? 
# q18_other

# HERE NEED TO ADD QUESTION NAMES BACK IN AND RECODE EXTRA AS OTHER

# THEN ADD A SECOND TAB CALLED TWO VARIABLES AND PRESENT A TABLE INSTEAD OF A CHART

# 3rd tab could be word cloud associated with each question explanation (string_dat)

rm(dat_list, string_cols, string_ind, temp,
   cat_ind, i, this_column, column_names, sub_col)
# 
# # function for plottng summary of one variable
# 
# plot_summarise <- function(plot_dat, var_name){
#   plot_dat <- plot_dat %>% filter(question == var_name) %>%
#     group_by(answer) %>% summarise(counts = n())
#   plot_dat$total <- sum(plot_dat$counts)
#   plot_dat$per <- round((plot_dat$counts/plot_dat$total)*100, 2)
#   plot_dat$answer <- as.character(plot_dat$answer)
#   return(plot_dat)
