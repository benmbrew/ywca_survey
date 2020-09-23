library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
source('global.R')

header <- dashboardHeader(title="Databrew app")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Single variable",
      tabName="single",
      icon=icon("eye")),
    menuItem(
      text="Two variable",
      tabName="double",
      icon=icon("eye")),
    menuItem(
      text="Word cloud",
      tabName="word",
      icon=icon("eye")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName="single",
      fluidPage(
        fluidRow(
          column(2, 
                 selectInput('q1', 
                             'Choose a question', 
                             choices = unique(dat$question), 
                             selected = 'In your area of work  what community ies  do you serve'),
                 uiOutput('a1_ui'),
                 
          ),
          column(10,
                 plotOutput('plot_1', height = '600px'))
        )
        
      )
    ),
    tabItem(
      tabName="double",
      fluidPage(
        fluidRow(
          column(4, 
                 selectInput('q1_double', 
                             'Choose a question', 
                             choices = unique(dat$question), 
                             selected = 'In your area of work  what community ies  do you serve'),
                 uiOutput('a1_double_ui'),
                 
                 selectInput('q2_double', 
                             'Choose a question', 
                             choices = unique(dat$question), 
                             selected = 'Have you been personally affected by gun violence'),
                 uiOutput('a2_double_ui')
                 
                 
                 
          ),
          
          
          
          column(8,
                 # DT::dataTableOutput('table_1'),
                 uiOutput('var_boxes'))
        )
        
      )
    ),
    tabItem(
      tabName="word",
      fluidPage(
        fluidRow(
          column(4, 
                 selectInput('q_word', 
                             'Choose a question', 
                             choices = names(string_dat), 
                             selected = 'How has this impacted your work performance'),
                 
          ),
          column(8,
                 plotOutput('word_1', height = '800px', width = '1000px'))
        )
        
      )
    ),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
          h4('Built in partnership with ',
             a(href = 'http://databrew.cc',
               target='_blank', 'Databrew'),
             align = 'center'),
          p('Empowering research and analysis through collaborative data science.', align = 'center'),
          div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                             icon = icon("envelope", lib = "font-awesome")),
                href="mailto:info@databrew.cc",
                align = 'center')), 
          style = 'text-align:center;'
        )
      )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  # render ui for answer 1
  output$a1_ui <- renderUI({
    # num_q <- input$num_q
    q1 <- 'In your area of work  what community ies  do you serve'
    q1 <- input$q1
    sub_dat <- dat[dat$question == q1, ]
    q1_answers <- unique(sub_dat$answer)
    fluidPage(
      fluidRow(
        selectInput('a1','Choose answers', 
                    choices = q1_answers, 
                    selected = q1_answers,
                    multiple = TRUE)
        
      )
    )# end fluid page
  })
  
  # create data that takes inputs and returns plot
  output$plot_1 <- renderPlot({
    # a1 <- as.character(q1_answers)
    # a2 <- as.character(q2_answers)
    # num_q <- input$num_q
    q1 <- input$q1
    a1 <- input$a1
    if(is.null(a1)){
      NULL
    } else {
      sub_dat <- dat %>% filter(question == q1)
      sub_dat <- sub_dat %>% filter(answer %in% a1)
      sub_dat <- sub_dat %>% group_by(answer) %>% summarise(counts=n())
      sub_dat$per <- sub_dat$counts/56
      plot_title <- paste0(q1)
      if(plot_title == "In your area of work  what community ies  do you serve"){
        plot_title = "In your area of work what communities do you serve"
      }
      p <- ggplot(sub_dat, aes(reorder(answer, -per), per)) + 
        geom_bar(stat='identity') +
        scale_y_continuous(labels = scales::percent) +
        labs(x = '',
             y = 'Percent responded',
             title = plot_title) +
        theme_bw() +
        theme(axis.text.x = element_text(angle=45, hjust = 1, size = 10))
      p
      return(p)
      
    }
  })
  
  # create a reactive dataframe that curates data for table - takes two questions and cleans for a1,a2
  two_q <- reactive({
    q1 <- 'In your area of work  what community ies  do you serve'
    q2 <- 'Are you able to provide or refer victims survivors impacted by gun violence to culturally appropriate social and health services'
    
    q1 <- input$q1_double
    q2 <- input$q2_double
    
    sub_dat <- cat_dat %>% select(c(q1,q2))
    dat_list <- list()
    for(i in c(q1,q2)){
      sub_col <- sub_dat %>% select(all_of(i))
      names(sub_col)[1] <- 'V'
      # sub_col <- sub_col %>% filter(V!='')
      temp <- sub_col %>% mutate(ind = row_number()) %>%
        separate_rows(V, sep=",") %>%
        mutate(V = ifelse(is.na(V),0, V)) %>%
        count(ind, V) %>%
        spread(V, n, fill = 0) %>%
        as.data.frame() %>%
        select(-ind)
      names(temp) <- trimws(names(temp), which = 'both')
      dat_list[[i]] <- temp
      print(i)
    }
    return(dat_list)
    
  })
  
  # render ui for answer 1 from q1_double
  output$a1_double_ui <- renderUI({
    # two_q_dat <- dat_list
    two_q_dat <- two_q()
    if(is.null(two_q_dat)){
      NULL
    } else {
      q1_dat <- two_q_dat[[1]]
      q1_answers = names(q1_dat)
      q1_answers <- q1_answers[!grepl(other_strings, q1_answers)]
      q1_answers <- ifelse(q1_answers == '0', 'Not applicable', q1_answers)
      q1_answers <- unique(q1_answers)
      
      fluidPage(
        fluidRow(
          selectInput('a1_double','Choose answers', 
                      choices = q1_answers, 
                      selected = q1_answers[1])
          
        )
      )# end fluid page
    }
    
  })
  
  # render ui for answer 1 from q2_double
  output$a2_double_ui <- renderUI({
    # num_q <- input$num_q
    # two_q_dat <- dat_list
    two_q_dat <- two_q()
    if(is.null(two_q_dat)){
      NULL
    } else {
      q2_dat <- two_q_dat[[2]]
      q2_answers = names(q2_dat)
      q2_answers <- q2_answers[!grepl(other_strings, q2_answers)]
      q2_answers <- ifelse(q2_answers == '0', 'Not applicable', q2_answers)
      q2_answers <- unique(q2_answers)
      fluidPage(
        fluidRow(
          selectInput('a2_double','Choose answers', 
                      choices = q2_answers, 
                      selected = q2_answers[1])
          
        )
      )# end fluid page
    }
    
  })
  
  output$var_boxes <- renderUI({
    # two_q_dat <- dat_list
    # a1 <- q1_answers[1]
    # a2 <- q2_answers[1]
    q2 <- input$q2_double
    q1 <- input$q1_double
    a1 <- input$a1_double
    a2 <- input$a2_double
    two_q_dat <- two_q()
    
    
    if(is.null(two_q_dat) |is.null(a1)|is.null(a2)){
      NULL
    } else {
      # extract data and subset
      q1_dat <- two_q_dat[[1]]
      q2_dat <- two_q_dat[[2]]
      q1_dat <- q1_dat %>% select(all_of(a1)) 
      q2_dat <- q2_dat %>% select(all_of(a2))
      q_dat <- as.data.frame(cbind(q1_dat, q2_dat))
      q_dat <- as.data.frame(cbind(q1=q1_dat, q2=q2_dat))
      q_dat <- as.data.frame(table(q_dat))
      # q_dat <- as.data.frame(t(q_dat))
      
      # get titles
      q1_title <- paste0(q1, ' =', a1)
      q2_title<- paste0(q2, ' =', a2)
      q_all_title <- paste0(a1, ' and ', a2)
      
      # get text
      q1_text <- paste0(round((sum(q1_dat)/56)*100, 2), '%')
      q2_text <- paste0(round((sum(q2_dat)/56)*100, 2), '%')
      q_all_text <- paste0(round((q_dat$Freq[q_dat[,1] == 1 & q_dat[,2]==1]/56)*100, 2), '%')
      
      fluidPage(
        fluidRow(
          valueBox(
            subtitle = q1_title, value = q1_text,
            color = 'blue', width = 6),
          valueBox(
            subtitle = q_all_title, value=q_all_text,
            color = 'red', width = 6)),
        fluidRow(
          valueBox(
            subtitle = q2_title,value =  q2_text,
            color = 'blue', width = 6)
        )
      )
      
      
      
      
    }
    
  }) 
  
  
  
  # render datatable
  output$table_1 <- DT::renderDataTable({
    
    # two_q_dat <- dat_list
    # a1 <- q1_answers[1]
    # a2 <- q2_answers[1]
    q2 <- input$q2_double
    q1 <- input$q1_double
    a1 <- input$a1_double
    a2 <- input$a2_double
    two_q_dat <- two_q()
    
    
    if(is.null(two_q_dat) |is.null(a1)|is.null(a2)){
      NULL
    } else {
      # extract data and subset
      q1_dat <- two_q_dat[[1]]
      q2_dat <- two_q_dat[[2]]
      q1_dat <- q1_dat %>% select(all_of(a1)) 
      q2_dat <- q2_dat %>% select(all_of(a2))
      q_dat <- as.data.frame(cbind(q1_dat, q2_dat))
      q_dat <- as.data.frame(cbind(q1=q1_dat, q2=q2_dat))
      q_dat <- as.data.frame(table(q_dat))
      names(q_dat)[1] <- paste0(q1, ' =', a1)
      names(q_dat)[2] <- paste0(q2, ' =', a2)
      DT::datatable(q_dat)
      
    }
  })
  
  #create word cloud from q_word input
  output$word_1 <- renderPlot({
    q_word <- 'How has this impacted your work performance'
    q_word <- input$q_word
    temp <- string_dat %>% select(all_of(q_word))
    #Create a vector containing only the text
    text <- as.character(temp[,1])
    # text <- gsub("[\r\n]", "", text)
    text <- gsub("[[:punct:]]", "", text)
    remove_text <- '^on$|the|and|^of|^and|^has|^by|^in|^you|for|yes|^yes|^or|^other|^than|where|are|^which|^but|^changed|what|available|about|not|most|get|feel|events|create|pay|given|how|with|helping|since|long|would|event|possibly|^any|^much|little|led|person|tell|lead|garing|sure|continue|allow|part|when|who|furr|having|time|that|from|knowing|since|^use|going|sense|thus|even|especially|outside|this|^ten|besides|own|more|becomes|was|were|had|while|lot|level|all|small|same|name|period|asking|might|its|not|years|waiting|the|how|several|enough|^out|^how|know|have'
    text <- gsub(remove_text, '', text)
    # Create a corpus  
    docs <- Corpus(VectorSource(text))
    
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    
    wordcloud(words = df$word, freq = df$freq, min.freq = 1, random.order=FALSE, rot.per=0.35,  colors=brewer.pal(8, "Dark2"))
    
    
    
  })
  
  
}

shinyApp(ui, server)