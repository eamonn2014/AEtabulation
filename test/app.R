  # experimental AE app dashboard
  library(shiny)
  library(shinydashboard)
  library(rms)          # automatically engages rms(Hmisc)
  library(tidyverse)
  library(kableExtra) 
  library(knitr) 
  # wordcloud
  library(wordcloud)
  library(wordcloud2)
  library(RColorBrewer)
  library(tm)
  set.seed(124)

  # format function 
  formatz <- function(x){
       sprintf(x, fmt = '%#.1f')  
  }
   

  ui <- dashboardPage( 
                      
  dashboardHeader(title = "Adverse Events"), 
  dashboardSidebar(width=300,
                   sidebarMenu(id = "SideBarMENU", 
                               #~~~~~~~~~~~~~
                               menuItem("1 Define parameters ", icon = icon("bar-chart-o"),
                                        
                                        tags$div(
                                          textInput(inputId="m", label='Number of System Organ Classes', width = '95%' , value="5"),
                                        ),
                                        
                                        tags$div(
                                          textInput(inputId='n', label="Total sample size", width = '95%' , "500"),
                                        ),
                                        
                                        tags$div(
                                          textInput(inputId='rate', label='Adverse event rate', width = '95%' , ".333"),
                                        ),
                                        tags$div(
                                          textInput(inputId="J", label='Maximum number of Preferred Terms', width = '95%' , value="5"),
                                        )

                               ),
                               #~~~~~~~~~~~~~
                               menuItem("2 AE table & Dynamic listing", tabName = "OVERVIEW",  icon = icon("bar-chart-o"), selected = TRUE),
                               #~~~~~~~~~~~~~
                               menuItem("3 Supporting outputs",  startExpanded = FALSE,  icon = icon("bar-chart-o"),
                                       
                                        menuSubItem("i Word cloud",        tabName = "RESULTS3"),
                                        menuSubItem("ii Dynamic listing (repeat)",  tabName = "RESULTS")
                                      #  menuSubItem("testing" ,         tabName = "RESULTS2")
                                      #  menuSubItem("PF WORD CLOUD" ,  tabName = "RESULTS4")
                               ), 
                               #~~~~~~~~~~~~~
                               menuItem("3 Grab the code", icon = icon("bar-chart-o"),
            
                                        menuSubItem("Shiny",
                                                    icon = icon("send",lib='glyphicon'),
                                                    href = "https://raw.githubusercontent.com/eamonn2014/AEtabulation/master/test/app.R"),
            
                                        menuSubItem("Rmarkdown",
                                                    icon = icon("send",lib='glyphicon'),
                                                    href = "https://raw.githubusercontent.com/eamonn2014/AEtabulation/master/AE%20tables.Rmd") #,
             
                               ),
                               
                               # tags$head(
                               #   tags$style(HTML('#resample{background-color:palegreen}'))
                               # ),
                               # 
                               # actionButton("resample"," Hit to sample another data set", icon = icon("th"),  width =250  ),
                               
                               
                               #~~~~~~~~~~~~~
                               menuItem("4 Wiki", tabName = "HELP",  icon = icon("bar-chart-o"))
                   ),
                               #~~~~~~~~~~~~~
                                 tags$head(
                                    tags$style(HTML('#resample{background-color:palegreen}'))
                                  ),
                                 
                                  actionButton("resample"," Hit to sample another data set", icon = icon("th"),  width =250  )

                                  ),
  
          dashboardBody(
            #~~~~~~~~~~~~~
              fluidRow(
                infoBox(
                  "Adverse Event", "Tabulation", icon = icon("line-chart"),
                  width = 4
                ),
                infoBox(
                  "Listing", "Dynamic", icon = icon("user-friends"),
                  width = 4
                ),
                infoBox(
                  "Wordcloud", "Informal yet insightful visualisation", icon = icon("book-open"),
                  width = 4
                )),
              #~~~~~~~~~~~~~
               tabItems(
                  tabItem("OVERVIEW",
                fluidRow(        
                  box(
                    title = "Table"
                    ,status = "primary"
                    ,solidHeader = TRUE 
                    ,collapsible = TRUE 
                    , htmlOutput("tableset") 
                  )
                  
                ,box(
                  title = "Dynamic listing, patients with at least one AE"
                  ,status = "primary"
                  ,solidHeader = TRUE 
                  ,collapsible = TRUE 
                  ,DT::dataTableOutput("mytable2")
                ) 
                )
              ),
            #~~~~~~~~~~~~~
              tabItem("RESULTS3",
                      fluidRow(        
                        box(
                          title = "SYSTEM ORGAN CLASS WORDCLOUD"
                          ,status = "primary"
                          ,solidHeader = TRUE 
                          ,collapsible = TRUE 
                          , plotOutput("SOC", height = "500px") #, width  ="800px")
                        )
                        
                        ,box(
                          title = "PREFERRED TERM WORDCLOUD"
                          ,status = "primary"
                          ,solidHeader = TRUE 
                          ,collapsible = TRUE 
                          ,plotOutput("PF", height = "500px")
                        ) 
                      )
              ),
            #~~~~~~~~~~~~~
     
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          tabItem("RESULTS", 
                  box(" ", 
                      DT::dataTableOutput("mytable")
                   )
          ),
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          tabItem("RESULTS2", 
                  box("Results box", 
                      htmlOutput("AE2")
                   )
          ),
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          tabItem("RESULTS4", 
                  box(" ", 
                   #   plotOutput("PF", height = "500px")
                  )
          ),
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
          tabItem("HELP", 
                  box("", 
                      textOutput("help"),
                      br(),
                      textOutput("help2"))
          ) 
          #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    )
  )
  
)

 

server <- function(input, output, session) {
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # This is where a new sample is instigated and inputs converted to numeric
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  random.sample <- reactive({
    
    foo <- input$resample
    
    m <- as.numeric(input$m)
    
    n <- as.numeric(input$n)
    
    rate <- as.numeric(input$rate)
    
    J <- as.numeric(input$J)
    
    return(list(  
      m=m,
      n=n,
      rate=rate,
      J=J
    ))
    
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # GENERATE THE MEDICAL DICTIONARY
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat <- reactive({
    
    sample <- random.sample()
    
    m=sample$m
    n=sample$n
    rate=sample$rate
    J=sample$J
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # make a second dictionary, maybe this is easier to explain the logic
    N   <- m                           # number of SOC
    S   <- rep(1:N) 
    p   <- round(runif(N,2,J))    
    d1 <-  sort(rep(S, times=p))  
    d1 <-  data.frame(cbind(id=d1))
    
    # count with group
    library(dplyr)
    d1 <- d1 %>%
      group_by(id) %>%
      mutate(count = seq(n()))
    
    SOC <- paste0("SOC",d1$id )
    PT <-  paste0("PT",d1$count,".SOC",d1$id)
    db <-  data.frame(cbind(SOC, PT ))
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    return(list(  db=db))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # GENERATE THE PATIENT DATA
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat2 <- reactive({
    
    sample <- random.sample()
    
    db=dat()$db
    n=sample$n
    rate=sample$rate
    # J=sample$J
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    id <- 1:n             # patients
    ae <- rpois(n, rate)  # AEs per person
    
    zeroAE <- id[ae %in% 0]  # these ids have no AEs
    elements_2_remove <- zeroAE
    
    people.with.AE =   id[!(id %in% elements_2_remove)]
    people.with.no.AE = id[(id %in% elements_2_remove)]
    
    AE <- ae[!ae %in% 0]
    
    ex <- rep(people.with.AE, AE)
    
    d1 <- data.frame(cbind(id=ex))
    d1 <- add_count(d1,id)
    
    d2 <- d1 %>%
      group_by(id, n) %>%
      mutate(count = seq(n()))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#peatients with no AEs
    people.with.no.AE = id[(id %in% elements_2_remove)]
    
    AE <- ae[ae %in% 0]
    
    d1 <- data.frame(cbind(id=people.with.no.AE, n=0,count=0))
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    all <- rbind(d1,d2)
    
    all <- plyr::arrange(all, id, count)
    
    names(all) <- c("id","tot.AE","count.AE")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    AES <- db[sample(nrow(db), nrow(d2), replace=TRUE), ]
    
    d2 <- cbind(d2, AES)
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    d1$SOC="";d1$PT=""
    
    all <- rbind(d1,d2)
    
    all <- plyr::arrange(all, id, count)
    
    names(all) <- c("id","tot.AE","count.AE","SOC","PT")
    
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # add trt
    trt <- sample(c("Treatment A","Treatment B"), n, replace=TRUE)
    
    all$treatment <- rep(trt, times=as.vector(table(all$id)))
    all$treatment <- factor(all$treatment)
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    return(list( all=all))
    
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # START BUILDING A TABLE, OVERALL COUNTS
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat3 <- reactive({
    
    all=dat2()$all
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # Calculate N
    N <- unique(all[, c("id", "treatment")])
    N0 <- addmargins(table(N$treatment))
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
    # Calculate the number of patients with at least one AE
    N <- unique(all[, c("id", "tot.AE","treatment")])
    atleast <- N[!N$tot.AE %in% 0,]
    N1 <- addmargins(table(atleast$treatment))
    
    return(list( N1=N1, N0=N0))
    
  })
  
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # MANAGE THE DATA SOC PT
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  dat4 <- reactive({
     
    db=dat()$db
    all=dat2()$all
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
     # Manage data for by System order class : Preferred terms, the right way
    
    callx <- all[all$count.AE > 0,]  # dispense with all obs with no AEs
    
    s <- callx[, c("id","SOC","PT","treatment")]  # drop count vars
    
    fooz<-list()                        # this will be used to collect data
    
    # analyse data in SOC chunks 
    #k <- sort(unique(s$SOC))       # get a vector of unique SOC
    k <- gtools::mixedsort(unique(s$SOC)) 
    L <- length(k)                 # how many unique SOC 
    
    for (i in 1:L) {               # one by one analyze SOC groups
      
      ss <- s[s$SOC %in% k[i],]    # isolate a SOC chunk
      
      # special case if only 1 SOC row
      if (nrow(ss) %in% 1) {
        
        ss1 <- addmargins(table(ss$PT, ss$treatment))
        Tot <- ss1[rownames(ss1) %in% "Sum",]
        body <- ss1[!rownames(ss1) %in% "Sum",]
        z <- rbind( Tot , body)
        rownames(z)[1] <- k[i]    
        rownames(z)[2] <-  rownames(ss1)[1]   
        
        fooz[[i]] <- z  
        
      } else {
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # This step ensure if same SOC occurs more than once in a patient count only once
        sss <-unique(ss[,c("id","SOC","treatment")]) #  [1] drop PT
        Tot <- addmargins(table(sss$treatment))      #  [2]
        
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        sss <- unique(ss[,c("id","PT","treatment")]) #  [1] drop SOC
        ss1 <- addmargins(table(sss$PT, sss$treatment))
        
        if (nrow(ss1) %in% 2) {      
          
          rownames(ss1) <- ifelse(rownames(ss1) %in% 'Sum', k[i], rownames(ss1))
          z <- ss1[order(rownames(ss1), decreasing=TRUE),]
          
        } else {
          
          body <- ss1[ c(1:(nrow(ss1)-1)),]   # select all rows except last
          body <- body[ gtools::mixedsort(row.names(body)), ]  # sort alphanumeric
          z <- rbind( Tot , body)
          rownames(z)[1] <- k[i]    # add in SOC name
        }
         fooz[[i]] <- z     # collect info
        
      }
      
      tmp <- do.call(rbind, fooz)      # bind the groups
      
      foo99 <- data.frame(tmp)  
    }
    
     return(list( foo99 = foo99, s=s, callx=callx, all=all, tmp=tmp,k=k))  # MOST OF THESE WERE USED TO TROUBLE SHOOT WE ONLY NEED foo99
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  # BRING BITS OF TABLE TOGETHER
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
   dat5 <- reactive({
    
    N1=dat3()$N1
    N0=dat3()$N0
    foo99=dat4()$foo99
  
    foo <- rbind(N0,N1,foo99)
    foo <- data.frame(foo)
    
    rownames(foo)[1] <- "Number of patients"
    rownames(foo)[2] <- "Number of patients with at least one adverse event"
    
    N0 <- data.frame((N0))#
    foo$Ap <- foo$Treatment.A/N0[1,2]*100
    foo$Bp <- foo$Treatment.B/N0[2,2]*100
    foo$Tp <- foo$Sum/N0[3,2]*100
    foo <- foo[,c("Treatment.A", "Ap","Treatment.B","Bp", "Sum","Tp")]
    foo$Ap <- formatz(foo$Ap);
    foo$Bp <- formatz(foo$Bp);
    foo$Tp <- formatz(foo$Tp);
    foo$Ap <- with(foo, paste0("(",  foo$Ap , ")"))
    foo$Bp <- with(foo, paste0("(",  foo$Bp , ")"))
    foo$Tp <- with(foo, paste0("(",  foo$Tp , ")"))
    names(foo) <- c("Treatment A", "(%)","Treatment B","(%)", "Total","(%)")
    return(list( foo = foo, N0=N0))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  # OUTPUT THE TABLE USING KABLE
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
   output$tableset <- renderText({
    
    foo <- dat5()$foo
    
    pt <- grepl( "PT" ,  rownames(foo) ) # indent this term
    
    d <- cbind("Primary System Organ Class / Preferred Term"= rownames(foo), foo)
    
    rownames(d) <- NULL
    
    # footnotes
    t1<- "- Adverse events are defined as new or worsening events that occur on or after the first study day of study treatment"
    t2<-"- A patient with multiple adverse events within a primary system organ class is counted only once for that system organ class"
    t3<-"- A patient with multiple adverse events within a preferred term is counted only once for that preferred term"
    t3a<-"- A patient with multiple occurrences of an AE is counted only once in the AE category"
    t4<-"- System organ classes are presented in alphabetical order"
    t5<-"- Preferred terms are sorted within system organ class in alphabetical order"
    t6<-"- MedDRA version XY.Z has been used for reporting of adverse events"
    t7<-"- Study ABCMADEUPEFG; Database lock: 10Nov2020"
    
    tab_1 <-  kable(d ,
                    format = "html",
                    table.attr = "style='width:80%;'",
                    align = "lcccccc" ,
                    #caption = "Adverse events, by primary system organ class and preferred term (Safety Analysis Set)",
                    escape = F
    ) %>%
      kable_styling(
        full_width = T ,
        bootstrap_options = c("hover", "condensed" , "bordered"),
        font_size = 14,
        position = "center",
      ) %>% row_spec(0, font_size=14
      ) %>% add_header_above(c("Adverse events, by primary system organ class and preferred term (Safety Analysis Set)" = 7) , bold = TRUE, font_size=15
      ) %>% footnote(
        general = c(t1, t3a,t4,t5,t6,t7),
        general_title = " ",
        footnote_as_chunk = F ,
        escape = F
      )
    
    tab_1 <- add_indent(tab_1, which(pt), level_of_indent = 1)
    return(tab_1)
  })
  
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
   # LISTING USING DATATABLE
   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
   output$mytable2 <- output$mytable <- DT::renderDataTable({
    
    require(gtools)
    
    all=dat2()$all
    
    # all2 <- all[!all$tot.AE %in% 0,] #  remove patients with no AE
    all2 <- all[all$count.AE > 0,]  # dispens
    
    all2 <- arrange(all2, SOC, PT,id, treatment)
    
    # https://community.rstudio.com/t/filter-out-all-rows-with-duplicate-values/41043/4
    # flag duplicates
    all2 = all2 %>% 
      group_by(id, SOC) %>% 
      mutate(duplicate.soc.flag = n() > 1)
    
    all2$duplicate.soc.flag<-all2$duplicate.soc.flag*1
    
    all2 = all2 %>% 
      group_by(id, PT) %>% 
      mutate(duplicate.pt.flag = n() > 1)
    
    all2$duplicate.pt.flag<-all2$duplicate.pt.flag*1
    
    all2 <- all2[mixedorder(all2$SOC),]
    
    all2[all2$duplicate.soc.flag %in%  1,]        
    
    DT::datatable(all2, rownames=FALSE,
                  plugins = 'natural',
                  colnames=c('Patient ID' = 'id', 'Total AEs per patient' = 'tot.AE', 'Count of AEs per patient'='count.AE',
                             'Treatment'='treatment','Duplicate SOC?'= 'duplicate.soc.flag',
                             'Duplicate PT?'='duplicate.pt.flag'),
                  options = list(
                    # dom = 't',
                    columnDefs = list(list(type = 'natural', targets = c(4,5)))
                  ))
    })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  # SOC WORD CLOUD, CREATE DATA
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
  CLOUD.SOC<- reactive({
 
    foo99=dat4()$foo99
     
    foo99s <-  foo99[substring(rownames(foo99),1,3) %in% "SOC",]
      
    return(list( foo99s = foo99s))
    
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  # SOC WORD CLOUD, CREATE PLOT
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  
  output$SOC <- renderPlot({
    
    foo99s = CLOUD.SOC()$foo99s
     
    # Create a vector containing only the text
    text <- rep(rownames(foo99s),foo99s$Sum)
    text <- text [! text %in% ""]
    
    # Create a corpus  
    docs <- Corpus(VectorSource(text))
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    
    wordcloud(words = df$word, freq = df$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
  
  
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # PF WORD CLOUD
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  CLOUD.PF<- reactive({
    
    foo99=dat4()$foo99
    
    foo99p <-  foo99[substring(rownames(foo99),1,2) %in% "PT",]
    
    return(list( foo99p = foo99p))

  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  # PF WORD CLOUD, CREATE PLOT
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##

  output$PF <- renderPlot({
    
  foo99p = CLOUD.PF()$foo99p
  
    text <- rep(rownames(foo99p),foo99p$Sum)
    text <- text [! text %in% ""]
    
    # Create a corpus  
    docs <- Corpus(VectorSource(text))
    dtm <- TermDocumentMatrix(docs) 
    matrix <- as.matrix(dtm) 
    words <- sort(rowSums(matrix),decreasing=TRUE) 
    df <- data.frame(word = names(words),freq=words)
    
    wordcloud(words = df$word, freq = df$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35,
              colors=brewer.pal(8, "Dark2"))
     
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  #  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  output$help <- renderText({
    HTML("Adverse event presentation may be confusing if you do not have experience seeing such a presentation before.
         
         
         
         Here each patient is counted only once within each category. So when one patient has two headache AEs for example, 
         then this patient is counted once under headache. This means, that the numbers of the preferred terms may not sum up to the body system numbers. 
          
         So to build the table investigate first each SOC. So within an SOC count how many patients have at least one of the particular SOC. Second, investigate all PTs within each SOC. 
         Report how many patients have at least one.  

         Note also the 'Number of patients with at least one adverse event' may not match the sum of the SOCs, as at least one patient is quite likely to have more than one SOC.

Ways of saying the same thing: 'Patients with more than one occurrence of a preferred term are counted only once'.'A patient with multiple occurrences of an AE is counted only once in the AE category.'

'Patients with any adverse event' is synonymous with 'Patients with at least one AE'")
  })
  
  
  output$help2 <- renderText({
    HTML("To simulate the data in click 'Define parameters' and enter the maximum number of SOCs, 
    the maximum number of PTs and the rate of AEs based on the Poisson distribution 
    (no difference in two treatement groups) and the total number of patients. A new sample can be simulated by hitting the green button. The dynamic listing is useful to help
         understand the table construction. By sorting on the two duplication columns any duplication within patient is evident. Filtering is also possible
         for example a patient ID can be entered and the data for said patient interogated. Word Clouds supplement the table, present most prevelant SOCs and PFs shown by increasing font size.")
  })
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##TESTING AND TROUBLESHOOTING
  # JUST A TEST output
  # output$AE3 <- renderTable({
  #   dat5()$foo
  # }, rownames = TRUE, colnames = TRUE)
  
  # output$AE2 <- renderTable({
  #   CLOUD.SOC()$foo99s
  # }, rownames = TRUE, colnames = TRUE)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
 
}

shinyApp(ui, server)