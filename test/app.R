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
    if (!is.na(x)  ) {
        sprintf(x, fmt = '%#.1f')  
    }
}


ui <- dashboardPage(
    dashboardHeader(), 
    dashboardSidebar(width=300,
        sidebarMenu(id = "SideBarMENU", 
                    
                    tags$head(
                        tags$style(HTML('#resample{background-color:palegreen}'))
                    ),
                    actionButton("resample"," Hit to sample another data set", icon = icon("th"),  width =250  ),
                    
                   # sidebarMenu(
                        #
                    #    id = "tabs",
                        menuItem("Define parameters ", icon = icon("bar-chart-o"),
                                 
                                 
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
                    
                    
                   
                   menuItem("Code & link to explanation", icon = icon("bar-chart-o"),
                            menuSubItem("Shiny",  
                                        icon = icon("send",lib='glyphicon'), 
                                        href = "https://raw.githubusercontent.com/eamonn2014/Functional-sensitivity/master/dashboard1/app.R"),
                            
                            
                            menuSubItem("R",  
                                        icon = icon("send",lib='glyphicon'), 
                                        href = "https://raw.githubusercontent.com/eamonn2014/Functional-sensitivity/master/Rcode.R") ,
                            
                            
                            
                            menuSubItem("Click for bells and whistles main app.",  
                                        icon = icon("send",lib='glyphicon'), 
                                        href = "https://eamonn3.shinyapps.io/LoQs/")
                            
                   ),
                    
                    menuItem("Overview", tabName = "OVERVIEW", selected = TRUE),
                    
                    menuItem("Results",  startExpanded = TRUE,
                             menuSubItem("Sepal.Length", tabName = "RESULTS"),
                             menuSubItem("Sepal.Width" , tabName = "RESULTS"),
                             menuSubItem("Petal.Length", tabName = "RESULTS"),
                             menuSubItem("Petal.Width" , tabName = "RESULTS")
                    ), 
                    menuItem("Help", tabName = "HELP")
        )
        
    ),
    dashboardBody(
        tabItems(
            tabItem("OVERVIEW", 
                    box(" ", 
                     #   tableOutput("AE2")) ###################################
                    htmlOutput("tableset") )
            ),
            tabItem("RESULTS", 
                    box("Results box", 
                        DT::dataTableOutput("mytable")
                        #plotOutput("results")
                    )
            ),
            tabItem("HELP", 
                    box("HELP box", 
                        textOutput("help"))
            ) 
        )
    )
)

server <- function(input, output, session) {
    
    
    data <- reactive({
        
        print(input$SideBarMENU)
        
        if(input$SideBarMENU %in% names(iris)){
            iris[[input$SideBarMENU]]
        } else {
            rnorm(100, 1000, 10)
        }
    })
    
    
    output$results <- renderPlot({
        hist(data())
    })
    
    
    # output$overview <- renderTable({ ###############################
    #     head(AE2)
    # })
    
    
    
    output$help <- renderText({
        HTML("A wiki is a website on which users collaboratively.....")
    })
    
    
    ###
    ###CODE
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # This is where a new sample is instigated and inputs converted to numeric
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    random.sample <- reactive({
        
        foo <- input$resample
        
        m <- as.numeric(input$m)
        
        n <-as.numeric(input$n)
        
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


    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # # BUILD A TABLE
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dat3 <- reactive({

       # sample <- random.sample()

       # db=dat()$db
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

    
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    dat4 <- reactive({
        
        # sample <- random.sample()
        
         db=dat()$db
        all=dat2()$all
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
       
        
 
    
        #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
        # Manage data for by System order class : Preferred terms, the right way


        # callx <- all[!all$count.AE %in% 0,]  # dispense with all obs with no AEs
         
         callx <- all[all$count.AE > 0,]  # dispense with all obs with no AEs
         
         s <- callx[, c("id","SOC","PT","treatment")]  # drop count vars
 
         fooz<-list()                        # this will be used to collect data
# 
        # analyse data in SOC chunks

        k <- sort(unique(s$SOC))       # get a vector of unique SOC
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
                rownames(z)[2] <- paste0("  ",rownames(ss1)[1])

                fooz[[i]] <- z

            } else {

                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # This step ensure if same SOC occurs more than once in a patient count only once
               # sss <-ss
                #sss$PT<-NULL
                #sss <-unique(ss ) #  [1] drop PT
                #Tot <- addmargins(table(sss$treatment))      #  [2]
                
                sss <-unique(ss[,c("id","SOC","treatment")]) #  [1] drop PT
                Tot <- addmargins(table(sss$treatment))    

                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                sss <- unique(ss[,c("id","PT","treatment")]) #  [1] drop SOC
                ss1 <- addmargins(table(sss$PT, sss$treatment))
                
                body <- ss1[ c(1:(nrow(ss1)-1)),]   # select all rows except last
                body <- body[ gtools::mixedsort(row.names(body)), ]  # sort alphanumeric
                z <- rbind( Tot , body)
                
                
                # sss <-ss
                # sss$SOC<-NULL
                # sss <- unique(ss ) #  [1] drop SOC
                # 
                # ss1 <- addmargins(table(sss$PT, sss$treatment))
                # 
                # body <- ss1[ c(1:(nrow(ss1)-1)),]   # select all rows except last
                # body <- body[ gtools::mixedsort(row.names(body)), ]  # sort alphanumeric
                
                z <- rbind( Tot,body )
                
                rownames(z)[1] <- k[i]    # add in SOC name

                fooz[[i]] <- z     # collect info

            }

            tmp <- do.call(rbind, fooz)      # bind the groups

            foo99 <- data.frame(tmp)
        }
        
        
               return(list( foo99 = foo99, s=s, callx=callx, all=all, tmp=tmp,k=k))
         
             })
# 
#         #  Build AE table
    dat5 <- reactive({
        
        N1=dat3()$N1
        N0=dat3()$N0
        foo99=dat4()$foo99
        
       # foo99 <- data.frame(foo99)
        
        
        
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
    # 
    # 
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
    
    output$mytable = DT::renderDataTable({
     
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
                  options = list(
                      # dom = 't',
                      columnDefs = list(list(type = 'natural', targets = c(4,5)))
                  ))
    })
    
    # output$AE2 <- renderTable({
    #    dat()$db
    # })
    
    output$AE2 <- renderTable({
        dat5()$foo
    }, rownames = TRUE, colnames = TRUE)
    # # 
    # # 
    # output$AE2 <- renderTable({
    #     dat3()$N0
    # })
    # output$AE2 <- renderTable({
    #     dat4()$s
    # })
    
    
    
    
    
    
}

shinyApp(ui, server)