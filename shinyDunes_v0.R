library(shiny)
library(DT)
library(data.table)
library(ggplot2)
library(reshape2)
## Provide static list for fixed Indicia input variables
## but capture variable ones by asking people to type in the group names e.g.
siteList <- read.csv(file = "data/siteList.csv", h = T, stringsAsFactors = F)
dipwellsTest <- read.csv(file = "data/dipwellsTest.csv", h = T)
quadratTest <- read.csv(file = "data/dunePostgresQuadratV1.csv", h = T)
profilesTest <- read.csv(file = "data/duneProfilesTest.csv", h = T)
structureTest <- read.csv(file = "data/duneVegStructureTest.csv", h = T)
zonesTest <- read.csv(file = "data/duneZonationTest.csv", h = T)

#######################################################################################################################################################################################
#######################################################################################################################################################################################
#### Define UI
#######################################################################################################################################################################################
#######################################################################################################################################################################################
ui <- shinyUI(fluidPage(
  mainPanel(h1("Dunescapes data visualisation platform"),
    tabsetPanel(
      #############################################################
      tabPanel(h3("0. Introduction"),
              br(),
              helpText(h4("This app has been designed to enable some basic visualisations and manipulations of data collected under the Dunescapes project.")),
              helpText(h4("Demo files indicating the expected structure (as per the project site downloads section) are given as downloads below.")),
              hr(),
              downloadLink('downloadData0.1', h4('Example for 1. Dipwell data')),
              br(),
              downloadLink('downloadData0.2', h4('Example for 2. Sand dune profiles')),
              br(),
              downloadLink('downloadData0.3', h4('Example for 3. Dune habitat zonation')),
              br(),
              downloadLink('downloadData0.4.5', h4('Example for 4. Quadrat data reformatting and 5. Indicator species summaries')),
              br(),
              downloadLink('downloadData0.6', h4('Example for 6. Vegetation structure')),
              hr(),
              tags$h3("Background"),
              "Whilst this platform is intended for admin downloads from the ",
              tags$a(href="https://dunescapes.brc.ac.uk", 
                     "Dunescapes project,"),
              "any data in the same format as these files could be uploaded.",
              br(),
              "This demo platform was originally designed by Dr O.L. Pescott (UKCEH).",
              "The code will be posted at his ",
              tags$a(href="https://github.com/sacrevert/dunescapeR", "GitHub.")
        ),

      tabPanel(h3("1. Dipwell data"),
              br(),
              fileInput('target_upload1', 'Choose CSV file to upload',
                         accept = c(
                           '.csv'
                         )),
              selectInput(inputId = "site", label = ("Choose site(s)"),
                      choices = unique(siteList$Site), selected = NULL, multiple = T),
              hr(),
              helpText(h4("To use the below filters, please ensure that only one site has been selected")),
              helpText(h5("See the data table below for available dipwell and dipwell group names at your site of interest")),
              selectizeInput(inputId = "fixed", label = ("Enter dipwell name(s)"),
                          choices = c(""), options = list(create = TRUE), multiple = T),
              selectizeInput(inputId = "group", label = ("Enter dipwell group name(s)"),
                             choices = c(""), options = list(create = TRUE), multiple = T),
              hr(),
              helpText(h4("Note that specific named dipwells or dipwell group plots require a single site to have been selected")),
              plotOutput("plot1"),
              ##########################
              ## PLOT DOWNLOAD BUTTON ##
              ##########################
              hr(),
              helpText(h4("Filter plot by year range and/or months")),
              ####################################################
              ## ADD Y-AXIS ADJUSTMENTS AND INCREASE FONT SIZES ##
              ####################################################
              sliderInput("years",
                          label="Year range:",
                          min=1980, max=2030, value = c(1980,2030), sep = ""),
              sliderInput("months",
                          label="Month range:",
                          min=1, max=12, value = c(1,12)),
              hr(),
              br(),
              br(),
              helpText(h4("Filter table using the search box")),
              dataTableOutput("sample_table1"),
              br(),
              br()
        ),
      #############################################################
      tabPanel(h3("2. Sand dune profiles"),
               br(),
               fileInput('target_upload2', 'Choose CSV file to upload',
                         accept = c(
                           '.csv'
                         )),
               selectInput(inputId = "site2", label = ("Choose site"),
                           choices = unique(siteList$Site), selected = NULL, multiple = F),
               hr(),
               helpText(h4("See the data table below for available transects at your site of interest")),
               selectizeInput(inputId = "transect2", label = ("Enter transect name"),
                              choices = c(""), options = list(create = TRUE), multiple = F),
               hr(),
               helpText(h4("Note that only one site/transect combination can be viewed at a time")),
               plotOutput("plot2"),
               ##########################
               ## PLOT DOWNLOAD BUTTON ##
               ##########################
               hr(),
               helpText(h4("Filter plot by year range and/or months")),
               ####################################################
               ## ADD Y-AXIS ADJUSTMENTS AND INCREASE FONT SIZES ##
               ####################################################
               sliderInput("years2",
                           label="Year range:",
                           min=1980, max=2030, value = c(1980,2030), sep = ""),
               sliderInput("months2",
                           label="Month range:",
                           min=1, max=12, value = c(1,12)),
               hr(),
               br(),
               br(),
               helpText(h4("Filter table using the search box")),
               br(),
               dataTableOutput("sample_table2"),
               br(),
               hr(),
               br(),
               br(),
               helpText(h4("The data plotted above will appear below")),
               br(),
               dataTableOutput("plottedData2"),
               ###################################
               #### PLOT DATA DOWNLOAD BUTTON ####
               ###################################
               br(),
               br()
        ),
      #############################################################
      tabPanel(h3("3. Dune habitat zonation"),
               br(),
               fileInput('target_upload3', 'Choose CSV file to upload',
                         accept = c(
                           '.csv'
                         )),
               selectInput(inputId = "site3", label = ("Choose site"),
                           choices = unique(siteList$Site), selected = NULL, multiple = F),
               hr(),
               helpText(h4("See the data table below for available transects at your site of interest")),
               selectizeInput(inputId = "transect3", label = ("Enter transect name"),
                              choices = c(""), options = list(create = TRUE), multiple = F),
               hr(),
               helpText(h4("Note that only one site/transect combination can be viewed at a time")),
               helpText(h5("Habitat types are currently plotted in a standard order, not according to their spatial arrangement on a transect")),
               plotOutput("plot3"),
               ##########################
               ## PLOT DOWNLOAD BUTTON ##
               ##########################
               hr(),
               helpText(h4("Filter plot by year range and/or months")),
               ####################################################
               ## ADD Y-AXIS ADJUSTMENTS AND INCREASE FONT SIZES ##
               ####################################################
               sliderInput("years3",
                           label="Year range:",
                           min=1980, max=2030, value = c(1980,2030), sep = ""),
               sliderInput("months3",
                           label="Month range:",
                           min=1, max=12, value = c(1,12)),
               hr(),
               br(),
               br(),
               helpText(h4("Filter table using the search box")),
               br(),
               dataTableOutput("sample_table3"),
               br()
        ),
      #############################################################
      tabPanel(h3("4. Quadrat data reformatting"),
               br(),
               fileInput('target_upload4', 'Choose CSV file to upload',
                         accept = c(
                           '.csv'
                         )),
               selectInput(inputId = "site4", label = ("Choose site"),
                           choices = c(unique(siteList$Site), "Select..." = ""), selected = "", multiple = F),
               hr(),
               br(),
               dataTableOutput("sample_table4.1"),
               br(),
               downloadLink('downloadData4.1', h4('Download all reshaped sample data (species in columns plus sample info)')),
               br(),
               hr(),
               br(),
               dataTableOutput("sample_table4.2"),
               br(),
               downloadLink('downloadData4.2', h4('Download all reshaped sample data (simplified, species in rows)')),
               br()
        ),
      #############################################################
      tabPanel(h3("5. Indicator species summaries"),
               br(),
               fileInput('target_upload5', 'Choose CSV file to upload',
                         accept = c(
                           '.csv'
                         )),
               selectInput(inputId = "site5", label = ("Choose site"),
                           choices = c(unique(siteList$Site), "Select..." = ""), selected = "", multiple = F),
               hr(),
               selectInput(inputId = "type5", label = ("Choose indicator type"),
                  choices = c("Health indicators" = "H", "Nitrogen indicators" = "N", "Select..." = ""), selected = "", multiple = F),
               hr(),
               plotOutput("plot5"),
               hr(),
               helpText(h4("Filter plot by year range and/or months")),
               ####################################################
               ## ADD Y-AXIS ADJUSTMENTS AND INCREASE FONT SIZES ##
               ####################################################
               sliderInput("years5",
                           label="Year range:",
                           min=1980, max=2030, value = c(1980,2030), sep = ""),
               sliderInput("months5",
                           label="Month range:",
                           min=1, max=12, value = c(1,12))
          ),
      #############################################################
      tabPanel(h3("6. Vegetation structure"),
               br(),
               fileInput('target_upload6', 'Choose CSV file to upload',
                         accept = c(
                           '.csv'
                         )),
               selectInput(inputId = "site6", label = ("Choose site"),
                           choices = c(unique(siteList$Site), "Select..." = ""), selected = "", multiple = F),
               hr(),
               h4(tags$b("Your available quadrat groups are:"), textOutput("text6", inline = TRUE)),
               helpText(h4("Type your selection in the box below")),
               selectizeInput(inputId = "group6", label = ("Enter group name"),
                              choices = c(""), options = list(create = TRUE), multiple = F, selected = ""),
               hr(),
               helpText(h4("Filter plot by year range and/or months")),
               ####################################################
               ## ADD Y-AXIS ADJUSTMENTS AND INCREASE FONT SIZES ##
               ####################################################
               sliderInput("years6",
                           label="Year range:",
                           min=1980, max=2030, value = c(1980,2030), sep = ""),
               sliderInput("months6",
                           label="Month range:",
                           min=1, max=12, value = c(1,12)),
               hr(),
               plotOutput("plot6"),
               hr(),
               br(),
               dataTableOutput("sample_table6"),
               br(),
               downloadLink('downloadData6', h4('Download averaged vegetation structure data for this site and quadrat group')),
               br()
          )
        )
      )
    )
  )
#######################################################################################################################################################################################
#######################################################################################################################################################################################
#### Define server logic
#######################################################################################################################################################################################
#######################################################################################################################################################################################
server <- shinyServer(function(input, output) {
  ###############################################
  ## Help page stuff
  output$downloadData0.1 <- downloadHandler(
    filename = function() {
      c("1_dipwellsTest.csv")
    },
    content = function(file) {
      write.csv(dipwellsTest, file, row.names = F)
    }
  )
  
  output$downloadData0.2 <- downloadHandler(
    filename = function() {
      c("2_profilesTest.csv")
    },
    content = function(file) {
      write.csv(profilesTest, file, row.names = F)
    }
  )
  
  output$downloadData0.3 <- downloadHandler(
    filename = function() {
      c("3_zonesTest.csv")
    },
    content = function(file) {
      write.csv(zonesTest, file, row.names = F)
    }
  )
  
  output$downloadData0.4.5 <- downloadHandler(
    filename = function() {
      c("4and5_quadratTest.csv")
    },
    content = function(file) {
      write.csv(quadratTest, file, row.names = F)
    }
  )
  
  output$downloadData0.6 <- downloadHandler(
    filename = function() {
      c("6_structureTest.csv")
    },
    content = function(file) {
      write.csv(structureTest, file, row.names = F)
    }
  )
  
  ###############################################
  upload1 <- reactive({
    inFile <- input$target_upload1
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = ',')
    return(df)
  })
  
  output$plot1 <- renderPlot({
    df <- upload1()
    if (is.null(df))
      return(NULL)
    df$Date <- as.Date(df$Date, format  = "%d/%m/%Y")
    df$year <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%Y")
    df$month <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%m")
    df <- df[(as.numeric(df$year) >= input$years[1]) & (as.numeric(df$year) <= input$years[2]),]
    df <- df[(as.numeric(df$month) >= input$months[1]) & (as.numeric(df$month) <= input$months[2]),]
    if (is.null(input$site) && is.null(input$fixed) && is.null(input$group)) {
      ggplot(data = df, aes(x = Date, y = Water.Depth, color = Site)) +
        geom_line() + 
        geom_point()# +
    } else if (!is.null(input$site) && is.null(input$fixed) && is.null(input$group)) {
      dfS <- df[df$Site %in% input$site,]
      ggplot(data = dfS[dfS$Site %in% input$site,],
             aes(x = Date, y = Water.Depth, color = Site)) +
        geom_line() + 
        geom_point()# +
    } else if (!is.null(input$site) && !is.null(input$fixed) && is.null(input$group) && (length(input$site) == 1)) {
      dfS <- df[df$Site %in% input$site,]
      ggplot(data = dfS[dfS$Fixed.point.label %in% input$fixed,],
                      aes(x = Date, y = Water.Depth, color = Fixed.point.label)) +
        geom_line() + 
        geom_point()# +
    } else if (!is.null(input$site) && !is.null(input$fixed) && is.null(input$group) && (length(input$site) > 1)) {
        return(NULL)
    } else if (!is.null(input$site) && !is.null(input$fixed) && !is.null(input$group) && (length(input$site) == 1)) {
      dfS <- df[df$Site %in% input$site,]
      ggplot(data = dfS[(dfS$Fixed.point.label %in% input$fixed) && (dfS$Group %in% input$group),],
             aes(x = Date, y = Water.Depth, color = Fixed.point.label, shape = Group)) +
        geom_line() + 
        geom_point()# +
    } else if (!is.null(input$site) && !is.null(input$fixed) && !is.null(input$group) && (length(input$site) > 1)) {
      return(NULL)
    } else if (!is.null(input$site) && is.null(input$fixed) && !is.null(input$group) && (length(input$site) == 1)) {
      dfS <- df[df$Site %in% input$site,]
      ggplot(data = dfS[dfS$Group %in% input$group,],
             aes(x = Date, y = Water.Depth, shape = Group)) +
        geom_line() + 
        geom_point()# +
    } else if (!is.null(input$site) && is.null(input$fixed) && !is.null(input$group) && (length(input$site) > 1)) {
      return(NULL)
  }
  })
  
  output$sample_table1<- DT::renderDataTable({
    df <- upload1()
    datatable(df)
  })
  
  ############################################################################################################
  
  upload2 <- reactive({
    inFile <- input$target_upload2
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = ',')
    return(df)
  })
  
  output$plot2 <- renderPlot({
    df <- upload2()
    if (is.null(df))
      return(NULL)
    df <- df[order(df$Site, df$Transect, df$Parent.sample.ID, df$Sample.ID),]
    cols <- c("Site", "Transect", "Parent.sample.ID", "Sample.ID")
    df[cols] <- lapply(df[cols], factor)
    df$Date <- as.Date(df$Date, format  = "%d/%m/%Y")
    df$year <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%Y")
    df$month <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%m")
    df <- df[(as.numeric(df$year) >= input$years2[1]) & (as.numeric(df$year) <= input$years2[2]),]
    df <- df[(as.numeric(df$month) >= input$months2[1]) & (as.numeric(df$month) <= input$months2[2]),]
    #if (!is.null(input$site) && !is.null(input$transect)) {
      dfST <- df[(as.character(df$Site) == input$site2) && (as.character(df$Transect) == input$transect2),]
      dfST$adjDis <- apply(as.matrix(dfST[,c(10:11)]), MARGIN = 1, function(x) cos( abs(x[1]) * pi/180 ) * x[2] ) # note that the R trig functions are in radians, hence pi/180 conversion
      dfST$xDis <- ave(x = dfST$adjDis, dfST$Site, dfST$Transect, dfST$Parent.sample.ID, FUN = cumsum) # cumulative distance within parent sample of a transect
      dfST$oppHei <- apply(as.matrix(dfST[,c(10:11)]), MARGIN = 1, function(x) sin( abs(x[1])  * pi/180 ) * x[2] ) # note that the R trig functions are in radians, hence pi/180 conversion
      dfST$relHei <- apply(as.matrix(dfST[,c(10,16)]), MARGIN = 1, function(x) ifelse(x[1] < 0,  0 - x[2], x[2]))
      dfST$yHei <- ave(x = dfST$relHei, dfST$Site, dfST$Transect, dfST$Parent.sample.ID, FUN = cumsum)
      ggplot(data = dfST, aes(x = xDis, y = yHei, shape = as.factor(year), colour = Parent.sample.ID)) +
        geom_point() +
        geom_line()
    #} else {
    #  return(NULL)
    #}
  })
  
  output$sample_table2<- DT::renderDataTable({
    df <- upload2()
    df <- df[!names(df) %in% c("Dune.Slope.Type.term.ID")] # drop useless column for display purposes
    datatable(df)
  })
  
  # This was partly for error checking, but useful to have and see for people to check the trig and/or download
  output$plottedData2<- DT::renderDataTable({
    df <- upload2()
    if (is.null(df))
      return(NULL)
    df <- df[order(df$Site, df$Transect, df$Parent.sample.ID, df$Sample.ID),]
    cols <- c("Site", "Transect", "Parent.sample.ID", "Sample.ID")
    df[cols] <- lapply(df[cols], factor)
    df$Date <- as.Date(df$Date, format  = "%d/%m/%Y")
    df$year <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%Y")
    df$month <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%m")
    df <- df[(as.numeric(df$year) >= input$years2[1]) & (as.numeric(df$year) <= input$years2[2]),]
    df <- df[(as.numeric(df$month) >= input$months2[1]) & (as.numeric(df$month) <= input$months2[2]),]
    dfST <- df[(as.character(df$Site) == input$site2) && (as.character(df$Transect) == input$transect2),]
    dfST$adjDis <- apply(as.matrix(dfST[,c(10:11)]), MARGIN = 1, function(x) cos( abs(x[1]) * pi/180 ) * x[2] ) # note that the R trig functions are in radians, hence pi/180 conversion
    dfST$xDis <- ave(x = dfST$adjDis, dfST$Site, dfST$Transect, dfST$Parent.sample.ID, FUN = cumsum) # cumulative distance within parent sample of a transect
    dfST$oppHei <- apply(as.matrix(dfST[,c(10:11)]), MARGIN = 1, function(x) sin( abs(x[1])  * pi/180 ) * x[2] ) # note that the R trig functions are in radians, hence pi/180 conversion
    dfST$relHei <- apply(as.matrix(dfST[,c(10,16)]), MARGIN = 1, function(x) ifelse(x[1] < 0,  0 - x[2], x[2]))
    dfST$yHei <- ave(x = dfST$relHei, dfST$Site, dfST$Transect, dfST$Parent.sample.ID, FUN = cumsum)
    #######################################################################################
    ### WOULD BE GOOD TO CROP THIS TABLE, IMPROVE NAMES, AND CONSTRAIN NUMBER OF DIGITS ###
    datatable(dfST)
    #######################################################################################
  })
  
  
  ############################################################################################################
  
  upload3 <- reactive({
    inFile <- input$target_upload3
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = ',')
    return(df)
  })
  
  output$plot3 <- renderPlot({
    df <- upload3()
    if (is.null(df))
      return(NULL)
    df <- df[order(df$Site, df$Transect, df$Parent.sample.ID, df$Sample.ID),]
    cols <- c("Site", "Transect", "Parent.sample.ID", "Sample.ID")
    df[cols] <- lapply(df[cols], factor)
    df$Date <- as.Date(df$Date, format  = "%d/%m/%Y")
    df$year <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%Y")
    df$month <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%m")
    df <- df[(as.numeric(df$year) >= input$years3[1]) & (as.numeric(df$year) <= input$years3[2]),]
    df <- df[(as.numeric(df$month) >= input$months3[1]) & (as.numeric(df$month) <= input$months3[2]),]
    dfST <- df[(as.character(df$Site) == input$site3) && (as.character(df$Transect) == input$transect3),]
    dfST <- df[!is.na(dfST$Dune.Zone.Distance),] # drop Starting point rows
    dfST$Dune.Previous.Habitat.term <- apply(as.matrix(dfST[,c(10,12)]), MARGIN = 1, function(x) ifelse(x[1]=="", x[2], x[1])) # move current hab to previous column for final points
    dfST$trSampTot <- ave(x = dfST$Dune.Zone.Distance, dfST$Site, dfST$Transect, dfST$Parent.sample.ID, FUN = sum)
    dfST$Percent <- round((dfST$Dune.Zone.Distance/dfST$trSampTot)*100, 1)
    names(dfST)[10] <- "Habitat"
    names(dfST)[8] <- "Metres"
    dfST$Habitat <- as.factor(dfST$Habitat)
    ### NOTE THAT AN ISSUE HERE IS THAT THE HABITATS ARE STACKED BY FACTOR ORDER, NOT BY APPEARING ON THE TRANSECT ###
    # NO OBVIOUS WAY TO CHANGE THIS -- FACTORS LEVELS COULD BE CHANGED FOR EACH SITE/TRANSECT COMBINATION, BUT
    # SPATIALLY SEPARATE HABITAT BLOCKS OF THE SAME TYPE WOULD STILL APPEAR TOGETHER (I.E. IN THE WRONG PLACE)
    # COULD ADD A SECTION NUMBER (1,2,...,n) AS AN ADDITIONAL LABEL?
    if (length(unique(dfST$year)) > 1) {
      ggplot(dfST, aes(x=Parent.sample.ID, y=Metres, fill=Habitat)) +
        geom_bar(stat="identity", width = .7, colour="black", lwd=0.1) +
        geom_text(aes(label=ifelse(Percent > 1, paste0(sprintf("%.0f", Percent),"%"),"")),
                  position=position_stack(vjust=0.5), colour="white") +
        coord_flip() +
        facet_wrap(~year)
    } else {
      ggplot(dfST, aes(x=Parent.sample.ID, y=Metres, fill=Habitat)) +
        geom_bar(stat="identity", width = .7, colour="black", lwd=0.1) +
        geom_text(aes(label=ifelse(Percent > 1, paste0(sprintf("%.0f", Percent),"%"),"")),
                  position=position_stack(vjust=0.5), colour="white") +
        coord_flip()
    }
  })
  
  output$sample_table3<- DT::renderDataTable({
    df <- upload3()
    datatable(df)
  })
  
  ############################################################################################################
  
  upload4 <- reactive({
    inFile <- input$target_upload4
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = ',')
    return(df)
  })
  
  df_wide4 <- reactive({
    if (input$site4 == "")
      return(NULL)
    df <- upload4()
    names(df)[1] <- "SampleID"
    names(df)[5] <- "QuadratName"
    names(df)[11] <- "Gridref"
    df <- df[!(df$Group==""),]
    df <- df[df$Site == input$site4,]
    df$Cover <- apply(as.matrix(df[9:10]), MARGIN = 1, function(x) ifelse(is.na(x[1]), x[2], x[1]))
    df <- df[,c("SampleID", "Parent.sample.ID","QuadratName","Date","Occurrence.ID", "Gridref", "Cover", "Species")]
    df$Cover[is.na(df$Cover)] <- 0
    df_wide <- dcast(df, SampleID + Parent.sample.ID + QuadratName + Date + Gridref ~ Species, value.var="Cover", fill = 0)
  })
  
  transformedDat4 <- reactive({
    if (input$site4 == "")
      return(NULL)
    df <- upload4()
    names(df)[1] <- "SampleID"
    names(df)[5] <- "QuadratName"
    names(df)[11] <- "Gridref"
    df <- df[!(df$Group==""),]
    df$Cover <- apply(as.matrix(df[9:10]), MARGIN = 1, function(x) ifelse(is.na(x[1]), x[2], x[1]))
    df <- df[,c("SampleID", "Parent.sample.ID","QuadratName","Date","Occurrence.ID", "Gridref", "Cover", "Species")]
    df$Cover[is.na(df$Cover)] <- 0
    df_wide <- dcast(df, SampleID + Parent.sample.ID + QuadratName + Date + Gridref ~ Species, value.var="Cover", fill = 0)
    transformedDat <- t(df_wide[,-which( names(df_wide) %in% c("Parent.sample.ID","QuadratName","Date","Gridref"))])
    colnames(transformedDat) <- transformedDat[1,]
    transformedDat <- transformedDat[-1,]
  })
  
  output$sample_table4.1<- DT::renderDataTable({
    df <- upload4()
    if (is.null(df))
      return(NULL)
    if (input$site4 == "")
      return(NULL)
    names(df)[1] <- "SampleID"
    names(df)[5] <- "QuadratName"
    names(df)[11] <- "Gridref"
    df <- df[!(df$Group==""),]
    df$Cover <- apply(as.matrix(df[9:10]), MARGIN = 1, function(x) ifelse(is.na(x[1]), x[2], x[1]))
    df <- df[,c("SampleID", "Parent.sample.ID","QuadratName","Date","Occurrence.ID", "Gridref", "Cover", "Species")]
    df$Cover[is.na(df$Cover)] <- 0
    df_wide <- dcast(df, SampleID + Parent.sample.ID + QuadratName + Date + Gridref ~ Species, value.var="Cover", fill = 0)
    datatable(df_wide)
  })
  
  output$sample_table4.2<- DT::renderDataTable({
    df <- upload4()
    if (is.null(df))
      return(NULL)
    if (input$site4 == "")
      return(NULL)
    names(df)[1] <- "SampleID"
    names(df)[5] <- "QuadratName"
    names(df)[11] <- "Gridref"
    df <- df[!(df$Group==""),]
    df$Cover <- apply(as.matrix(df[9:10]), MARGIN = 1, function(x) ifelse(is.na(x[1]), x[2], x[1]))
    df <- df[,c("SampleID", "Parent.sample.ID","QuadratName","Date","Occurrence.ID", "Gridref", "Cover", "Species")]
    df$Cover[is.na(df$Cover)] <- 0
    df_wide <- dcast(df, SampleID + Parent.sample.ID + QuadratName + Date + Gridref ~ Species, value.var="Cover", fill = 0)
    transformedDat <- t(df_wide[,-which( names(df_wide) %in% c("Parent.sample.ID","QuadratName","Date","Gridref"))])
    colnames(transformedDat) <- transformedDat[1,]
    transformedDat <- transformedDat[-1,]              
    datatable(transformedDat)
  })
  
  output$downloadData4.1 <- downloadHandler(
     filename = function() {
       paste('wideVegDataSppCols-', Sys.Date(), '.csv', sep='')
     },
     content = function(file) {
       write.csv(df_wide4(), file, row.names = F)
     }
   )
  
  output$downloadData4.2 <- downloadHandler(
    filename = function() {
      paste('wideVegDataSppRows-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(transformedDat4(), file, row.names = T)
    }
  )
  
  ############################################################################################################
  
  upload5 <- reactive({
    inFile <- input$target_upload5
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = ',')
    return(df)
  })
  
  output$plot5 <- renderPlot({
    df5 <- upload5()
    if (is.null(df5))
      return(NULL)
    if (is.null(input$site5))
      return(NULL)
    names(df5)[1] <- "SampleID"
    names(df5)[5] <- "QuadratName"
    names(df5)[11] <- "Gridref"
    df5 <- df5[!(df5$Group==""),]
    df5 <- df5[df5$Site == input$site5,]
    df5$Date <- as.Date(df5$Date, format  = "%d/%m/%Y")
    df5$year <- format(as.Date(df5$Date, format  = "%Y-%m-%d"), "%Y")
    df5$month <- format(as.Date(df5$Date, format  = "%Y-%m-%d"), "%m")
    df5 <- df5[,c("SampleID", "Parent.sample.ID","QuadratName","Date","Occurrence.ID", "Gridref", "Health", "Nitrogen", "Species", "year", "month")]
    df5 <- df5[(as.numeric(df5$year) >= input$years5[1]) & (as.numeric(df5$year) <= input$years5[2]),]
    df5 <- df5[(as.numeric(df5$month) >= input$months5[1]) & (as.numeric(df5$month) <= input$months5[2]),]
    df5$Health[is.na(df5$Health)] <- 0
    df5$Nitrogen[is.na(df5$Nitrogen)] <- 0
    if (input$type5 == ""){
        return(NULL)
    } else if (input$type5 == "H") {
        df5HealSumSamp <- aggregate(Health ~ SampleID + QuadratName + year, data = df5, function(x) sum(x))
        df5HealSumQuad <- aggregate(Health ~ QuadratName + year, data = df5HealSumSamp, function(x) mean(x))
        ggplot(df5HealSumQuad, aes(x=QuadratName, y=Health)) +
          geom_bar(stat="identity", width = .7, lwd=0.1) +
          facet_wrap(~year)
    } else {
        df5NitrSumSamp <- aggregate(Nitrogen ~ SampleID + QuadratName + year, data = df5, function(x) sum(x))
        df5NitrSumQuad <- aggregate(Nitrogen ~ QuadratName + year, data = df5NitrSumSamp, function(x) mean(x))
        ggplot(df5NitrSumQuad, aes(x=QuadratName, y=Nitrogen)) +
          geom_bar(stat="identity", width = .7, lwd=0.1) +
          facet_wrap(~year)
    }
})
  
  ############################################################################################################
  
  upload6 <- reactive({
    inFile <- input$target_upload6
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE, sep = ',')
    return(df)
  })
  
  output$text6 <- renderText({
    df <- upload6()
    if (input$site6 == "")
      return(NULL)
    df <- df[df$Site == input$site6,]
    text6 <- unique(df$Group)
  }, sep = ";")
  
  df6 <- reactive({
    df <- upload6()
    if (is.null(df)) {
      return(NULL)
    } else if (input$site6 == "" | input$group6 == "") {
      return(NULL) 
    } else {
    df <- df[df$Site == input$site6,]
    df[is.na(df)] <- 0
    df$Date <- as.Date(df$Date, format  = "%d/%m/%Y")
    df$year <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%Y")
    df$month <- format(as.Date(df$Date, format  = "%Y-%m-%d"), "%m")
    df <- df[(as.numeric(df$year) >= input$years6[1]) & (as.numeric(df$year) <= input$years6[2]),]
    df <- df[(as.numeric(df$month) >= input$months6[1]) & (as.numeric(df$month) <= input$months6[2]),]
    df <- df[as.character(df$Group) == input$group6,]
    # original data gives 5 separate height measurements for a sample in Vegetation column, take mean
    df <- aggregate(Vegetation ~ Sample.ID + Fixed.point.label + Sand + Moss...Lichen +
                      Grasses + Herbs + Shrubs + Scrub + year + month, data = df, function(x) mean(x))
    names(df)[11] <- "VegHeight"
    df$trSampTot <- rowSums(df[,c(3:8,11)]) # not essential, but might display somewhere or feature in download
    df <- df[,c(1:2,9:10,3:8,11,12)]
    names(df)[2] <- "Quadrat"
    names(df)[6] <- "Moss.Lichen"
    # average over samples within a quadrat location and year
    df <- aggregate(. ~ Quadrat + year + month + trSampTot, data = df, function(x) mean(x)) # also takes mean of sample.id, which we delete later
    df <- df[,-which(names(df) %in% c("Sample.ID"))]
    names(df)[4] <- "Total.percent"
    return(df)
    }
  })
  
  output$plot6 <- renderPlot({
    df <- df6()
    if (is.null(df))
      return(NULL)
    dfL <- melt(df, id.vars=c("Quadrat", "year", "month", "Total.percent"), variable.name = "Type", value.name = "Percent")
    dfL$Quadrat <- as.factor(dfL$Quadrat)
    ## Plotting averages across any samples taken at a quadrat location within a year
    if (length(unique(dfL$year)) > 1) {
      ggplot(dfL, aes(x=Quadrat, y=Percent, fill=Type)) +
        geom_bar(stat="identity", width = .7, colour="black", lwd=0.1, drop = T) +
        geom_text(aes(label=ifelse(Percent > 5, paste0(sprintf("%.0f", Percent),"%"),"")),
                  position=position_stack(vjust=0.5), colour="white") +
        facet_wrap(vars(year)) + 
        ylab("Cumulative percent cover")
    } else {
      ggplot(dfL, aes(x=Quadrat, y=Percent, fill=Type)) +
        geom_bar(stat="identity", width = .7, colour="black", lwd=0.1, drop = T) +
        geom_text(aes(label=ifelse(Percent > 5, paste0(sprintf("%.0f", Percent),"%"),"")),
                  position=position_stack(vjust=0.5), colour="white") +
        ylab("Cumulative percent cover")
    }
  })
  
  output$sample_table6 <- DT::renderDataTable({
    df <- df6()
    if (is.null(df))
      return(NULL)
    datatable(df)
  })
  
  output$downloadData6 <- downloadHandler(
    filename = function() {
      paste('vegStructureAvgs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(df6(), file, row.names = F)
    }
  )
})
  
#######################################################################################################################################################################################
#######################################################################################################################################################################################
#### Run the application 
#######################################################################################################################################################################################
#######################################################################################################################################################################################
shinyApp(ui = ui, server = server)
#### END
