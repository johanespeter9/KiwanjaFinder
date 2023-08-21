library(tidyverse) #Data manipulation and visualization
library(sf)#Sinstatial data reading and manipulation
library(tmap)#Thematic mapping for spatial data
library(magrittr)#Chaining operations for clearer code
library(shiny)#Creating interactive web applications
library(shinyWidgets)#Custom input controls for Shiny
library(shinythemes)#lumen,cosmo,paper,simplex,slate, superhero (Pre-designed themes for Shiny apps.)
library(shinydashboard)# Building dashboards in Shiny
library(shinyalert)#Customized alert dialogs for Shiny
library(thematic)#Alternative theme for ggplot2 plots
library(DT)#Interactive data tables for Shiny
library(stringr)#Convenient string manipulation functions
require(shadowtext)#Adding shadow effects to ggplot2 text
library(RColorBrewer)#olor palettes for appealing plots
library(ggplot2)#Creating static and dynamic visualizations
library(shinythemes)#
library(plotly)#Interactive and animated visualizations





#SETTING UP THE COLOR OF LANDUSES TO BE USED AS PALETTE IN TMAP
land_use_colors = c(
  "Botanical Park" = "#003600",
  "Nursery School" = "#BA1007",
  "Religious Site" = "#7A0B05",
  "Open Space" = "#00FF00",    
  "Fire Station" = "#FA160A",  
  "Primary School" = "#FF0000",
  "Hotel" = "#1C00BA",
  "Housing Estate" = "#FFA500",
  "Dc Resident" = "#BAB20F",
  "Public Building" = "#8B0000",
  "Stadium" = "#017500",       
  "Recreation Park" = "#01B500",
  "Cemetery Area" = "#00FF00", 
  "Petty Trade" = "#12007A",
  "Bus Stand" = "#FF4500",
  "Secondary School" = "#3B0502",
  "District Hospital" = "#E01409",
  "Community Hall" = "#FF0000",
  "Market" = "#2500FA",
  "Car Parking" = "#A52A2A",
  "Police Barracks" = "#7A1A05",
  "Collage" = "#BA2807",       
  "Dc Office" = "#FA360A",     
  "Work Department" = "#3B0D02",
  "District Council Office" = "#FF0000",
  "Service Industry" = "#800080",
  "Dispensary" = "#E02F08",    
  "Mtaa Office" = "#FF0000",   
  "Waste Collection Point" = "#A52A2A",
  "Park" = "#006400",
  "Shop" = "#09003B",
  "Hostel" = "#E1D811",        
  "Car Wash" = "#0000FF",
  "Petrol Station" = "#FF0000",
  "Playground" = "#01B500",    
  "Police Post" = "#FF0000",   
  "Residential" = "#FAEE14",
  "Commercial Residential" = "#2000E0",
  "Reserved Area" = "#90EE90",
  "Playfield" = "#01C200",     
  "Bus Stop" = "#FFA500",
  "Urban Farming" = "#019C00"  
)

#command is used to adjust the display of numeric values in scientific notation
options(scipen = 999)

# Adding data for the KiwanjaFinder Application
TPlots = st_read("data/Plots.gpkg") %>% st_make_valid()
Road = st_read("data/Road Network.gpkg") %>% st_make_valid()
Site = st_read("data/Site Location.gpkg") %>% st_make_valid()

# Extract unique values for Region and District
unique_regions=unique(TPlots$Region)
unique_districts=unique(TPlots$District)



# CREATING USER INTERFACE FOR THE PROGRAM


title=div(
  img(src = "tz2.png", height = 60, width = 60, align = "right"),
  h2(strong("KiwanjaFinder")), h3("Discover And Own Your Perfect Plot In Tanzania"),
  align = "center"
)
ui = fluidPage(Position="fixed-top",
  #Best theme to use lumen,cosmo,paper,simplex,slate, superhero
  theme = shinytheme("lumen"),
  # App title and logo
  titlePanel(title = title
    
  ),
  
  # Main content or main body of programm
  mainPanel(width="100%",
    tabsetPanel(id = "mainTabset",
                
      #ITERACTIVE MAP TABPANEL
      tabPanel(icon=icon("earth"),"KIWANJA -The Plot",
               
        fluidRow(
          
          column(12,
              column(2,style='color:green;',
                selectInput("selectedStatus", "Status", choices = c("Occupied", "Available"), multiple = TRUE, selected = c("Occupied", "Available")),
                sliderInput("plotSize", "Size of Plot in (SQM)", min = min(TPlots$`Area..SQM.`), max = max(TPlots$`Area..SQM.`), value = c(min(TPlots$`Area..SQM.`), max(TPlots$`Area..SQM.`))),
                sliderInput("plotCost", "Cost of Plot in (TZS)", min = min(TPlots$Cost), max = max(TPlots$Cost), value = c(min(TPlots$Cost), max(TPlots$Cost))),
                radioButtons("selectedRegion", "Region", choices = unique_regions, selected = "Mara", inline = TRUE),
                selectInput("selectedDistrict", "District", choices = unique_districts, selected = unique_districts, multiple = TRUE),
                selectInput("selectedLanduse", "Land Use", choices = NULL, multiple = TRUE)
              ),
              
              fluidRow(    
          column(9,h4(align="center",strong(style='color:green;',"MAP TO SHOW ALL PLOTS BASED ON SELECTED AREA")),strong(style='color:red;',"(ATTENTION: Attributes of plots are not real and currently not fo sale, they are only used for testing the system)"),tmapOutput("map", width = "100%",height = "600"))
              )
          )
        )
      
      ),
      
      #SECOND TABPANEL FOR ALL PLOTS AND FIGURES
      tabPanel(icon=icon("table"),"Table for All plots and Figures",
        fluidRow(
          column(12,
                 column(width = 4,br(),align="center",h4(style='color:green;',"Figure: Distribution of Plots by Status"),plotOutput("pieChart"), plotOutput("barPlot")),
                 
                 column(width = 8,br(),align="center",h4(style='color:green;',"Table: Distribution of Plots"),div(DTOutput("myTable"), style = "height: calc(100vh - 330px); overflow-y: scroll;"),plotOutput("barPlotlanduses"))
                 )
                )
               ),
      
      
      #THIRD TABPANEL PLOT REQUEST FORM
      
      tabPanel(icon=icon("list"),
        "Plot Requests Form",
        fluidRow(
          column(12,
                 column(6, h4("Check on map for confimation")),
                 column(6, htmlOutput("plotrequestform"))
                 )
                )
               ),
      
      #FORTH TABPANEL CONTACT US
      
      tabPanel(icon=icon("location"),"Office Location",
               
               fluidRow(column(12,
                               column(10,br(),htmlOutput("mylocation")),
                               column(2,h3(strong (style='color:green;',"Name:"),("Johanes Petro Mchela")),
                                      h5(strong (style='color:green;',"Contact:"),("+255716957044")),
                                      h5(strong (style='color:green;',"Region:"),("Dar es salaam")),
                                      h5(strong (style='color:green;',"District:"),("Kinondoni")),
                                      h5(strong (style='color:green;',"Ward:"),("Mikocheni")),
                                      h5(strong (style='color:green;',"Street:"),("Lukuledi Street")))
                               )
                        )
               ),
      
      
      
      
      tabPanel(icon=icon("info"),"Developer",
        fluidRow(column(12,
                        column(6, br(),br(),
                               img(src = "jo.png",width="400",height="400"),
                               br(),br(),
                               strong (style='color:green;',"Name:"),("Johanes Petro Machela"),br(),
                               br(strong (style='color:green;',"Email:"),("johanespeter9@gmail.com")),
                               br(strong (style='color:green;',"contact:"),("+255716957044/+255757248788")),
                               br(strong (style='color:green;',"Address:"),("Ubungo, Dar es Salaam"))
                               ),
                        
                        column(6,br(),br(),h2(style='color:green;',"About Me"),
                        p("Johanes Petro is a GIS and Technical Project Manager at Open Map DevelopmentTanzania with vast field experience supervising
                        data collection for urban resilience."),"Johanes holds a Bachelor of Science degree in Urban and Regional Planning and has extensive experience managing various projects within
                        and outside Tanzania, including Geoglam/Crop Mapping by Collecte Localisation Satellites(CLS) and TerraSphere,
                        Mills Mapping and School Mapping by United Nations World Food Program(WFP),Ramanihuria by World Bank, Tanzania Rural
                        Electrification (Tanzania Minigrid) - REA Phase III, Zambia off-grid, Lusaka Facilities,and Sanitation Programmes",
                        
                        tags$li(class = "dropdown", tags$a(href = "https://www.linkedin.com/in/johanes-petro-13663991/", icon("linkedin"))),
                        tags$li(class = "dropdown", tags$a(href = "https://twitter.com/johanespetro", icon("twitter"))),
                        tags$li(class = "dropdown", tags$a(href = "https://web.facebook.com/johanes.peter", icon("facebook"))),
                        h2(style='color:green;',"Acknowledgement"),
                        h4("Dr.Masumbuko Semba" ),
                        h5("The Nelson Mandela African Institution of Science and Technology (NM-AIST)" ),
                        h5("P.O.BOX 447 Arusha" ),
                        br(),
                        h4("George Milingay" ),
                        h5("Ofisi ya Rais Tawala za Mikoa na Serikali za Mitaa (TAMISEMI)" ),
                        h5("P.O.BOX 1923 Dodoma - Tanzania" )
                        
                        
                        )
                        )
                 )
              ),
      
      
      
      tabPanel(icon=icon("book"),"How to Use",
               
             fluidRow(br(),h3("Start by navigating through the different tabs to explore the app's features",align="centre"),
               
               column(6,
                      
                      
                      
                      
                      
                      h5(strong(style='color:green;',"The KIWANJA -The Plot tab")),
                      p("Allows you to filter plots based on different criteria such as status, plot size, plot cost, region, district, and land use. Adjust the filters to narrow down your search and see the filtered plots displayed on the interactive map."),
                      
                      h5(strong(style='color:green;',"The Table for All Plots and Figures tab")),
                      p("You can view a table that shows all the plots available. Additionally, you can see figures such as the total number of plots per region in a pie chart and bar plot."),
                      
                      h5(strong(style='color:green;',"The Plot Requests Form tab ")),
                      p("You can interact with the form provided on this tab to make plot requests."),
                      h5(strong(style='color:green;',"The Office Location tab")),
                      p("Provides information about the office location, including the name, contact details, region, district, ward, and street"),
                      
                      h5(strong(style='color:green;',"The Developers tab")),
                      p("Gives an overview of the developers involved in creating the app. It provides their names, contact information, and a brief description of their experience and qualifications."),
                      
                      h5(strong(style='color:green;',"The How to Use tab")),
                      p("Contain additional instructions or guidance on how to use the app effectively. Review this tab if you need further assistance.
                      Feel free to explore and interact with the different tabs and features of the app to find your perfect plot in Tanzania or get the information you need.
                      If you have any questions or issues while using the app, you can contact the developer mentioned in the Developers tab for assistance.") 
                      
                      
                      )
               
               
               
             )  
               
               
               
               
               
               ),
      
    )
  )
)

# Define server logic
server = function(input, output, session) {
  # Update district choices based on selected region
  observeEvent(input$selectedRegion, {
    districts = unique(TPlots$District[TPlots$Region %in% input$selectedRegion])
    updateSelectInput(session, "selectedDistrict", choices = districts, selected = districts[1])
  })

  # Update land use choices based on selected district
  observeEvent(c(input$selectedRegion, input$selectedDistrict), {
    landUses = unique(
      TPlots$Land.uses[
        TPlots$Region %in% input$selectedRegion &
          TPlots$District %in% input$selectedDistrict
      ]
    )
    updateSelectInput(session, "selectedLanduse", choices = landUses, selected = landUses)
  })

  # Plot the map based on selected filters
  output$map = renderTmap({
    Selected.Plots = TPlots %>%
     filter(
        `Area..SQM.` >= input$plotSize[1] & `Area..SQM.` <= input$plotSize[2] &
          Cost >= input$plotCost[1] & Cost <= input$plotCost[2] &
          Region %in% input$selectedRegion &
          District %in% input$selectedDistrict &
          Land.uses %in% input$selectedLanduse &
          Status %in% input$selectedStatus
      )

    tm_shape(Selected.Plots ) +

      tm_polygons(col = "Land.uses",palette = land_use_colors, popup.vars = c("Plot.Number", "Area..SQM.", "Status", "Cost","Region","District")) +
      
      tm_shape(Road) +
      tm_polygons(col = "#525252",lwd=2,border.col = "white")+
      tmap_options(max.categories = 50)+
    tm_basemap("Esri.WorldImagery")+
    tm_basemap("OpenStreetMap") 
      
  })

  
  
  
  # Calculate the total number of plots per region
  totalPlotsstatus <- reactive({
    TPlots %>%
      st_drop_geometry() %>%
      group_by(Status) %>%
      summarise(Total_Plots = n())
  })
  
  
  
  
  output$pieChart <- renderPlot({
    # Calculate percentage
    totalPlots <- totalPlotsstatus()
    totalPlots$Percentage <- totalPlots$Total_Plots / sum(totalPlots$Total_Plots) * 100
    
    ggplot(totalPlots, aes(x = "", y = Total_Plots, fill = Status)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar(theta = "y") +
      geom_text(aes(label = paste0(Total_Plots, " (", sprintf("%.1f", Percentage), "%)")),
                position = position_stack(vjust = 0.5), size = 3) +  # Add count and percentage labels
      scale_fill_manual(values = c("#006400", "orange")) +  # Assign colors to statuses
      labs(fill = "Status") +
      theme_void() +
      theme(legend.position = "bottom")
  })
  
  
  
 
  
  output$mylocation = renderUI({
    HTML('<iframe width="100%" height="600" frameborder="0" scrolling="no" marginheight="0" marginwidth="0" src="https://www.openstreetmap.org/export/embed.html?bbox=39.25722688436508%2C-6.774640467664486%2C39.26067620515824%2C-6.772762709663773&amp;layer=mapnik&amp;marker=-6.77370158957782%2C39.25895154476166" style="border: 1px solid black"></iframe><br/><small><a href="https://www.openstreetmap.org/?mlat=-6.77370&amp;mlon=39.25895#map=19/-6.77370/39.25895">Maximuise Map to show big map</a></small>')
  })
  
  
  output$plotrequestform = renderUI({HTML('<iframe src="https://docs.google.com/forms/d/e/1FAIpQLScW7hg73HyXr1Pxf0YIKvO-FDW2vZ8bvCqglrSLb4wfVQAfdQ/viewform?embedded=true" width="640" height="1530" frameborder="0" marginheight="0" marginwidth="0">Loading…</iframe>')
  })
  
  
  
  output$myTable <- renderDT({
    TPlots <- TPlots %>% st_drop_geometry()
    
    datatable(TPlots) %>% 
      formatStyle('Status', backgroundColor = styleEqual(c('Available', 'Occupied'), c('green', 'orange')))
  })
  
  
  
  
   
  
  
  
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
