
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(rsconnect)
library(DT)
library(plotly)
library(devtools)
library(dashboardthemes)
require(maps)
require(mapdata)
library(ggplot2)
library(ggrepel)

packageVersion('plotly')

homic=read_csv("Homicide.csv")
homicide_col=homic %>%
          rename(Record_ID= "Record ID", Agency_Code="Agency Code", Agency_Name="Agency Name", Agency_Type="Agency Type",
                 Crime_Type="Crime Type", Crime_Solved="Crime Solved",
         Victim_Sex= "Victim Sex", Victim_Age="Victim Age", Victim_Race="Victim Race", Victim_Ethnicity="Victim Ethnicity", 
         Perpetrator_Sex="Perpetrator Sex",  Perpetrator_Age="Perpetrator Age", Perpetrator_Race="Perpetrator Race"  ,
         Perpetrator_Ethnicity= "Perpetrator Ethnicity",    Victim_Count="Victim Count" , Perpetrator_Count= "Perpetrator Count",
         Record_Source="Record Source"        
         
  )

homicide= homicide_col %>%
       filter(Victim_Age<=90 & Victim_Age>=10 & Perpetrator_Age<=90 & Perpetrator_Age>=10)



# Define UI for application that draws a histogram
ui <- dashboardPage(   
  
  dashboardHeader(title= "Homicide reports from 1980 to 2014"),
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem("Presentation", tabName = "present", icon=icon("bomb")),
      menuItem("Data",tabName ="datapage", icon = icon("database")  ),
      menuItem("Some numbers", tabName="basicanal", icon = icon("dashboard")),
      menuItem("Homicide by categories", tabName = "tercat", icon = icon("chart-pie")),
      menuItem("Over the years", tabName = "overyears", icon = icon("hourglass-half")),
      menuItem("similitude", tabName = "maps", icon = icon("globe"))
    )
    
  ),
  dashboardBody(   shinyDashboardThemes(
    theme = "boe_website"
  ),
  
  
  tabItems(
    tabItem(tabName = "present", 
            fluidPage(
              h1("Homicide in USA"),
              p("Homicide is the act of one human killing another.
                A homicide requires only a volitional act by another person that results in death, and thus a homicide may result from accidental,
                reckless, or negligent acts even if there is no intent to cause harm. Homicides can be divided into many overlapping legal
                categories, including murder, manslaughter and justifiable homicide. "),
              
              
              
              br(),
              br(),
             
              h3("Homicide Dashboard"),
              p(" The aim of this Dashboard is to explore the homicide incidents happened in the USA from 1980 to 2014. 
                
                The database used is -Killed Who? Homicide Reports 1980-2014- that you can find in Kaggle.
                
                We will be exploring the data, trying to extract information and make some discoveries about who killed who? The weapon used for 
                the crime?... and a lot of other questions.  "),
              br()
              
              
              
              
              
              )
            
              ),
    tabItem(tabName = "datapage",
            
            tabBox(id="tabset1", height = "100%",width = "100%" ,
                   tabPanel("Data", 
                            fluidPage( box(status="danger",width=12, DT::dataTableOutput("database")  ) )
                            
                   ),
                   tabPanel(  "Statistics", 
                              
                              fluidPage( box(status="danger", title="Summary", width= 12, verbatimTextOutput("strdat") ))
                   )
                   
            )
    ),
    tabItem(tabName = "basicanal",
            fluidPage(
              
                  
                     
                     valueBoxOutput("numbhomic"), 
                     valueBoxOutput("mostusedweapon"), 
                     valueBoxOutput("solvedperc"),
                     
           box(width=6,title = " ", status = "danger", solidHeader = FALSE, plotlyOutput("yesnosolved")),
           box(width=6,title = "Homicide through years ", status = "danger", solidHeader = TRUE, plotlyOutput("yearhomicide"))
                     
                     
                   
                     
            
              )
            ),
    
    tabItem(tabName = "tercat",
            
            tabBox(id="tabset2", height = "100%",width = "100%" ,
                   
                   
                   tabPanel("By state",
                            fluidPage(
                              box(width=12,title = "Homicide by states ", status = "danger", solidHeader = TRUE, plotlyOutput("bystate")),
                              box(width=12, status = "danger", solidHeader = FALSE, "California is the state that has recorded the highest number 
                                  of homicide records. Followed by Texas and Florida. New York takes the fourth place.")
                              
                              ) ),   
                   tabPanel("By relationship",
                            fluidPage(
                              box(width=12,title = "Different relationships between victim/perpetrator ", status = "danger", solidHeader = TRUE, plotlyOutput("byrelation")),
                              box(width=12, status = "danger", solidHeader = FALSE, "Most of the homicide crimes were made by acquaintance. Strangers take the second place. 
                                  Should be more aware of the people that we know?")
                              
                              
                              )
                            ),
                   
                   tabPanel("By weapon",
                            fluidPage(
                              box(width=8,title = "Weapons used for the homicide", status = "danger", solidHeader = TRUE, plotlyOutput("byweapon")),
                              box(width=4, status = "danger", solidHeader = FALSE, "The most used weapon is handgun. The second type the is knife.")
                              
                              
                               )   ),
                   tabPanel("By Record Source", 
                            fluidPage(
                              box(width=8,title = "Record sources ", status = "danger", solidHeader = TRUE, plotlyOutput("byrecord")),
                              box(width=4, status = "danger", solidHeader = FALSE, "Almost all the records were treated by the FBI")
                              
                              )  ),
                   tabPanel("By crime type",
                            fluidPage(
                              box(width=8,title = "Crime type  ", status = "danger", solidHeader = TRUE, plotlyOutput("bycrimetype")),
                              box(width=4, status = "danger", solidHeader = FALSE, "Almost all the homicide records in our data were concidered
                                  murder or manslaughter.")
                              
)    ), 
                 tabPanel("By sex",
                           fluidPage(
                                box(width=6,title = "Perpetrators sex  ", status = "danger", solidHeader = TRUE, plotlyOutput("byperpetsex")),
                                box(width=6,title = "Victims sex  ", status = "danger", solidHeader = TRUE, plotlyOutput("byvictimsex"))
           
         )    )           
                   
                   
                            )
            
            
                              ),
    
    tabItem(tabName = "overyears",
            
            tabBox(id="tabset3", height = "100%",width = "100%" ,
                   
                   tabPanel("By state",
                            fluidPage(
                              box(width=12,title = "Crimes through years ", status = "danger", solidHeader = TRUE, plotlyOutput("yearbystate")),
                              box(width=12, status = "danger", solidHeader = FALSE, "Since 1980, California, Texas and Florida have recorded respectivey the highest homicide records. 
                                  We can see though that the number decreased in 1994.")
                              
                              
                            ) ),   
                   tabPanel("By relationship",
                            fluidPage(
                              box(width=12,title = "Evolution of relationship conflicts", status = "danger", solidHeader = TRUE, plotlyOutput("yearbyrelation")),
                              box(width=12, status = "danger", solidHeader = FALSE, "We don't have available data for all the relationships so we can not say a lot of things. 
                                  However what we can say is that despite the lack of data we have concerning -acquaintanance- we can still see that this is the category that has the highest homicide records. ")
                              
                              
                            )
                   ),
                   
                   tabPanel("By weapon",
                            fluidPage(
                              box(width=8,title = "Evolution of weapons ", status = "danger", solidHeader = TRUE, plotlyOutput("yearbyweapon")),
                              box(width=4, status = "danger", solidHeader = FALSE, " Through years the handgun has remained by far the most used 
                                  weapon for murder.")
                              
                              
                            )   ),
                  
                   tabPanel("By age ",
                            fluidPage(
                              box(width=6,title = "Victims ages ", status = "danger", solidHeader = TRUE, plotlyOutput("victimage")),
                              box(width=6,title = "Perpetrator ages ", status = "danger", solidHeader = TRUE, plotlyOutput("perpetage")),
                              box(width=12, status = "danger", solidHeader = FALSE, "With these two plots above we can say that the highest number of crimes were recorded for people who are aged between 17 and 30.")
                              
                            )    )
                   
                   
            )
            
            
    ),
    
    tabItem("maps",
            
            
            
            
            tabBox(id="tabset2", height = "100%",width = "100%" ,
                   
                   
                   
                   tabPanel("Favorite weapon by sex",
                            fluidPage(
                              box(width=6,title = "Weapons prefered by Women ", status = "danger", solidHeader = TRUE, plotlyOutput("femaleweapon")),
                              box(width=6,title = "Weapons prefered by men ", status = "danger", solidHeader = TRUE, plotlyOutput("maleweapon")),
                              box(width=12, status = "danger", solidHeader = FALSE, "Hald of the male homicide crimes were done using a handgun. However
                                  for women the knife is also a good alternative since more than 30% of the crimes were done using a knife and less than 3% for poison. Who said that 
                                  poison is the women's favorite weapon?")
                              
                              
                             )   ),
                   
                   tabPanel("Relationships by sex", 
                            fluidPage(
                              box(width=6,title = "Relationships female perpetrators had with their victims ", status = "danger", solidHeader = TRUE, plotlyOutput("femalerelation")),
                              box(width=6,title = "Relationships male perpetrators had with their victims", status = "danger", solidHeader = TRUE, plotlyOutput("malerelation")),
                              box(width=12, status = "danger", solidHeader = FALSE, " Women's first target is their husbands, then people they know and then their boyfriends.
                                  For men, their targets are respectively, people they know, strangers and then their friends. Men be nice to your wives OR to your girlfiends!!!!")
                              
                              
                            )  )
                   
                  
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
                   
            )
            
            
            
            
    )
    
    
    
            )
  
  
  
            )
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  
  
  
  output$database <- DT::renderDataTable({homicide} ,
                                         #extensions="Responsive",
                                         options = list(
                                           columnDefs = list(list(className = 'dt-center', targets = 5)),
                                           pageLength = 5,
                                           lengthMenu = c(5, 10, 15, 20), scrollX = TRUE)   )
  
  
  
  
  output$strdat= renderPrint ({
    summary(homicide) })
  
  #############################################################################################
  
  output$numbhomic=renderValueBox({
    ll= homicide %>%
     count %>%
       collect
    
    valueBox(
      ll,"Number of homicide", icon = icon("male", lib = "glyphicon"),
      color = "maroon"
    )
    
    
  })
  
  output$mostusedweapon=renderValueBox({
    ll= homicide %>%
      group_by(Weapon) %>%
      summarise(nn=n()) %>%
      top_n(1) %>%
      collect
    
    valueBox(
      ll$Weapon,"Most used weapon", icon = icon("knife-kitchen", lib = "glyphicon"),
      color = "purple"
    )
    
    
  })
  
  output$solvedperc=renderValueBox({
    ll= homicide %>%
      filter(Crime_Solved=='Yes')%>%
      count %>%
      collect
    summm= homicide %>%
      count %>%
      collect
    
    perc=(ll$n*100)/summm
      
    valueBox(
      round(perc, 2) ,"Percentage of solved cases", icon = icon("male", lib = "glyphicon"),
      color = "maroon"
    )
    
    
  })
  

  output$yesnosolved= renderPlotly({
    ll= homicide %>% 
      group_by(Crime_Solved) %>%
      summarise(ncase=n())%>%
      collect
    
    
    plot_ly(ll, x = ~Crime_Solved, y = ~ncase,  type = 'bar' ,
            marker = list(color ="dimgray") ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Yes/No"),
             yaxis = list(title = "Number of homicide cases"))
    
  })
  
  output$yearhomicide= renderPlotly({
    ll=  homicide%>% 
      group_by(Year) %>%
      summarise(nyears=n())%>%
      collect
    
    
    plot_ly(ll, x = ~Year, y = ~nyears, type= 'scatter', mode='lines+markers' ,
            marker = list(color = c('rgba(222,45,38,0.8)'
                                    
            ))) %>%  
      
      layout( title='', xaxis = list(title = 'Years'), yaxis =list(title= 'Number of homicide cases') )
    
  })
  
  ##################################################################################################################
  
  output$bystate=renderPlotly({
    ss= homicide %>%
      group_by(State) %>%
      summarise(numbercases=n()) %>%
      arrange(desc(numbercases)) %>%
      collect
    
    plot_ly(ss, x = ~State, y = ~ss$numbercases,  type = 'bar' ,
            marker = list(color = "darkorchid"
                                    
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "State"),
             yaxis = list(title = "Number of homicide"))
    
  })
  
  
  output$byrelation=renderPlotly({
    ss= homicide %>%
      group_by(Relationship) %>%
      summarise(numbercases=n()) %>%
      arrange(desc(numbercases)) %>%
      collect
    
    plot_ly(ss, x = ~Relationship, y = ~ss$numbercases,  type = 'bar' ,
            marker = list(color = "darkorchid"
                                    
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "Relationships"),
             yaxis = list(title = "Number of homicide cases"))
    
    
  })
  
  output$byweapon=renderPlotly({
    ss= homicide %>%
      group_by(Weapon) %>%
      summarise(numbercases=n()) %>%
      arrange(desc(numbercases)) %>%
      collect
    
    plot_ly(ss, x = ~numbercases, y = ~Weapon,  type = 'bar'  , orientation = 'h',
            marker = list(color = "darkorchid"  ) ) %>%
      
      layout(title = "", autosize = T,
             xaxis = list(title = "By weapon"),
             yaxis = list(title = "Number of homicide cases"))
  })
  
  output$byrecord=renderPlotly({
    ss= homicide %>%
      group_by(Record_Source) %>%
      summarise(numbercases=n()) %>%
      arrange(desc(numbercases)) %>%
      collect
    
    plot_ly(ss, labels = ~Record_Source, values = ~numbercases,  type = 'pie' ,
            
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste( Record_Source ,numbercases ),
            
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Record source"),
             yaxis = list(title = "Number of homicide cases"))
  })
  
  
  
  output$bycrimetype=renderPlotly({
    ss= homicide %>%
      group_by(Crime_Type) %>%
      summarise(numbercases=n()) %>%
      arrange(desc(numbercases)) %>%
      collect
    
    plot_ly(ss, x = ~Crime_Type, y = ~ss$numbercases,  type = 'bar' ,
            marker = list(color = "darkorchid"
                          
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "Crime type"),
             yaxis = list(title = "Number of homicide"))
    
  })
  
  output$byperpetsex=renderPlotly({
    ss= homicide %>%
      group_by(Perpetrator_Sex) %>%
      summarise(numbercases=n()) %>%
      arrange(desc(numbercases)) %>%
      collect
    
    plot_ly(ss, labels = ~Perpetrator_Sex, values = ~numbercases,  type = 'pie' ,
            
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste( Perpetrator_Sex ,numbercases ),
            
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Perpetrator sex"),
             yaxis = list(title = "Number of homicide cases"))
    
  })
  
  output$byvictimsex=renderPlotly({
    ss= homicide %>%
      group_by(Victim_Sex) %>%
      summarise(numbercases=n()) %>%
      arrange(desc(numbercases)) %>%
      collect
    
    plot_ly(ss, labels = ~Victim_Sex, values = ~numbercases,  type = 'pie' ,
            
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste( Victim_Sex ,numbercases ),
            
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Victim sex"),
             yaxis = list(title = "Number of homicide cases"))
    
  })

  ##########################################################################################################
  output$yearbystate= renderPlotly({
    
    
    gg=homicide%>%
      group_by(State) %>%
      summarise(nomb=n()) %>%
      top_n(15) %>%
      collect
    
    ll=  homicide%>% 
      group_by( State, Year) %>%
      summarise(nyears=n())%>%
      filter(State %in% gg$State) %>%
      collect
    
    plot_ly(ll, x = ~Year, y = ~nyears , color=~State) %>%  
      add_trace( mode='lines+markers')
    
    
  })
  
  
  
  output$yearbyrelation= renderPlotly({
    ll=  homicide %>% 
      group_by( Relationship, Year) %>%
      summarise(nyears=n())%>%
      top_n(8) %>%
      collect
    
    
    plot_ly(ll, x = ~Year, y = ~nyears , color=~Relationship) %>%  
      add_trace( mode='lines+markers')
    
    
  })
  
  
  output$yearbyweapon= renderPlotly({
    ll=  homicide %>% 
      group_by( Weapon, Year) %>%
      summarise(nyears=n())%>%
      collect
    
    
    plot_ly(ll, x = ~Year, y = ~nyears , color=~Weapon) %>%  
      add_trace( mode='lines+markers')
    
    
  })
  
  
  output$victimage= renderPlotly({
    ll=  homicide%>% 
      group_by(Victim_Age) %>%
      summarise(nyears=n())%>%
      collect
    
    
    plot_ly(ll, x = ~Victim_Age, y = ~nyears, type= 'scatter', mode='lines' ,
            marker = list(color = c('rgba(222,45,38,0.8)'
                                    
            ))) %>%  
      
      layout( title='', xaxis = list(title = 'Victim Age'), yaxis =list(title= 'Number of homicide cases') )
    
  })
  
  output$perpetage= renderPlotly({
    ll=  homicide%>% 
      group_by(Perpetrator_Age) %>%
      summarise(nyears=n())%>%
      collect
    
    
    plot_ly(ll, x = ~Perpetrator_Age, y = ~nyears, type= 'scatter', mode='lines' ,
            marker = list(color = c('rgba(222,45,38,0.8)'
                                    
            ))) %>%  
      
      layout( title='', xaxis = list(title = 'Perpatrator Age'), yaxis =list(title= 'Number of homicide cases') )
    
  })
  
#####################################################################################
  
  
  
  output$femaleweapon=renderPlotly({
    ss= homicide %>%
      filter(Perpetrator_Sex=="Female") %>%
      group_by(Weapon) %>%
      summarise(numbercases=n()) %>%
      arrange(desc(numbercases)) %>%
      collect
    
    plot_ly(ss, labels = ~Weapon, values = ~numbercases,  type = 'pie' ,
            
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste( Weapon ,numbercases ),
            
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Weapons used by female perpetrator "),
             yaxis = list(title = "Number of homicide cases"))
  })
  
  
  output$maleweapon=renderPlotly({
    ss= homicide %>%
      filter(Perpetrator_Sex=="Male") %>%
      group_by(Weapon) %>%
      summarise(numbercases=n()) %>%
      arrange(desc(numbercases)) %>%
      collect
    
    plot_ly(ss, labels = ~Weapon, values = ~numbercases,  type = 'pie' ,
            
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste( Weapon ,numbercases ),
            
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)), showlegend = TRUE ) %>%
      
      
      layout(title = "", autosize = T,
             xaxis = list(title = "Weapons used by male perpetrator "),
             yaxis = list(title = "Number of homicide cases"))
    
  })
  
  
  output$femalerelation=renderPlotly({
  
    ss= homicide %>%
      filter(Perpetrator_Sex=="Female") %>%
      group_by(Relationship) %>%
      summarise(numbercases=n()) %>%
      top_n(15) %>%
      collect
    
    plot_ly(ss, x = ~Relationship, y = ~ss$numbercases,  type = 'bar' ,
            marker = list(color = "darkorchid"
                          
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "Relationships for female perpetrators"),
             yaxis = list(title = "Number of homicide cases"))
    
    
  })
  
  output$malerelation=renderPlotly({
    ss= homicide %>%
      filter(Perpetrator_Sex=="Male") %>%
      group_by(Relationship) %>%
      summarise(numbercases=n()) %>%
      top_n(15) %>%
      collect
    
    plot_ly(ss, x = ~Relationship, y = ~ss$numbercases,  type = 'bar' ,
            marker = list(color = "darkorchid"
                          
            )) %>%
      layout(title = "", autosize = T,
             xaxis = list(title = "Relationships for male perpetrators"),
             yaxis = list(title = "Number of homicide cases"))
    
    
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

