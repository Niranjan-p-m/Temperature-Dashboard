# load the required packages
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(leaflet)

data <- read.csv('temperature.csv',stringsAsFactors = F,header=T)
df<-select(data, -State)
data$AvgTemperature[data$AvgTemperature == -99] <- NA
tapply(data$AvgTemperature, data$City, median, na.rm=TRUE)
df$Month <-factor(df$Month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

agg<- df %>% group_by(City,Year) %>% summarise_at(vars(AvgTemperature), list(name = mean)) 
aa <- agg %>% filter(City=="Auckland")
Year <- aa$Year
name <- aa$name
aaa <- data.frame(Year,name)
avg <- agg %>% group_by(City) %>% summarise(value=mean(name))



#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Temperature Dashboard")  

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
    selectInput(
        inputId = "city",
        label = "City:", 
        choices = list("Auckland"=1,"Brisbane"=2,"Canberra"=3,"Melbourne"=4,"Perth"=5,"Sydney"=6), 
        selectize = FALSE),
    sidebarMenu(
        selectInput(
            inputId = "year",
            label = "Year:", 
            choices = list("1995"=1995,"2000"=2000,"2006"=2006,"2010"=2010,"2015"=2015,"2019"=2019), 
            selectize = FALSE)
    )
)


frow1 <- fluidRow(
    splitLayout(
        valueBoxOutput("value1",width = 10),
        valueBoxOutput("value2",width = 10),
        valueBoxOutput("city1",width = 10),
        valueBoxOutput("city",width = 10)
    )
)


frow3 <- fluidRow(
    column(3, valueBoxOutput("Display",width = 30))
)


frow2 <- fluidRow(
    splitLayout(
        box(title = "Overall Average Temperature of Cities", plotlyOutput("line"))
    )
    ,box(
        title = "Yearly Temperature of City",plotlyOutput("bar")
    )
    ,box(
        title = "Average Temperature of City across Years"
        ,plotlyOutput("bar1")
    ),leafletOutput("map",width = 600) 
) 

# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow2, frow3)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')

# create the server functions for the dashboard  
server <- function(input, output,session) {
    
    maxtemp <- df %>% summarise(AvgTemperature = max(AvgTemperature))
    #maxtemp <- round((maxtemp-32)/1.8,2)
    mintemp <- df %>% summarise(AvgTemperature = min(AvgTemperature))
    #mintemp <- round((mintemp-32)/1.8,2)
    
    a <- df %>% group_by(df$City) %>% summarise(AvgTemperature=max(AvgTemperature))
    b <- df %>% group_by(df$City) %>% summarise(AvgTemperature=min(AvgTemperature))
    c <- df %>% group_by(df$Month) %>% summarise(AvgTemperature=min(AvgTemperature))
    d <- df %>% group_by(df$City,df$Month) %>% summarise(AvgTemperature=max(AvgTemperature))
    
    df_Auckland <- subset(df, df$City == "Auckland")
    df_brisbane <- subset(df, df$City == "Brisbane")
    df_Canberra <- subset(df, df$City == "Canberra")
    df_Melbourne <- subset(df, df$City == "Melbourne")
    df_Perth <- subset(df, df$City == "Perth")
    df_Sydney <- subset(df, df$City == "Sydney")
    
    output$value1 <- renderValueBox({
        valueBox(
            paste0(mintemp), "Avg Min Temp of Aus region in Fahreinheit",
            color = "blue")
    })
    
    output$city <- renderValueBox({
        
        if(input$city == 1){
            maxcity <- a[1,2]
            tt <- "Auckland"
        }
        else if(input$city ==2){
            maxcity <- a[2,2]
            tt <- "Brisbane"
        }else if(input$city ==3){
            maxcity <- a[3,2]
            tt <- "Canberra"
        }else if(input$city ==4){
            maxcity <- a[4,2]
            tt <- "Melbourne"
        }else if(input$city ==5){
            maxcity <- a[5,2]
            tt <- "Perth"
        }else if(input$city ==6){
            maxcity <- a[6,2]
            tt <- "Sydney"
        }
        valueBox(paste0(tt,":",maxcity),"Avg Max Temp in Fahrenheit", color = "red")
    })
    
    
    output$city1 <- renderValueBox({
        
        if(input$city == 1){
            mincity <- b[1,2]
            tt <- "Auckland"
        }
        else if(input$city ==2){
            mincity <- b[2,2]
            tt <- "Brisbane"
        }else if(input$city ==3){
            mincity <- b[3,2]
            tt <- "Canberra"
        }else if(input$city ==4){
            mincity <- b[4,2]
            tt <- "Melbourne"
            
        }else if(input$city ==5){
            mincity <- b[5,2]
            tt <- "Perth"
        }else if(input$city ==6){
            mincity <- b[6,2]
            tt <- "Sydney"
        }
        valueBox(paste0(tt,":",mincity),"Avg Min Temp in Fahrenheit", color = "blue")
    })
    
    
    output$value2 <- renderValueBox({
        valueBox(
            paste0(maxtemp), "Avg Max Temp of Aus region in Fahrenheit", 
            color = "red")
    })
    
    
    
    output$Display <- renderValueBox({
        valueBox(paste0("Alert"),"Average Temperature increased in the year 2020", color = "red",icon("arrow-circle-up", lib = "glyphicon"))
        })
    
    output$line <- renderPlotly({
        
        p1 <- ggplot(data = avg, aes(x=City,y=value))
        p1+geom_bar(stat = "identity",colour="white",fill="chocolate")+labs(title = "From years 1995 to 2020 ")
    })
    
    
    
    output$bar <- renderPlotly({
        if(input$city == 1){
            plotcity <- df_Auckland
            tt <- "Auckland"
        }
        else if(input$city ==2){
            plotcity <- df_brisbane
            tt <- "Brisbane"
            
        }else if(input$city ==3){
            plotcity <- df_Canberra
            tt <- "Canberra"
            
        }else if(input$city ==4){
            plotcity <- df_Melbourne
            tt <- "Melbourne"
            
        }else if(input$city ==5){
            plotcity <- df_Perth
            tt <- "Perth"
            
        }else if(input$city ==6){
            plotcity <- df_Sydney
            tt <- "Sydney"
        }
        print(tt)
        aa <- agg %>% filter(City==tt)
        Year <- aa$Year
        name <- aa$name
        cityy <- data.frame(Year,name)
        
        ggplot(data = cityy,aes(x=cityy$Year, y=cityy$name)) + 
            geom_line() + ylab("Temperature") + 
            xlab("Years") + ggtitle(paste("Temperature of :",tt)) + labs(fill = "Region")
    })
    
    output$map <- renderLeaflet({
        
        df_Auckland <- subset(df, df$City == "Auckland")
        df_brisbane <- subset(df, df$City == "Brisbane")
        df_Canberra <- subset(df, df$City == "Canberra")
        df_Melbourne <- subset(df, df$City == "Melbourne")
        df_Perth <- subset(df, df$City == "Perth")
        df_Sydney <- subset(df, df$City == "Sydney")
       
         df_Auckland %>% group_by(df_Auckland$Year) %>% summarise(AvgTemperature=mean(AvgTemperature))
        df_brisbane %>% group_by(df_brisbane$Year) %>% summarise(AvgTemperature=mean(AvgTemperature))
        df_Canberra %>% group_by(df_Canberra$Year) %>% summarise(AvgTemperature=mean(AvgTemperature))
        df_Melbourne %>% group_by(df_Melbourne$Year) %>% summarise(AvgTemperature=mean(AvgTemperature))
        df_Perth %>% group_by(df_Perth$Year) %>% summarise(AvgTemperature=mean(AvgTemperature))
        df_Sydney %>% group_by(df_Sydney$Year) %>% summarise(AvgTemperature=mean(AvgTemperature))
        
        if(input$city == 1){
            long <- 174.768
            latt <- -36.84
            content <- paste("average Temperature Increased by 0.9 Fahrenheit in 2 decades")
        }
        else if(input$city ==2){
            long <- 153.02
            latt <- -27.46
            content <- paste("Average Temperature Increased by 2.1 Fahrenheit in 2 decades")
            
        }else if(input$city ==3){
            long <- 149.13
            latt <- -35.28
            content <- paste("Average Temperature Increased by 2.4 Fahrenheit in 2 decades")
            
        }else if(input$city ==4){
            long <- 144.768
            latt <- -37.81
            content <- paste("Average Temperature Increased by 1.7 Fahrenheit in 2 decades")
            
        }else if(input$city ==5){
            long <- 115.86
            latt <- -31.95
            content <- paste("Average Temperature Increased by 1.1 Fahrenheit in 2 decades")
            
        }else if(input$city ==6){
            long <- 151.20
            latt <- -33.86
            content <- paste("Average Temperature Increased by 2.9 Fahrenheit in 2 decades")
        }
        
        
        
        leaflet() %>%
            addTiles() %>% setView(lng=133.77, lat=-25.27, zoom = 4) %>% addPopups(long, latt, content,
                      options = popupOptions(closeButton = FALSE)
            )
    })
    

              
    
    
    
    output$bar1 <- renderPlotly({
        
        if(input$city == 1){
            plotcity <- df_Auckland
            tt <- "Auckland"
        }
        else if(input$city ==2){
            plotcity <- df_brisbane
            tt <- "Brisbane"
            
        }else if(input$city ==3){
            plotcity <- df_Canberra
            tt <- "Canberra"
            
        }else if(input$city ==4){
            plotcity <- df_Melbourne
            tt <- "Melbourne"
            
        }else if(input$city ==5){
            plotcity <- df_Perth
            tt <- "Perth"
            
        }else if(input$city ==6){
            plotcity <- df_Sydney
            tt <- "Sydney"
        }
        
        if(input$year == 1995){
            plotyear <- df %>% filter((City == tt) & (Year == input$year))   
            
        }
        else if(input$year ==2000){
            plotyear <- df %>% filter((City == tt) & (Year == input$year))
            
            
        }else if(input$year ==2006){
            plotyear <- df %>% filter((City == tt) & (Year == input$year))
            
            
        }else if(input$year ==2010){
            plotyear <- df %>% filter((City == tt) & (Year == input$year))
            
            
        }else if(input$year ==2015){
            plotyear <- df %>% filter((City == tt) & (Year == input$year))
            
            
        }else if(input$year ==2019){
            plotyear <- df %>% filter((City == tt) & (Year == input$year))
            
        }
        
        cc <-paste(" Temperature of:",tt)
        yy <- paste(" In Year:",input$year)
        
        ggplot(data = plotyear,aes(x=plotyear$Month, y=plotyear$AvgTemperature)) + 
            geom_bar(position = "dodge", stat = "identity") + ylab("Temperature") + 
            xlab("Years") + ggtitle(paste0(cc,yy)) + labs(fill = "Region")
 })
    
    
    
}

shinyApp(ui=ui, serve=server)






