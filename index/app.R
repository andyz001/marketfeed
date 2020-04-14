library(shiny)
library(lubridate)
library(dplyr)
library(RODBC)
library(ggplot2)
library(reshape2)
library(RPostgreSQL)

data <- read.csv('data.csv')

ui <- fluidPage(
  # title
  p("Midas系统菜单：",
    tags$a(href="/", "主页"),
    " | ",
    tags$a(href="/landindex", "地产指数"),
    " | ",
    tags$a(href="/dijia", "地产估值"),
    " | ",
    tags$a(href="/fangjia", "房产估值"),
    " | ",
    tags$a(href="/shebei", "设备估值"),
    " | ",
    tags$a(href="/ceya", "测压分析"),
    " | ",
    tags$a(href="/zichan", "债权评估"),
    " "),  
  titlePanel("地产指数分析报告"),
  # sidebar
  fluidRow(
    # textOutput("msg", "正在加载模型。。。"),
    column(4, 
      selectInput("province", "请选择土地所在省或者直辖市：",
                  choices = c("安徽省",	"澳门特别行政区",	"北京市",	"福建省",	"甘肃省",	"广东省",	
                              "广西壮族自治区",	"贵州省",	"海南省",	"河北省",	"河南省",	"黑龙江",	
                              "湖北省",	"湖南省",	"吉林省",	"江苏省",	"江西省",	"辽宁省",	"内蒙古自治区",
                              "宁夏回族自治区",	"青海省",	"山东省",	"山西省",	"陕西省",	"上海市",	
                              "四川省",	"台湾省",	"天津市",	"西藏自治区",	"香港特别行政区",	
                              "新疆维吾尔自治区",	"云南省",	"浙江省",	"重庆市"),
                  selected="山东省")),
    column(4,     
      selectInput("city", "请选择土地所在市：",
                  choices = c(""))),
      
      #uiOutput("landIndex"),
      
      #checkboxInput("summary", "显示概要", TRUE)
    column(8,
    
    # 展示一个HTML表格

    tabsetPanel(
        tabPanel("工业", 
                 textOutput("textI"),
                 plotOutput("plotI"),
                 dataTableOutput('tableI')
                 ),
        tabPanel("住宅", 
                 textOutput("textR"),
                 plotOutput("plotR"),
                 dataTableOutput('tableR')
                 ),
        tabPanel("商业", 
                 textOutput("textC"),
                 plotOutput("plotC"),
                 dataTableOutput('tableC')
                 )
      )
    )
  )
)

server <- function(input, output) {

getAreaList<-function(parent_id)
{
  areaList <- getQryResult("Market", paste("select \"ID\", \"Name\",\"ParentId\",\"LevelType\" from dbo.\"mapAdminArea\" where \"ParentId\" =",parent_id))
  return(areaList)
}

getAreaListAll<-function()
{
  areaList <- getQryResult("Market", "select \"ID\", \"Name\",\"ParentId\",\"LevelType\" from dbo.\"mapAdminArea\"")
  return(areaList)
}

refreshIndex<-function(output, AreaCd)
{
  #AreaCd <- 0
  # refresh land index
  qry <- paste("select \"LandType\", \"YearMonth\", exp(\"M_5\") as \"PriceIndex\",
                     exp(\"M_1\") as \"Low4\",exp(\"M_2\") as \"Low3\",exp(\"M_3\") as \"Low2\",exp(\"M_4\") as \"Low1\",
                     exp(\"M_9\") as \"High4\",exp(\"M_8\") as \"High3\", exp(\"M_7\") as \"High2\",exp(\"M_6\") as \"High1\"
                     from dbo.land_index 
                     where \"AreaCd\"=",AreaCd, " ")
  landIdx <- getQryResult("Market",qry)  

  if (nrow(landIdx) < 1) {
    output$textI <- renderUI({("没找到相应地区的数据。")})
    output$textC <- renderUI({("没找到相应地区的数据。")})
    output$textR <- renderUI({("没找到相应地区的数据。")})
    return()
  }
  
  l_fields <- c("YearMonth","Low4","Low3","Low2","Low1","PriceIndex",
                "High1","High2","High3","High4")
  
  # string to numeric
  landIdx$Low4 <- as.numeric(as.character(landIdx$Low4))
  landIdx$Low3 <- as.numeric(as.character(landIdx$Low3))
  landIdx$Low2 <- as.numeric(as.character(landIdx$Low2))
  landIdx$Low1 <- as.numeric(as.character(landIdx$Low1))
  landIdx$PriceIndex <- as.numeric(as.character(landIdx$PriceIndex))
  landIdx$High1 <- as.numeric(as.character(landIdx$High1))
  landIdx$High2 <- as.numeric(as.character(landIdx$High2))
  landIdx$High3 <- as.numeric(as.character(landIdx$High3))
  landIdx$High4 <- as.numeric(as.character(landIdx$High4))
  
  landIdx$YearMonth <- as.numeric(landIdx$YearMonth)
  
    #landIdxI = reactive ({ subset(landIdx, LandType %in% c("I"))})
  #industry
  output$plotI <- renderPlot({ 
    ggplot(subset(landIdx, LandType %in% c("I")),
          aes(x=YearMonth,
          y=PriceIndex,
          color=LandType))+
      geom_point() + geom_smooth()})
  
  output$tableI <- renderDataTable({format(subset(landIdx, LandType %in% c("I"))[,l_fields],digits = 6)},
                                   options = list(searching = FALSE,  smartsearch=FALSE, bInfo = FALSE,
                                     autoWidth = TRUE,
                                     columnDefs = list(list(width = '40%', targets = "_all"))
                                   ))
  #output$tableI <- renderDataTable(landIdxI)
  
  #Commercial
  output$plotC <- renderPlot({
    ggplot(subset(landIdx, LandType %in% c("C")),
           aes(x=YearMonth,
               y=PriceIndex,
               color=LandType))+
      geom_point() + geom_smooth()
  })

  output$tableC <- renderDataTable({format(subset(landIdx, LandType %in% c("C"))[,l_fields],digits = 6)},
    options = list(searching = FALSE,  smartsearch=FALSE, bInfo = FALSE,
    autoWidth = TRUE,
    columnDefs = list(list(width = '40%', targets = "_all"))
  ))

    #Residential
  output$plotR <- renderPlot({
    ggplot(subset(landIdx, LandType %in% c("R")),
           aes(x=YearMonth,
               y=PriceIndex,
               color=LandType))+
      geom_point() + geom_smooth()
  })

  output$tableR <- renderDataTable({format(subset(landIdx, LandType %in% c("R"))[,l_fields],digits = 6)},
      options = list(searching = FALSE,  smartsearch=FALSE, bInfo = FALSE,
      autoWidth = TRUE,
      columnDefs = list(list(width = '40%', targets = "_all"))
      ))
  
}

#
prev_prov <- ""
prev_city <- ""

function(input, output, session) {
  
  observe({
    # generate province list
    if ( prev_prov == "") {
      areas <- getAreaListAll()
    }

    # province changed
    if ( prev_prov != input$province) {
      
      # update city list
      parent_id <- areas[which(areas$Name == input$province),]["ID"]
      l_city <- c(subset(areas, ParentId %in% c(parent_id))["Name"])
      updateSelectInput(session, "city", 
                        choices = l_city, 
                        selected=0)
      
      #initial Index plot
      refreshIndex(output,parent_id)
      
      prev_prov <- input$province
      updateSelectInput(session, "province", selected = prev_prov)
    }
    
    # city changed
    if ( prev_city != input$city) {
      
      # update city list
      parent_id <- areas[which(areas$Name == input$city),]["ID"]
      l_district <- c(subset(areas, ParentId %in% c(parent_id))["Name"])
      updateSelectInput(session, "district", 
                        choices = l_district, 
                        selected=0)
      
      #initial Index plot
      refreshIndex(output,parent_id)
      
      prev_city <- input$city
      updateSelectInput(session, "city", selected = prev_city)
    }


  })
}
}

# Run the application 
shinyApp(ui = ui, server = server)
