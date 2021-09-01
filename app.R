library(readxl)
library(shiny)
library(reshape)
library(shinydashboard)
library(cluster)
library(DT)
library(ggplot2)
library(gapminder)
library(pheatmap)
library(stringr)
library(dashboardthemes)
library(leaflet)
library(sp)
rm(list=ls())

Sys.setlocale("LC_ALL","Chinese")

# all points
dict <- c("海门湾桥闸" = "haimenwanqiaozha", "和平桥" = "hepingqiao", "青洋山桥" = "qingyangshanqiao"
          , "北港河闸" = "beiganghezha", "北港水" = "beigangshui", "成田大寮" = "chengtiandaliao"
          , "东北支流" = "dongbeizhiliu", "港洲桥" = "gangzhouqiao", "官田水" = "guangtianshui"
          , "护城河闸" = "huchenghezha", "华侨学校" = "huaqiaoxuexiao", "井仔湾闸" = "jingzaiwanzha"
          , "流仙学校" = "liuxianxuexiao", "万兴桥" = "wanxingqiao", "五福桥" = "wufuqiao"
          , "西埔桥闸" = "xipuqiaozha", "峡山大溪" = "shanshandaxi", "仙马闸" = "xianmazha"
          , "新坛港" = "xintanggang", "新溪西村" = "xinxixicun", "瑶池港" = "yaochigang", "云陇" = "yunlong")

# get data
Sys.setlocale("LC_ALL","Chinese")
path <- getwd()
#  2010-2016 data
files1_ <- paste(path,"/","data/2010-2016", sep = "")
files1 <- list.files(files1_)
for(i in 1:length(files1)){
  data_name <- paste(path, "/data/2010-2016/", files1[i], sep = "")
  data_temp <- read_excel(data_name,sheet=1,na="NA")
  data_temp <- data_temp[, -1]
  assign(paste("data_", dict[str_replace(files1[i],"2010-2016年.xlsx", "")], sep = ""), data_temp)
}
# one year data
files2_ <- paste(path,"/","data/year", sep = "")
files2 <- list.files(files2_)
for(i in 1:length(files2)){
  data_name <- paste(path, "/data/year/", files2[i], sep = "")
  data_temp <- read.csv(data_name,skip = 0, header = FALSE, sep=",")
  data_temp <- data_temp[-1:-5, ]
  colnames(data_temp) <- data_temp[1,]
  data_temp <- data_temp[-1, ]
  assign(paste("data_year_", str_replace(str_replace(files2[i],"汕头练江水站",""),"年数据汇总（小时值）.csv", ""), sep = ""), data_temp)
}
points <- read_excel("points.xlsx",sheet=1,na="NA")
logo <- shinyDashboardLogoDIY(
  
  boldText = "练江"
  ,mainText = "可视化平台"
  ,textSize = 16
  ,badgeText = "v1.0"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#40E0D0"
  ,badgeBorderRadius = 3
  
) # theme
imagefile <- paste(path,"/temp.png", sep = "")  ## 设置文件名
theme_blue_gradient <- shinyDashboardThemeDIY(
  
  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  ### header
  ,logoBackColor = "rgb(23,103,124)"
  
  ,headerButtonBackColor = "rgb(238,238,238)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgb(210,210,210)"
  ,headerButtonIconColorHover = "rgb(0,0,255)"
  
  ,headerBackColor = "rgb(238,238,238)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"
  
  ### sidebar
  ,sidebarBackColor = cssGradientThreeColors(
    direction = "down"
    ,colorStart = "rgb(20,97,117)"
    ,colorMiddle = "rgb(56,161,187)"
    ,colorEnd = "rgb(3,22,56)"
    ,colorStartPos = 0
    ,colorMiddlePos = 50
    ,colorEndPos = 100
  )
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(255,255,255)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = cssGradientThreeColors(
    direction = "right"
    ,colorStart = "rgba(44,222,235,1)"
    ,colorMiddle = "rgba(44,222,235,1)"
    ,colorEnd = "rgba(0,255,213,1)"
    ,colorStartPos = 0
    ,colorMiddlePos = 30
    ,colorEndPos = 100
  )
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgba(44,222,235,1)"
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)
link1 <- paste( path,"/","lianjiang_map/try.html", sep = "")
ui <- dashboardPage(
  
  dashboardHeader(title = logo,
                  dropdownMenu(
                    type = "tasks", badgeStatus = "danger",
                    notificationItem(icon = icon("battery-full"), status = "danger",
                                     text = "练江可视化平台", 
                    ),
                    notificationItem(icon = icon("user"), status = "info",
                                     text =  "作者：林澈"
                    ),
                    
                    notificationItem(icon = icon("envelope-square"), status = "danger",
                                     text = "邮箱：19clin1@stu.edu.cn", href="mailto:19clin1@stu.edu.cn"
                    ),
                    notificationItem(icon = icon("clock"),
                                     status = "success", 
                                     text = "当前时间", href = "http://www.daojishiqi.com/bjtime.asp"
                                     
                    ),
                    notificationItem(icon = icon("calendar-week"),
                                     status = "danger", text = "2021年6月"
                    )
                  )
  ),
  
  dashboardSidebar(
    
    
    sidebarMenu(
      menuItem("练江", tabName = "lianjiang", icon = icon("line-chart"),
               badgeLabel = "nation", badgeColor = "red",selected=T),
      menuItem("数据集", tabName = "data", icon = icon("table"),startExpanded=F,
               menuSubItem("国家数据",tabName = "national_data", icon = icon("blank")),
               
               menuItem("项目介绍", icon = icon("book"),startExpanded=T,
                        menuSubItem("数据集介绍",tabName = "main_data", icon = icon("blank")),
                        menuSubItem("CO2数据信息",tabName = "national_data_com", icon = icon("blank")),      
                        menuSubItem("研究背景",tabName = "item_background", icon = icon("blank"))),
               menuItem("作者介绍", tabName = "author", icon = icon("user-edit"),
                        badgeLabel = "author", badgeColor = "light-blue"),br(),hr(),
               menuItem("小建议", tabName = "tips", icon = icon("smile"))
      )
    )
  ),
  
  dashboardBody(
    theme_blue_gradient,
    tabItems(
      tabItem(tabName = "lianjiang",fluidPage(
        tabBox(title = tagList(icon("gear"), "2018-2020")
               ,width = 5,side="right",
               tabPanel(title = "折线图",imageOutput("line1")),
               tabPanel(title = "散点图",imageOutput("point1")),
               tabPanel(title = "柱状图",imageOutput("bar1")),
               selectInput('choose_point', '选择站点', c("海门湾桥闸", "青洋山桥")),
               dateRangeInput("dates", "日期选择")
        ),
        box(title = "练江地图",width = 4,selectInput('selectmap', '选择地图样式', 
                                                c("Esri.WorldImagery", "Esri.NatGeoWorldMap",
                                                  "NASAGIBS.ViirsEarthAtNight2012")),
            leafletOutput("mapp", width = "440px", height = "300px")
        )

      )
      )
      
    )# tab
  )
)


server <- function(input, output) {
  output$mapp <- renderLeaflet({
    mapstyle <- "Esri.WorldImagery"
    mapstyle = input$selectmap
    leaflet(points)%>%addTiles()%>% addMarkers(popup=~mc)%>%addProviderTiles(mapstyle)
    
  })
  
}


shinyApp(ui, server)


