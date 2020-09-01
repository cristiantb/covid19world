library(shiny)
library(shinydashboard)
library(dplyr)
library(plotly)
library(shinyFeedback)
library(tidyr)
library(shinyWidgets)
library(xlsx)
library(tibble)
library(multcomp)
library(sjPlot)
library(sjstats)
library(EpiEstim)
library(kableExtra)
library(shinycssloaders)
library(vroom)
library(shinyFeedback)

#No accents function:
no_accents<-function(out)
{
  unwanted_array = list('S'='S', 's'='s', 'Z'='Z', 'z'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E', 'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',  'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c','è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o','ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y' )
  for(i in seq_along(unwanted_array)){
    out <- gsub(names(unwanted_array)[i],unwanted_array[i],out)}
  
  return(out)
}

#------

#Multi language:
df <- data.frame(
  val = c("English","Castellano","Català")
)
#Language images:
df$img = c(
  sprintf("<img src='https://upload.wikimedia.org/wikipedia/commons/thumb/1/13/United-kingdom_flag_icon_round.svg/512px-United-kingdom_flag_icon_round.svg.png' width=30px><div class='jhr'>%s</div></img>", df$val[1],width="200%"),
  sprintf("<img src='https://images.vexels.com/media/users/3/164599/isolated/preview/ce858535b77f22068049aca2457e59ad-spain-flag-language-icon-circle-by-vexels.png' width=30px><div class='jhr'>%s</div></img>", df$val[2],width="200%"),
  sprintf("<img src='catalanflag.png' width=30px><div class='jhr'>%s</div></img>", df$val[3],width="200%")
)

Translate<-vroom("translate.csv",delim=";")

colnames(Translate)[1]<-"row"

Translate<-Translate%>%column_to_rownames("row")

lan<-colnames(Translate)

#Country selection:

load("country_code.Rda")

country_code<-rbind(country_code,data.frame(country="Andorra",code="ad"))
country_code<-country_code[order(country_code$country),]
country_code<-rbind(cbind(country="Global",code="gb"),country_code)

df2 <- data.frame(
  val = c("Global",country_code$country)
)
#Country images:
df2$img<-c(
  sprintf("<img src=countries/global.png width=40px><div class='jhr'>%s</div></img>",df2$val[1],width="200%"),
  sprintf(paste0("<img src=countries/",country_code$code,".png width=50px><div class='jhr'>%s</div></img>"),df2$val[-1],width="200%")
)

choices<-paste0(country_code$code)
names(choices)<-country_code$country

ui <-tagList(
  dashboardPage(title = "COVID-19 WORLD TRACKER",
                dashboardHeader(title = span("COVID-19 WORLD TRACKER",
                                             style = "color: #3644E4; font-weight: bold;"),
                                tags$li(class = "dropdown", 
                          tags$head(tags$style("
                                               .selectize-input{
                                               margin-top:10px;
                                               }")),
                                        selectizeInput("country",label=NULL,choices=choices,
                                                       options = list(
                                                         render = I(
                                                           "{option: function(item, escape) {
      return '<div><img src=\"' + item.value + '.png\" width = 20 /> &nbsp;' + escape(item.label) + '</div>'
      }
      }"))
                                        )),
                                tags$li(a(href = 'http://www.idibell.cat',
                                          img(src = 'logo.png',
                                              title = "IDIBELL", height = "30px",width="59.9px"),
                                          style = "padding-top:12px; padding-bottom:10px;"),
                                        class = "dropdown"),
                                tags$li(a(href = 'https://www.csic.es',
                                          img(src = 'logoA1.png',
                                              title = "CSIC", height = "27px",width="90px"),
                                          style = "padding-top:12px; padding-bottom:10px;"),
                                        class = "dropdown"),
                                tags$li(a(href = 'https://www.idaea.csic.es',
                                          img(src = 'logoA2.png',
                                              title = "IDAEA", height = "27px",width="135px"),
                                          style = "padding-top:10px; padding-bottom:10px;"),
                                        class = "dropdown"),
                                tags$li(class = "dropdown", 
                                        tags$head(tags$style("
                       .jhr{
                       display: inline;
                       vertical-align: middle;
                       padding-left: 10px;
                       }")),
                                        pickerInput("lan",label=NULL,choices=df$val,choicesOpt =list(content=df$img),width="fit",inline=T))
                ),
                dashboardSidebar(
                  tags$head(tags$style(HTML('
                              .logo {
                              background-color: #ECF0F5   !important;
                              }
                              .main-header .navbar .sidebar-toggle{
                                background-color: #3644E4;
                              }
                              .navbar {
                              background-color: #ECF0F5   !important;
                              }'))),
                  sidebarMenu(
                    menuItem(text=textOutput("menu_home"),tabName="home"),
                             menuItem(text=textOutput("menu_cas"),tabName="cas"),
                             menuItem(text=textOutput("menu_cas_inc"),tabName="cas_inc"),
                             menuItem(text=textOutput("menu_mort"),tabName="mort"),
                             menuItem(text=textOutput("menu_mort_inc"),tabName="mort_inc"),
                             menuItem(text=textOutput("menu_leta"),tabName="leta"
                             ),
                             menuItem(text=textOutput("menu_inf"),tabName="inf"
                             ), 
                             menuItem(text=textOutput("menu_r0"),tabName="r0"),
                    menuItem(text=textOutput("menu_met"), tabName = "method"),
                    menuItem(text=textOutput("menu_ab"), tabName = "about")
                  )
                ),
                dashboardBody(
                  tags$head(
                    tags$style(
                      "body{
    max-height: 1000px;
        }
    .small-box {height: 120px}
    .img-local {
        }
        .small-box .img-local {
        position: absolute;
        top: auto;
        bottom: 65px;
        right: 5px;
        z-index: 0;
        font-size: 70px;
        color: rgba(0, 0, 0, 0.15);
        }
        "
                    )
                  ),
                  tabItems( 
                    tabItem(tabName="home",
                            fluidRow(
                              column(
                                width=4,
                                valueBoxOutput("count_tc", width=13)
                              ),
                              column(
                                width=4,
                                valueBoxOutput("count_tm", width=13)
                              ),
                              column(
                                width=4,
                                valueBoxOutput("count_tr", width=13)
                              )),
                            tags$style(".topimg {
                            margin-left:-30px;
                            margin-right:-30px;
                            margin-top:-15px;
                            position: absolute;
                          }"),
                            div(class="topimg",img(src="Background8.jpg",width="100%"))
                    ),
                    tabItem(tabName="cas",
                            fluidRow(
                              box(
                                width = 12,
                                title=textOutput("title_cas"),
                                h3(textOutput("where0")),
                                tabsetPanel(type = "tabs",
                                            tabPanel(textOutput("menu_cas_plot"), plotlyOutput("plot0",height="320px")),
                                            tabPanel(textOutput("menu_cas_model"), htmlOutput("table_mcas"),htmlOutput("table_mcas_a")),
                                            tabPanel(textOutput("menu_cas_coef"),splitLayout(cellWidths = c("33%", "33%","33%"),plotlyOutput("plot_cas_beta2",height="500",width = "330"),
                                                                                             plotlyOutput("plot_cas_beta3",height="500",width = "330"),
                                                                                             plotlyOutput("plot_cas_beta4",height="500",width = "330"))),
                                            tabPanel(textOutput("menu_cas_error"),splitLayout(cellWidths = c("33%", "33%", "33%"),plotlyOutput("plot_cas_error1",height="500",width = "300"),
                                                                                              plotlyOutput("plot_cas_error2",height="500",width = "300"),
                                                                                              plotlyOutput("plot_cas_error3",height="500",width = "300")))
                                ))
                            )
                    ),
                    tabItem(tabName="cas_inc",
                            fluidRow(
                              box(
                                width = 12,
                                title=textOutput("title_cas_inc"),
                                h3(textOutput("where_inc")),
                                tabsetPanel(type = "tabs",
                                            tabPanel(textOutput("menu_inc_plot1"),plotlyOutput("plot_inc",height="320px")),
                                            tabPanel(textOutput("menu_inc_model1"), htmlOutput("table_mca_inc"),htmlOutput("table_mca_inc_a")),
                                            tabPanel(textOutput("menu_inc_error"),splitLayout(cellWidths = c("33%", "33%", "33%"),plotlyOutput("plot_inc_error1",height="500",width = "300"),
                                                                                              plotlyOutput("plot_inc_error2",height="500",width = "300"),
                                                                                              plotlyOutput("plot_inc_error3",height="500",width = "300")))
                                ))
                            )
                    ),    
                    tabItem(tabName="mort",
                            fluidRow(
                              box(
                                width = 12,
                                title=textOutput("title_mort"),
                                h3(textOutput("where2")),
                                #              plotlyOutput("plot2",height="320")
                                tabsetPanel(type = "tabs",
                                            tabPanel(textOutput("menu_mort_plot"), plotlyOutput("plot2",height="320px")),
                                            tabPanel(textOutput("menu_mort_model"), htmlOutput("table_mmort"),htmlOutput("table_mmort_a")),
                                            tabPanel(textOutput("menu_mort_coef"),splitLayout(cellWidths = c("33%", "33%","33%"),plotlyOutput("plot_mort_beta2",height="500",width = "330"),
                                                                                              plotlyOutput("plot_mort_beta3",height="500",width = "330"),
                                                                                              plotlyOutput("plot_mort_beta4",height="500",width = "330"))),
                                            tabPanel(textOutput("menu_mort_error"),splitLayout(cellWidths = c("33%", "33%", "33%"),plotlyOutput("plot_mort_error1",height="500",width = "300"),
                                                                                               plotlyOutput("plot_mort_error2",height="500",width = "300"),
                                                                                               plotlyOutput("plot_mort_error3",height="500",width = "300")))
                                ))
                            )
                    ),
                    tabItem(tabName="mort_inc",
                            fluidRow(
                              box(
                                width = 12,
                                title=textOutput("title_mort_inc"),
                                h3(textOutput("where_mort_inc")),
                                tabsetPanel(type = "tabs",
                                            tabPanel(textOutput("menu_mort_plot1"),plotlyOutput("plot_mort_inc",height="320px")),
                                            tabPanel(textOutput("menu_mort_model1"), htmlOutput("table_mmort_inc"),htmlOutput("table_mmort_inc_a")),
                                            tabPanel(textOutput("menu_mort_inc_error"),splitLayout(cellWidths = c("33%", "33%", "33%"),plotlyOutput("plot_mort_inc_error1",height="500",width = "300"),
                                                                                                   plotlyOutput("plot_mort_inc_error2",height="500",width = "300"),
                                                                                                   plotlyOutput("plot_mort_inc_error3",height="500",width = "300")))
                                ))
                            )
                    ),  
                    tabItem(tabName="leta",
                            fluidRow(
                              box(
                                width = 12,
                                title=textOutput("title_casl"),
                                h3(textOutput("where_casl")),
                                plotlyOutput("plot_casl",height="320")
                              )
                            )
                    ),    
                    tabItem(tabName="inf",
                            fluidRow( 
                              box(
                                width = 12,
                                title=textOutput("title_casf"),
                                h3(textOutput("wheref")),
                                plotlyOutput("plotf2",height="400")                
                              ))
                    ),    
                    tabItem(tabName="r0",
                            fluidRow(
                              box(
                                width = 12,
                                title=textOutput("title_r0"),
                                h3(textOutput("wherer0")),
                                uiOutput("log_scale"),
                                plotlyOutput("plotr0",height="320")
                              )
                            )
                    ),
                    tabItem(tabName="method",
                            h2(textOutput("title_met")),
                            uiOutput("text_method")
                    ),
                    tabItem(tabName="about",
                            h3(textOutput("title_ab")),
                            htmlOutput("text_about")
                    )
                  )
                )
  ),
  tags$footer(
    tags$style(".copyleft {
  display: inline-block;
  transform: rotate(180deg);
}
"),
    HTML("COVID-19 WOLRD TRACKER<sup><span class='copyleft'/>&copy;</sup> &nbsp"),a(href = 'https://ubidi.shinyapps.io/covid19', 'https://ubidi.shinyapps.io/covid19'),align = "center", style = "
              position:absolute;
              bottom:-10;
              width:100%;
              height:50px;   /* Height of the footer */
              color: black;
              padding: 10px;
              background-color: white;
              z-index: 1000;")
)


server <- function(input, output,session) {
  
  #Download the data from: https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
  
  covid19 <- read.csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", na.strings = "", fileEncoding = "UTF-8-BOM")
  
  covid19<-covid19 %>% mutate(fecha=as.Date(with(covid19, paste(year, month, day,sep="-")), "%Y-%m-%d"),countriesAndTerritories=gsub("_"," ",countriesAndTerritories)) 
  
  #Calculate the numbers for the global of the world previous to the subset of countries:
  
  population_global<-sum(unique(covid19$popData2019),na.rm=T)
  
  covid19_global<-covid19 %>% group_by(fecha) %>% summarise(casosf=sum(cases),fallecidosf=sum(deaths)) %>% mutate(country="Global",idcountry="GB",population=population_global)
  
  #Subset by population size:
  
  covid19<-covid19 %>% subset(!is.na(popData2019) & (popData2019>500000 | countriesAndTerritories=="Andorra")) %>% dplyr::select(fecha,casosf=cases,fallecidosf=deaths,country=countriesAndTerritories,idcountry=geoId,population=popData2019)
  
  #Add the global:
  
  covid19<-rbind(covid19,covid19_global) %>% arrange(country,fecha) %>% mutate_if(is.factor,as.character) 
  
  #Some countries have zeros at the beginning, remove this cases:
  covid19<-covid19 %>% group_by(idcountry) %>% mutate(zeros=cumsum(casosf!=0 | fallecidosf!=0)) %>% subset(zeros>0) %>% dplyr::select(-zeros)
  
  #Some entries are negative, put them missing:
  
  covid19<-covid19 %>% mutate(casosf=ifelse(casosf<0,NA,casosf),fallecidosf=ifelse(fallecidosf<0,NA,fallecidosf))
  
  #Some in-between days are missing, we identify them and put to missing those days:
  
  covid19<-covid19 %>% group_by(idcountry) %>% mutate(dif=c(1,diff(fecha))) %>% ungroup()
  
  country_dif<-covid19 %>% subset(dif>1) %>% group_by(country) %>% summarise(idcountry=unique(idcountry),population=unique(population)) %>% as.data.frame()
  
  for(i in 1:nrow(country_dif)){
    scovid19<-covid19 %>% subset(country==country_dif[i,"country"])
    covid19_miss<-data.frame(fecha=seq(min(scovid19$fecha),max(scovid19$fecha),1),country=country_dif[i,"country"],idcountry=country_dif[i,"idcountry"],population=country_dif[i,"population"])
    scovid19<-merge(scovid19,covid19_miss,by=c("fecha","country","idcountry","population"),all=TRUE)
    covid19<-rbind(scovid19,covid19 %>% subset(country!=country_dif[i,"country"]))
  }
  
  covid19<-covid19%>% group_by(idcountry) %>% mutate(casos=cumsum(replace_na(casosf,0)),fallecidos=cumsum(replace_na(fallecidosf,0))) %>% ungroup() %>% dplyr::select(-dif)
  
  covid19<-covid19 %>% arrange(country,fecha)
  
  #List of countries with maximum cases below 50 
  
  country_lessc50<-covid19 %>% group_by(country) %>% summarise(max=max(casos)) %>% subset(max<50) 
  country_lessc50<-country_lessc50$country
  
  covid19<-covid19 %>% mutate(idcountry=tolower(idcountry))
  
  #Count boxes:
  source("override.R", local = TRUE) # override 'icon' and 'valueBox' functions
  
  output$count_tc <- renderValueBox({
    scovid19<-covid19 %>% subset(idcountry==input$country)
    x <- "virus.png"
    lab<-ifelse(input$country=="Global",Translate["tg_cas",input$lan],paste(Translate["t_cas",input$lan],      unique(covid19$country[covid19$idcountry==input$country])
    ))
    valueBox(tail(round(na.exclude(scovid19$casos*100000/scovid19$population),1),1),lab,
             icon=icon(list(src=x, width="50px"), lib="local"),
             color="aqua", width=NULL)
  })
  output$count_tm <- renderValueBox({
    scovid19<-covid19 %>% subset(idcountry==input$country)
    x <- "death.png"
    valueBox(tail(round(na.exclude(scovid19$fallecidos*100000/scovid19$population),1),1), ifelse(input$country=="Global",Translate["tg_mort",input$lan],paste(Translate["t_mort",input$lan],      unique(covid19$country[covid19$idcountry==input$country])
    )),
    icon=icon(list(src=x, width="30px"), lib="local"),
    color="red", width=NULL)
  })
  output$count_tr <- renderValueBox({
    scovid19<-covid19 %>% subset(idcountry==input$country)
    x <- "rate.png"
    valueBox(tail(round(na.exclude(scovid19$fallecidos*100/scovid19$casos),1),1), ifelse(input$country=="Global",Translate["tg_rate",input$lan],paste(Translate["t_rate",input$lan],      unique(covid19$country[covid19$idcountry==input$country])
    )),
    icon=icon(list(src=x, width="40px"), lib="local"),
    color="orange")
  })
  #Text multilingüe:
  
  output$menu_home<-renderText(
    Translate["menu_home",input$lan]
  )
  output$menu_comp<-renderText(
    Translate["menu_comp",input$lan]
  )
  output$menu_compc<-renderText(
    Translate["menu_compc",input$lan]
  )
  output$menu_compc2<-renderText(
    Translate["menu_compc2",input$lan]
  )
  output$menu_compm<-renderText(
    Translate["menu_compm",input$lan]
  )
  output$menu_compm2<-renderText(
    Translate["menu_compm2",input$lan]
  )
  output$menu_compl<-renderText(
    Translate["menu_compl",input$lan]
  )
  output$menu_compl2<-renderText(
    Translate["menu_compl2",input$lan]
  )
  output$menu_compr<-renderText(
    Translate["menu_compr",input$lan]
  )
  output$menu_cas<-renderText(
    Translate["menu_casi",input$lan]
  )
  output$menu_cas_plot<-renderText(
    Translate["plot",input$lan]
  )
  output$menu_cas_model<-renderText(
    Translate["expi",input$lan]
  )
  output$menu_cas_coef<-renderText(
    Translate["coef",input$lan]
  )
  output$menu_cas_error<-renderText(
    Translate["error",input$lan]
  )
  output$menu_cas_plot1<-renderText(
    Translate["plot",input$lan]
  )
  output$menu_cas_model1<-renderText(
    Translate["expi",input$lan]
  )
  output$menu_cas_coef1<-renderText(
    Translate["coef",input$lan]
  )
  output$menu_cas_inc<-renderText(
    Translate["menu_cas_inc",input$lan]
  )
  output$menu_inc_error<-renderText(
    Translate["error",input$lan]
  )
  output$menu_mort<-renderText(
    Translate["menu_mort",input$lan]
  )
  output$menu_mort_plot<-renderText(
    Translate["plot",input$lan]
  )
  output$menu_mort_model<-renderText(
    Translate["expi",input$lan]
  )
  output$menu_mort_coef<-renderText(
    Translate["coef",input$lan]
  )
  output$menu_mort_error<-renderText(
    Translate["error",input$lan]
  )
  output$menu_mort_inc<-renderText(
    Translate["menu_mort_inc",input$lan]
  )
  output$menu_inc_plot1<-renderText(
    Translate["plot",input$lan]
  )
  output$menu_inc_model1<-renderText(
    Translate["expi",input$lan]
  )
  output$menu_mort_inc_error<-renderText(
    Translate["error",input$lan]
  )
  output$menu_mort_plot1<-renderText(
    Translate["plot",input$lan]
  )
  output$menu_mort_model1<-renderText(
    Translate["expi",input$lan]
  )
  output$menu_casa<-renderText(
    Translate["menu_casi",input$lan]
  )
  output$menu_morta<-renderText(
    Translate["menu_morti",input$lan]
  )
  output$menu_leta<-renderText(
    Translate["menu_leta",input$lan]
  )
  output$menu_inf<-renderText(
    Translate["menu_inf",input$lan]
  )
  output$menu_met<-renderText(
    Translate["menu_met",input$lan]
  )
  output$menu_ab<-renderText(
    Translate["menu_ab",input$lan]
  )
  output$title_cas<-renderText(
    paste(Translate["title_cas",input$lan],tail(covid19$fecha[covid19$idcountry==input$country],1)+3)
  )
  output$title_cas_inc<-renderText(
    paste(Translate["title_cas_inc",input$lan],tail(covid19$fecha[covid19$idcountry==input$country],1)+3)
  )
  output$title_mort<-renderText(
    paste(Translate["title_mort",input$lan],tail(covid19$fecha[covid19$idcountry==input$country],1)+3)
  )
  output$title_mort_inc<-renderText(
    paste(Translate["title_mort_inc",input$lan],tail(covid19$fecha[covid19$idcountry==input$country],1)+3)
  )
  output$title_casl<-renderText(
    paste(Translate["title_casl",input$lan],tail(covid19$fecha[covid19$idcountry==input$country],1))
  )
  output$title_casf<-renderText(
    paste(Translate["title_casf",input$lan],tail(covid19$fecha[covid19$idcountry==input$country],1))
  )
  output$title_met<-renderText(
    Translate["menu_met",input$lan]
  )
  output$title_ab<-renderText(
    Translate["menu_ab",input$lan]
  )
  output$text_method<-renderUI({
    withMathJax(HTML(as.character(source(no_accents(paste0("Method",input$lan,".R")),encoding="UTF-8"))[1]))
  })
  output$text_about<-renderText({
    as.character(source(no_accents(paste0("About",input$lan,".R")),encoding="UTF-8"))[1]
  })
  
  #CAS
  
  cas_plot<-reactive({
    if(length(input$country)>0){
      
      dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=casos)
      
      cas.model <- glm(total~dia,family=poisson(link="log"),data=dat)
      cas.model.quartic <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat)
      pred.date <- seq(min(dat$fecha), by = "day", to = as.Date(tail(dat$fecha,1))+3)
      pred<- predict(cas.model,type="response",se.fit=TRUE,newdata=list(dia=1:length(pred.date)))
      pred2 <- predict(cas.model.quartic,se.fit=TRUE,type="response",newdata=list(dia=1:length(pred.date)))
      dat<-dat%>%dplyr::select(fecha,total)
      dat<-rbind(dat,data.frame(fecha=pred.date[!pred.date%in%dat$fecha],total=NA))
      dat<-dat%>%arrange(fecha)%>%mutate(fecha=as.character(fecha))
      dat$pred<-pred$fit
      dat$pred2<-pred2$fit
      dat$predict_uciu<-pred$fit+1.96*pred$se.fit
      dat$predict_ucil<-pred$fit-1.96*pred$se.fit
      dat$predict_uci2u<-pred2$fit+1.96*pred2$se.fit
      dat$predict_uci2l<-pred2$fit-1.96*pred2$se.fit
      
      dat
    }
  })
  
  output$where0<-renderText({
    if(length(input$country)>0 & length(input$lan)>0){
      unique(covid19$country[covid19$idcountry==input$country])
    }
  })
  
  output$plot0<-renderPlotly({
    if(length(input$country)>0 & length(input$lan)>0){
      plot<-plot_ly(data = cas_plot(), x = ~fecha, y = ~total,type="scatter",text = ~paste(paste0(Translate["date",input$lan],": "), fecha, '<br>',paste0(Translate["obs",input$lan],':'), total,'<br>',paste0(Translate["exp_qua",input$lan],':'),round(pred2,0)),hoverinfo = 'text',name=Translate["obs",input$lan]) %>%
        layout(xaxis = list(title=Translate["date",input$lan]), yaxis = list(title=Translate["menu_casi",input$lan]),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        add_trace(y = ~predict_uci2u, type = 'scatter', mode = 'lines',
                  fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group2')%>%
        add_trace(y = ~predict_uci2l, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group2')%>%
        add_trace(y = ~pred2, name = Translate["exp_qua",input$lan], mode = 'lines',legendgroup = 'group2') %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 750,
          height = 300
        ))
    }
  })
  
  output$table_mcas <- renderText({
    if(length(input$country)>0){
      
      dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=casos)
      
      cas.model <- glm(total~dia,family=poisson(link="log"),data=dat)
      cas.model.quadratic <- glm(total~dia+I(dia^2),family=poisson(link="log"),data=dat)
      cas.model.cubic <- glm(total~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat)
      cas.model.quartic <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat)
      
      taula<-tab_model(cas.model,cas.model.quadratic,cas.model.cubic,cas.model.quartic,show.se=T,show.ci = F,show.aic=T,show.r2 = F,show.dev = T,
                       pred.labels = c("Intercept","Day","Day^2","Day^3","Day^4"),transform = NULL,
                       dv.labels=c("Linear model","Quadratic model","Cubic model","Quartic model"))
      taula$knitr
      
    }
  })
  
  output$table_mcas_a <- renderText({
    
    if(length(input$country)>0){
      
      dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=casos)
      
      cas.model <- glm(total~dia,family=poisson(link="log"),data=dat)
      cas.model.quadratic <- glm(total~dia+I(dia^2),family=poisson(link="log"),data=dat)
      cas.model.cubic <- glm(total~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat)
      cas.model.quartic <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat)
      
      tab<-anova(cas.model,cas.model.quadratic,cas.model.cubic,cas.model.quartic,test = "Chisq")
      row.names(tab)<-c(Translate["mod_l",input$lan],Translate["mod_qua",input$lan],Translate["mod_c",input$lan],Translate["mod_quar",input$lan])
      tab$`Pr(>Chi)`<-ifelse(tab$`Pr(>Chi)`<0.0001,"<0.0001",paste(round(tab$`Pr(>Chi)`,4)))
      kable(tab,digits=c(0,1,0,1),format="html") %>%
        kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),full_width = F, position = "left") %>%
        add_header_above(c("Deviance analysis" = 6)) %>%
        add_header_above(c(" " = 6))  %>%
        add_header_above(c(" " = 6))  %>%
        add_header_above(c(" " = 6))
    }
    
  })
  
  #Reactive data for the coefficient evolution:
  
  dat_coef<-reactive({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=casos)
    
    b0l<-vector()
    b1l<-vector()
    b0q<-vector()
    b1q<-vector()
    b2q<-vector()
    b0c<-vector()
    b1c<-vector()
    b2c<-vector()
    b3c<-vector()
    b0q2<-vector()
    b1q2<-vector()
    b2q2<-vector()
    b3q2<-vector()
    b4q2<-vector()
    k<-1
    
    for(i in seq(5,NROW(dat),k) ) {
      
      cas.model <- glm(total~dia,family=poisson(link="log"),data=dat[1:i,])
      b0l[i/k]<-summary(cas.model)[["coefficients"]][, "z value"][1]
      b1l[i/k]<-summary(cas.model)[["coefficients"]][, "z value"][2]
      cas.model.quadratic <- glm(total~dia+I(dia^2),family=poisson(link="log"),data=dat[1:i,])
      b0q[i/k]<-summary(cas.model.quadratic)[["coefficients"]][, "z value"][1]
      b1q[i/k]<-summary(cas.model.quadratic)[["coefficients"]][, "z value"][2]
      b2q[i/k]<-summary(cas.model.quadratic)[["coefficients"]][, "z value"][3]
      cas.model.cubic <- glm(total~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat[1:i,])
      b0c[i/k]<-summary(cas.model.cubic)[["coefficients"]][, "z value"][1]
      b1c[i/k]<-summary(cas.model.cubic)[["coefficients"]][, "z value"][2]
      b2c[i/k]<-summary(cas.model.cubic)[["coefficients"]][, "z value"][3]
      b3c[i/k]<-summary(cas.model.cubic)[["coefficients"]][, "z value"][4]
      cas.model.quartic <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat[1:i,])
      b0q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][1]
      b1q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][2]
      b2q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][3]
      b3q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][4]
      b4q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][5]
      
    }
    
    long<-max(seq(k,NROW(dat),k)/k)
    
    dades<-data.frame(seq=rep(seq(k,NROW(dat),k),k),coef=c(rep("b0",long),rep("b1",long),rep("b2",long),rep("b3",long),rep("b4",long)),linial=c(b0l,b1l,rep(NA,long),rep(NA,long),rep(NA,long)),
                      quad=c(b0q,b1q,b2q,rep(NA,long),rep(NA,long)),cub=c(b0c,b1c,b2c,b3c,rep(NA,long)),
                      quar=c(b0q2,b1q2,b2q2,b3q2,b4q2))
    
    dades
  })
  
  
  output$plot_cas_beta2<-renderPlotly({
    
    if(length(input$country)>0){
      
      plot1<-plot_ly(data = dat_coef(), x = ~seq, y = ~quad,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(quad,2)),hoverinfo = 'text') %>%
        add_trace(y = ~quad, name = "model",color=~coef, mode = 'scatter',showlegend=F) %>%
        layout(title=list(text=Translate["mod_qua",input$lan],x=0.075,y=0.975),xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1,tickfont = list(size = 5)), yaxis = list(title="z-statistic",range=c(round(min(dat_coef()$quad,na.rm=T)-20),round(max(dat_coef()$quad,na.rm=T)+20))),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        ))
    }
    
  })
  
  output$plot_cas_beta3<-renderPlotly({
    
    if(length(input$country)>0){
      plot<-plot_ly(data = dat_coef(), x = ~seq, y = ~cub,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(cub,0)),hoverinfo = 'text') %>%
        add_trace(y = ~cub, name = "model",color=~coef, mode = 'scatter',showlegend=F) %>%
        layout(title=list(text=Translate["mod_c",input$lan],x=0.075,y=0.975),xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1,tickfont = list(size = 5)), yaxis = list(title="z-statistic",range=c(round(min(dat_coef()$cub,na.rm=T)-20),round(max(dat_coef()$cub,na.rm=T)+20))),
               margin = list(b = 130),
               annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        ))
      
    }
  })
  
  output$plot_cas_beta4<-renderPlotly({
    
    if(length(input$country)>0){
      
      plot<-plot_ly(data = dat_coef(), x = ~seq, y = ~quar,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(quar,0)),hoverinfo = 'text') %>%
        add_trace(y = ~quar, name = "model",color=~coef, mode = 'scatter',showlegend=F) %>% 
        layout(title=list(text=Translate["mod_quar",input$lan],x=0.075,y=0.975),xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1,tickfont = list(size = 5)), yaxis = list(title="t-statistic",range=c(round(min(dat_coef()$quar,na.rm=T)-20),round(max(dat_coef()$quar,na.rm=T)+20))),
               margin = list(b = 130),
               annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper', 
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        ))
      
    }
  })
  
  #Error plots:
  
  data_error<-reactive({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=casos)
    
    devl<-vector()
    resl<-vector()
    phil<-vector()
    devq<-vector()
    resq<-vector()
    phiq<-vector()
    devc<-vector()
    resc<-vector()
    phic<-vector()
    dev4<-vector()
    res4<-vector()
    phi4<-vector()
    
    k<-1
    
    for(i in seq(5,NROW(dat),k) ) {
      
      cas.model <- glm(total~dia,family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model,"pearson")
      devl[i/k]<-cas.model$deviance
      phil[i/k]<-round(sum(pr^2)/df.residual(cas.model),2)
      resl[i/k]<-mean(residuals(cas.model,"pearson")^2)
      cas.model.quadratic <- glm(total~dia+I(dia^2),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.quadratic,"pearson")
      devq[i/k]<-cas.model.quadratic$deviance
      phiq[i/k]<-round(sum(pr^2)/df.residual(cas.model.quadratic),2)
      resq[i/k]<-mean(residuals(cas.model.quadratic,"pearson")^2)
      cas.model.cubic <- glm(total~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.cubic,"pearson")
      devc[i/k]<-cas.model.cubic$deviance
      phic[i/k]<-round(sum(pr^2)/df.residual(cas.model.cubic),2)
      resc[i/k]<-mean(residuals(cas.model.cubic,"pearson")^2)
      cas.model.4th <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.4th,"pearson")
      dev4[i/k]<-cas.model.4th$deviance
      phi4[i/k]<-round(sum(pr^2)/df.residual(cas.model.4th),2)
      res4[i/k]<-mean(residuals(cas.model.4th,"pearson")^2)
      #https://stats.stackexchange.com/questions/71720/error-metrics-for-cross-validating-poisson-models
      #https://online.stat.psu.edu/stat504/node/86/
      #https://stackoverflow.com/questions/2531489/understanding-glmresiduals-and-residglm
      #https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/
    }
    
    long<-max(seq(k,NROW(dat),k)/k)
    
    dades<-data.frame(seq=rep(seq(k,NROW(dat),k),k),coef=c(rep("l",long),rep("q",long),rep("c",long),rep("4º",long)),dev=c(devl,devq,devc,dev4),
                      res=c(resl,resq,resc,res4),phi=c(phil,phiq,phic,phi4))
    dades$dev<-ifelse(dades$dev>10000,NA,dades$dev)
    dades$phi<-ifelse(dades$phi>100,NA,dades$phi)
    dades$res<-ifelse(dades$res>100,NA,dades$res)
    dades$coef<-factor(dades$coef,levels=c("l","q","c","4º"),labels=c("linear","quadratic","cubic","quartic"))
    dades
    
  } )
  
  output$plot_cas_error1<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_error(), x = ~seq, y = ~phi,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(phi,2)),hoverinfo = 'text') %>%
        #add_trace(y = ~dev, name = "model",color=~coef, mode = 'scatter',showlegend=F) %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Overdispersion"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
    
  })
  
  output$plot_cas_error2<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_error(), x = ~seq, y = ~res,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Estimador",':'), round(res,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Sum of Pearson's residuals"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
    
  })
  output$plot_cas_error3<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_error(), x = ~seq, y = ~dev,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(dev,2)),hoverinfo = 'text') %>%
        #add_trace(y = ~dev, name = "model",color=~coef, mode = 'scatter',showlegend=F) %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Deviance"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
    
    
    
  })
  
  # Casos incidents
  
  cas_inc_plot<-reactive({
    
    if(length(input$country)>0){
      
      dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1))
      cas.model.quartic <- glm(casosf~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat)
      dat$lab<-weekdays(as.Date(dat$fecha))
      cas.model.quartic.lab <- glm(casosf~dia+I(dia^2)+I(dia^3)+I(dia^4)+lab,family=poisson(link="log"),data=dat)
      pred.date <- seq(min(dat$fecha), by = "day", to = as.Date(tail(dat$fecha,1))+3)
      pred<- predict(cas.model.quartic,type="response",se.fit=TRUE,newdata=list(dia=1:length(pred.date),lab=weekdays(pred.date)))
      pred2 <- predict(cas.model.quartic.lab,se.fit=TRUE,type="response",newdata=list(dia=1:length(pred.date),lab=weekdays(pred.date)))
      dat<-dat%>%dplyr::select(fecha,casosf)
      dat<-rbind(dat,data.frame(fecha=pred.date[!pred.date%in%dat$fecha],casosf=NA))
      dat<-dat%>%arrange(fecha)%>%mutate(fecha=as.character(fecha))
      dat$pred<-pred$fit
      dat$pred2<-pred2$fit
      dat$predict_uciu<-pred$fit+1.96*pred$se.fit
      dat$predict_ucil<-pred$fit-1.96*pred$se.fit
      dat$predict_uci2u<-pred2$fit+1.96*pred2$se.fit
      dat$predict_uci2l<-pred2$fit-1.96*pred2$se.fit
      
      dat
    }
  })
  
  output$where_inc<-renderText({
    if(length(input$country)>0 & length(input$lan)>0){
      unique(covid19$country[covid19$idcountry==input$country])
    }
  })
  
  output$plot_inc<-renderPlotly({
    if(length(input$country)>0 & length(input$lan)>0){
      plot<-plot_ly(data = cas_inc_plot(), x = ~fecha, y = ~casosf,type="scatter",text = ~paste(paste0(Translate["date",input$lan],": "), fecha, '<br>',paste0(Translate["obs",input$lan],':'), casosf,'<br>',paste0(Translate["expi",input$lan],':'),round(pred,0),'<br>',paste0(Translate["expi_qua",input$lan],':'),round(pred2,0)),hoverinfo = 'text',name=Translate["obs",input$lan]) %>%
        layout(xaxis = list(title=Translate["date",input$lan]), yaxis = list(title=Translate["new_casf",input$lan]),
               margin = list(b = 130),
               annotations =
                 list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                      xanchor='right', yanchor='auto', xshift=0, yshift=0),
               legend=list(traceorder="normal")
        ) %>%
        add_trace(y = ~predict_uciu, type = 'scatter', mode = 'lines',
                  fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group1') %>%
        add_trace(y = ~predict_ucil, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group1') %>%
        add_trace(y = ~pred, name = Translate["expi",input$lan], mode = 'lines',legendgroup = 'group1')%>%
        add_trace(y = ~predict_uci2u, type = 'scatter', mode = 'lines',
                  fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group2')%>%
        add_trace(y = ~predict_uci2l, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group2')%>%
        add_trace(y = ~pred2, name = Translate["expi_qua",input$lan], mode = 'lines',legendgroup = 'group2') %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 750,
          height = 300
        ))
    }
  })
  
  output$table_mca_inc <- renderText({
    if(length(input$country)>0){
      
      dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1))
      cas.model.quadratic <- glm(casosf~dia+I(dia^2),family=poisson(link="log"),data=dat)
      cas.model.cubic <- glm(casosf~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat)
      cas.model.quartic <- glm(casosf~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat)
      dat$lab<-as.factor(weekdays(as.Date(dat$fecha)))
      dat$lab<-factor(dat$lab,levels(dat$lab)[c(2,3,4,1,7,5,6)])
      dat$lab<-relevel(dat$lab,ref=levels(dat$lab)[1])
      cas.model.quartic.lab <- glm(casosf~dia+I(dia^2)+I(dia^3)+I(dia^4)+lab,family=poisson(link="log"),data=dat)
      
      taula<-tab_model(cas.model.quadratic,cas.model.cubic,cas.model.quartic,cas.model.quartic.lab ,show.se=T,show.ci = F,show.aic=T,show.r2 = F,show.dev = T,
                       pred.labels = c("Intercept","Day","Day^2","Day^3","Day^4","Monday","Tuesday","Sunday","Friday","Wednesday","Saturday"),
                       dv.labels=c("Quadratic model","Cubic model","Quartic model","Labour day model"),transform=NULL)
      
      taula$knitr
    }
  })
  
  output$table_mca_inc_a <- renderText({
    
    if(length(input$country)>0){
      
      dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1))
      cas.model.quadratic <- glm(casosf~dia+I(dia^2),family=poisson(link="log"),data=dat)
      cas.model.cubic <- glm(casosf~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat)
      cas.model.quartic<-glm(casosf~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat)
      dat$lab<-as.factor(weekdays(as.Date(dat$fecha)))
      dat$lab<-factor(dat$lab,levels(dat$lab)[c(2,3,4,1,7,5,6)])
      dat$lab<-relevel(dat$lab,ref=levels(dat$lab)[1])
      cas.model.quartic.lab <- glm(casosf~dia+I(dia^2)+I(dia^3)+I(dia^4)+lab,family=poisson(link="log"),data=dat)
      tab<-anova(cas.model.quadratic,cas.model.cubic,cas.model.quartic,cas.model.quartic.lab,test = "Chisq")
      row.names(tab)<-c("Quadratic model","Cubic model","Quartic model","Labour day model")
      tab$`Pr(>Chi)`<-ifelse(tab$`Pr(>Chi)`<0.0001,"<0.0001",paste(round(tab$`Pr(>Chi)`,4)))
      kable(tab,digits=c(0,1,0,1),format="html") %>%
        kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),full_width = F, position = "left") %>%
        add_header_above(c("Deviance analysis" = 6)) %>%
        add_header_above(c(" " = 6))  %>%
        add_header_above(c(" " = 6))  %>%
        add_header_above(c(" " = 6))
      
    }
  })
  
  #Error plots:
  data_inc_error<-reactive({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=casos,totalf=casosf)
    
    dat$lab<-weekdays(as.Date(dat$fecha))
    
    devl<-vector()
    resl<-vector()
    phil<-vector()
    devq<-vector()
    resq<-vector()
    phiq<-vector()
    devc<-vector()
    resc<-vector()
    phic<-vector()
    dev4<-vector()
    res4<-vector()
    phi4<-vector()
    
    k<-1
    
    for(i in seq(5,NROW(dat),k) ) {
      
      cas.model <- glm(totalf~dia,family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model,"pearson")
      devl[i/k]<-cas.model$deviance
      phil[i/k]<-round(sum(pr^2)/df.residual(cas.model),2)
      resl[i/k]<-mean(residuals(cas.model,"pearson")^2)
      
      cas.model.quadratic <- glm(totalf~dia+I(dia^2),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.quadratic,"pearson")
      devq[i/k]<-cas.model.quadratic$deviance
      phiq[i/k]<-round(sum(pr^2)/df.residual(cas.model.quadratic),2)
      resq[i/k]<-mean(residuals(cas.model.quadratic,"pearson")^2)
      
      cas.model.cubic <- glm(totalf~dia+I(dia^2)+I(dia^3)+lab,family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.cubic,"pearson")
      devc[i/k]<-cas.model.cubic$deviance
      phic[i/k]<-round(sum(pr^2)/df.residual(cas.model.cubic),2)
      resc[i/k]<-mean(residuals(cas.model.cubic,"pearson")^2)
      
      cas.model.4th <- glm(totalf~dia+I(dia^2)+I(dia^3)+I(dia^4)+lab,family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.4th,"pearson")
      dev4[i/k]<-cas.model.4th$deviance
      phi4[i/k]<-round(sum(pr^2)/df.residual(cas.model.4th),2)
      res4[i/k]<-mean(residuals(cas.model.4th,"pearson")^2)
      #https://stats.stackexchange.com/questions/71720/error-metrics-for-cross-validating-poisson-models
      #https://online.stat.psu.edu/stat504/node/86/
      #https://stackoverflow.com/questions/2531489/understanding-glmresiduals-and-residglm
      #https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/
    }
    
    long<-max(seq(k,NROW(dat),k)/k)
    
    dades<-data.frame(seq=rep(seq(k,NROW(dat),k),k),coef=c(rep("l",long),rep("q",long),rep("c",long),rep("4º",long)),dev=c(devl,devq,devc,dev4),
                      res=c(resl,resq,resc,res4),phi=c(phil,phiq,phic,phi4))
    dades$dev<-ifelse(dades$dev>20000,NA,dades$dev)
    dades$phi<-ifelse(dades$phi>200,NA,dades$phi)
    dades$res<-ifelse(dades$res>200,NA,dades$res)
    dades$coef<-factor(dades$coef,levels=c("l","q","c","4º"),labels=c("linear","quadratic","cubic","4º"))
    dades
    
  } )
  
  output$plot_inc_error1<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_inc_error(), x = ~seq, y = ~phi,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(phi,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Overdispersion"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
    
  })
    
  output$plot_inc_error2<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_inc_error(), x = ~seq, y = ~res,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Estimador",':'), round(res,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Sum of Pearson's residuals"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
    
  })
  output$plot_inc_error3<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_inc_error(), x = ~seq, y = ~dev,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(dev,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Deviance"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
    
  })
  

  #Mortality:
  
  mor_plot<-reactive({
    
    if(length(input$country)>0){
      
      dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=fallecidos)
      
      mor.model <- glm(total~dia,family=poisson(link="log"),data=dat)
      mor.model.quartic <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat)
      pred.date <- seq(head(dat$fecha,1), by = "day", to = as.Date(tail(dat$fecha,1))+3)
      pred<- predict(mor.model,type="response",se.fit=TRUE,newdata=list(dia=1:length(pred.date)))
      pred2 <- predict(mor.model.quartic,type="response",se.fit=TRUE,newdata=list(dia=1:length(pred.date)))
      dat<-dat%>%dplyr::select(fecha,total)
      dat<-rbind(dat,data.frame(fecha=pred.date[!pred.date%in%dat$fecha],total=NA))
      dat<-dat%>%arrange(fecha)%>%mutate(fecha=as.character(fecha))
      dat$pred<-pred$fit
      dat$pred2<-pred2$fit
      dat$predict_uciu<-pred$fit+1.96*pred$se.fit
      dat$predict_ucil<-pred$fit-1.96*pred$se.fit
      dat$predict_uci2u<-pred2$fit+1.96*pred2$se.fit
      dat$predict_uci2l<-pred2$fit-1.96*pred2$se.fit
      
      dat
    }
  })
  
  
  output$where2<-renderText({
    if(length(input$country)>0 & length(input$lan)>0){
      unique(covid19$country[covid19$idcountry==input$country])
    }
  })
  
  output$plot2<-renderPlotly({
    if(length(input$country)>0 & length(input$lan)>0){
      plot<-plot_ly(data = mor_plot(), x = ~fecha, y = ~total,type="scatter",text = ~paste(paste0(Translate["date",input$lan],": "), fecha, '<br>',paste0(Translate["obs",input$lan],':'), total,'<br>',paste0(Translate["exp_qua",input$lan],':'),round(pred2,0)),hoverinfo = 'text',name=Translate["obs",input$lan]) %>%
        layout(xaxis = list(title=Translate["date",input$lan]), yaxis = list(title=Translate["menu_mort",input$lan],range=c(0,na.exclude(unique(mor_plot()$Total))*4)),
               margin = list(b = 130),
               annotations =
                 list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                      xanchor='right', yanchor='auto', xshift=0, yshift=0),
               legend=list(traceorder="normal")
        ) %>% 
        add_trace(y = ~predict_uci2u, type = 'scatter', mode = 'lines',
                  fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group2')%>%
        add_trace(y = ~predict_uci2l, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group2')%>%
        add_trace(y = ~pred2, name = Translate["exp_qua",input$lan], mode = 'lines',legendgroup = 'group2') %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 750,
          height = 300
        ))
    }
  })
  
  output$table_mmort <- renderText({
    
    if(length(input$country)>0){
      dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=fallecidos)
      
      cas.model <- glm(total~dia,family=poisson(link="log"),data=dat)
      cas.model.quadratic <- glm(total~dia+I(dia^2),family=poisson(link="log"),data=dat)
      cas.model.cubic <- glm(total~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat)
      cas.model.quartic <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat)
      
      taula<-tab_model(cas.model,cas.model.quadratic,cas.model.cubic,cas.model.quartic,show.ci=F,show.se=T,show.aic=T,show.r2 = F,show.dev = T,
                       pred.labels = c("Intercept","Day","Day^2","Day^3","Day^4"),transform = NULL,
                       dv.labels=c("Linear model","Quadratic model","Cubic model","Quartic model"))
      taula$knitr
      
    }
  })
  
  output$table_mmort_a <- renderText({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=fallecidos)
    
    cas.model <- glm(total~dia,family=poisson(link="log"),data=dat)
    cas.model.quadratic <- glm(total~dia+I(dia^2),family=poisson(link="log"),data=dat)
    cas.model.cubic <- glm(total~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat)
    cas.model.quartic <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat)
    
    tab<-anova(cas.model,cas.model.quadratic,cas.model.cubic,cas.model.quartic,test = "Chisq")
    row.names(tab)<-c(Translate["mod_l",input$lan],Translate["mod_qua",input$lan],Translate["mod_c",input$lan],Translate["mod_quar",input$lan])
    tab$`Pr(>Chi)`<-ifelse(tab$`Pr(>Chi)`<0.0001,"<0.0001",paste(round(tab$`Pr(>Chi)`,4)))
    kable(tab,digits=c(0,1,0,1),format="html") %>%
      kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),full_width = F, position = "left") %>%
      add_header_above(c("Deviance analysis" = 6)) %>%
      add_header_above(c(" " = 6))  %>%
      add_header_above(c(" " = 6))  %>%
      add_header_above(c(" " = 6))
    
  })
  
  dat_mort_coef<-reactive({
    
    if(length(input$country)>0){
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=fallecidos)
    
    b0l<-vector()
    b1l<-vector()
    b0q<-vector()
    b1q<-vector()
    b2q<-vector()
    b0c<-vector()
    b1c<-vector()
    b2c<-vector()
    b3c<-vector()
    b0q2<-vector()
    b1q2<-vector()
    b2q2<-vector()
    b3q2<-vector()
    b4q2<-vector()
    k<-1
    
    for(i in seq(k,NROW(dat),k) ) {
      
      cas.model <- glm(total~dia,family=poisson(link="log"),data=dat[1:i,])
      b0l[i/k]<-summary(cas.model)[["coefficients"]][, "z value"][1]
      b1l[i/k]<-summary(cas.model)[["coefficients"]][, "z value"][2]
      cas.model.quadratic <- glm(total~dia+I(dia^2),family=poisson(link="log"),data=dat[1:i,])
      b0q[i/k]<-summary(cas.model.quadratic)[["coefficients"]][, "z value"][1]
      b1q[i/k]<-summary(cas.model.quadratic)[["coefficients"]][, "z value"][2]
      b2q[i/k]<-summary(cas.model.quadratic)[["coefficients"]][, "z value"][3]
      cas.model.cubic <- glm(total~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat[1:i,])
      b0c[i/k]<-summary(cas.model.cubic)[["coefficients"]][, "z value"][1]
      b1c[i/k]<-summary(cas.model.cubic)[["coefficients"]][, "z value"][2]
      b2c[i/k]<-summary(cas.model.cubic)[["coefficients"]][, "z value"][3]
      b3c[i/k]<-summary(cas.model.cubic)[["coefficients"]][, "z value"][4]
      cas.model.quartic <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat[1:i,])
      b0q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][1]
      b1q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][2]
      b2q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][3]
      b3q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][4]
      b4q2[i/k]<-summary(cas.model.quartic)[["coefficients"]][, "z value"][5]
    }
    
    long<-max(seq(k,NROW(dat),k)/k)
    
    dades<-data.frame(seq=rep(seq(k,NROW(dat),k),k),coef=c(rep("b0",long),rep("b1",long),rep("b2",long),rep("b3",long),rep("b4",long)),linial=c(b0l,b1l,rep(NA,long),rep(NA,long),rep(NA,long)),
                      quad=c(b0q,b1q,b2q,rep(NA,long),rep(NA,long)),cub=c(b0c,b1c,b2c,b3c,rep(NA,long)),
                      quar=c(b0q2,b1q2,b2q2,b3q2,b4q2))
    dades
    }
  })
  
  output$plot_mort_beta2<-renderPlotly({
    
    plot1<-plot_ly(data = dat_mort_coef(), x = ~seq, y = ~quad,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(quad,2)),hoverinfo = 'text') %>%
      add_trace(y = ~quad, name = "model",color=~coef, mode = 'scatter',showlegend=F) %>%
      layout(title=list(text=Translate["mod_qua",input$lan],x=0.075,y=0.975),xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1,tickfont = list(size = 6)), yaxis = list(title="z-statistic",range=c(round(min(dat_mort_coef()$quad,na.rm=T)-20),round(max(dat_mort_coef()$quad,na.rm=T)+20))),
             margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                       xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
      config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
        format = "png",
        width = 350,
        height = 650
      ))
    
  })
  
  output$plot_mort_beta3<-renderPlotly({
    
    plot<-plot_ly(data = dat_mort_coef(), x = ~seq, y = ~cub,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(cub,0)),hoverinfo = 'text') %>%
      add_trace(y = ~cub, name = "model",color=~coef, mode = 'scatter',showlegend=F) %>%
      layout(title=list(text=Translate["mod_c",input$lan],x=0.075,y=0.975),xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1,tickfont = list(size = 6)), yaxis = list(title="z-statistic",range=c(round(min(dat_mort_coef()$cub,na.rm=T)-20),round(max(dat_mort_coef()$cub,na.rm=T)+20))),
             margin = list(b = 130),
             annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
      config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
        format = "png",
        width = 350,
        height = 650
      ))
    
  })
  
  output$plot_mort_beta4<-renderPlotly({
    
    
    plot<-plot_ly(data = dat_mort_coef(), x = ~seq, y = ~quar,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(quar,0)),hoverinfo = 'text') %>%
      add_trace(y = ~quar, name = "model",color=~coef, mode = 'scatter',showlegend=F) %>% 
      layout(title=list(text=Translate["mod_quar",input$lan],x=0.075,y=0.975),xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1,tickfont = list(size = 6)), yaxis = list(title="t-statistic",range=c(round(min(dat_mort_coef()$quar,na.rm=T)-20),round(max(dat_mort_coef()$quar,na.rm=T)+20))),
             margin = list(b = 130),
             annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper', 
                                xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
      config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
        format = "png",
        width = 350,
        height = 650
      ))
    
  })
  
  #Error plots: 
  
  data_mort_error<-reactive({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=fallecidos)
    
    devl<-vector()
    resl<-vector()
    phil<-vector()
    devq<-vector()
    resq<-vector()
    phiq<-vector()
    devc<-vector()
    resc<-vector()
    phic<-vector()
    dev4<-vector()
    res4<-vector()
    phi4<-vector()
    
    k<-1
    
    for(i in seq(5,NROW(dat),k) ) {
      
      cas.model <- glm(total~dia,family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model,"pearson")
      devl[i/k]<-cas.model$deviance
      phil[i/k]<-round(sum(pr^2)/df.residual(cas.model),2)
      resl[i/k]<-mean(residuals(cas.model,"pearson")^2)
      cas.model.quadratic <- glm(total~dia+I(dia^2),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.quadratic,"pearson")
      devq[i/k]<-cas.model.quadratic$deviance
      phiq[i/k]<-round(sum(pr^2)/df.residual(cas.model.quadratic),2)
      resq[i/k]<-mean(residuals(cas.model.quadratic,"pearson")^2)
      cas.model.cubic <- glm(total~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.cubic,"pearson")
      devc[i/k]<-cas.model.cubic$deviance
      phic[i/k]<-round(sum(pr^2)/df.residual(cas.model.cubic),2)
      resc[i/k]<-mean(residuals(cas.model.cubic,"pearson")^2)
      cas.model.4th <- glm(total~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.4th,"pearson")
      dev4[i/k]<-cas.model.4th$deviance
      phi4[i/k]<-round(sum(pr^2)/df.residual(cas.model.4th),2)
      res4[i/k]<-mean(residuals(cas.model.4th,"pearson")^2)
      #https://stats.stackexchange.com/questions/71720/error-metrics-for-cross-validating-poisson-models
      #https://online.stat.psu.edu/stat504/node/86/
      #https://stackoverflow.com/questions/2531489/understanding-glmresiduals-and-residglm
      #https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/
    }
    
    long<-max(seq(k,NROW(dat),k)/k)
    
    dades<-data.frame(seq=rep(seq(k,NROW(dat),k),k),coef=c(rep("l",long),rep("q",long),rep("c",long),rep("4º",long)),dev=c(devl,devq,devc,dev4),
                      res=c(resl,resq,resc,res4),phi=c(phil,phiq,phic,phi4))
    dades$dev<-ifelse(dades$dev>20000,NA,dades$dev)
    dades$phi<-ifelse(dades$phi>200,NA,dades$phi)
    dades$res<-ifelse(dades$res>200,NA,dades$res)
    dades$coef<-factor(dades$coef,levels=c("l","q","c","4º"),labels=c("linear","quadratic","cubic","quartic"))
    dades
    
  } )
  
  output$plot_mort_error1<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_mort_error(), x = ~seq, y = ~phi,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(phi,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Overdispersion"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
    
  })
  
  output$plot_mort_error2<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_mort_error(), x = ~seq, y = ~res,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Estimador",':'), round(res,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Sum of Pearson's residuals"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
  })
  
  output$plot_mort_error3<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_mort_error(), x = ~seq, y = ~dev,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(dev,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Deviance"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
    
  })
  
  # Daily mortality
  mort_inc_plot<-reactive({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1))
    
    dat$lab<-weekdays(dat$fecha)
    
    cas.model.cubic <- glm(fallecidosf~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat)
    cas.model.cubic.lab <- glm(fallecidosf~dia+I(dia^2)+I(dia^3)+lab,family=poisson(link="log"),data=dat)
    pred.date <- seq(min(dat$fecha), by = "day", to = as.Date(tail(dat$fecha,1))+3)
    pred<- predict(cas.model.cubic,type="response",se.fit=TRUE,newdata=list(dia=1:length(pred.date),lab=weekdays(pred.date)))
    pred2 <- predict(cas.model.cubic.lab,se.fit=TRUE,type="response",newdata=list(dia=1:length(pred.date),lab=weekdays(pred.date)))
    dat<-dat%>%dplyr::select(fecha,fallecidosf)
    dat<-rbind(dat,data.frame(fecha=pred.date[!pred.date%in%dat$fecha],fallecidosf=NA))
    dat<-dat%>%arrange(fecha)%>%mutate(fecha=as.character(fecha))
    dat$pred<-pred$fit
    dat$pred2<-pred2$fit
    dat$predict_uciu<-pred$fit+1.96*pred$se.fit
    dat$predict_ucil<-pred$fit-1.96*pred$se.fit
    dat$predict_uci2u<-pred2$fit+1.96*pred2$se.fit
    dat$predict_uci2l<-pred2$fit-1.96*pred2$se.fit
    
    dat
  })
  
  output$where_mort_inc<-renderText({
    if(length(input$ca_mort_inc)>0 & length(input$lan)>0){
      unique(covid19$country[covid19$idcountry==input$country])
    }
  })
  
  output$plot_mort_inc<-renderPlotly({
    if(length(input$country)>0 & length(input$lan)>0){
      plot<-plot_ly(data = mort_inc_plot(), x = ~fecha, y = ~fallecidosf,type="scatter",text = ~paste(paste0(Translate["date",input$lan],": "), fecha, '<br>',paste0(Translate["obs",input$lan],':'), fallecidosf,'<br>',paste0(Translate["expi",input$lan],':'),round(pred,0),'<br>',paste0(Translate["expi_qua",input$lan],':'),round(pred2,0)),hoverinfo = 'text',name=Translate["obs",input$lan]) %>%
        layout(xaxis = list(title=Translate["date",input$lan]), yaxis = list(title=Translate["menu_mort_inc",input$lan]),
               margin = list(b = 130),
               annotations =
                 list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                      xanchor='right', yanchor='auto', xshift=0, yshift=0),
               legend=list(traceorder="normal")
        ) %>%
        add_trace(y = ~predict_uciu, type = 'scatter', mode = 'lines',
                  fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group1') %>%
        add_trace(y = ~predict_ucil, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group1') %>%
        add_trace(y = ~pred, name = Translate["expi",input$lan], mode = 'lines',legendgroup = 'group1')%>%
        add_trace(y = ~predict_uci2u, type = 'scatter', mode = 'lines',
                  fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group2')%>%
        add_trace(y = ~predict_uci2l, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,legendgroup = 'group2')%>%
        add_trace(y = ~pred2, name = Translate["expi_qua",input$lan], mode = 'lines',legendgroup = 'group2') %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 750,
          height = 300
        ))
    }
  })
  
  output$table_mmort_inc <- renderText({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1))
    dat$lab<-weekdays(dat$fecha)
    
    cas.model.quadratic <- glm(fallecidosf~dia+I(dia^2),family=poisson(link="log"),data=dat)
    cas.model.cubic <- glm(fallecidosf~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat)
    dat$lab<-as.factor(weekdays(as.Date(dat$fecha)))
    dat$lab<-factor(dat$lab,levels(dat$lab)[c(2,3,4,1,7,5,6)])
    dat$lab<-relevel(dat$lab,ref=levels(dat$lab)[1])
    cas.model.cubic.lab <- glm(fallecidosf~dia+I(dia^2)+I(dia^3)+lab,family=poisson(link="log"),data=dat)
    
    taula<-tab_model(cas.model.quadratic,cas.model.cubic,cas.model.cubic.lab ,show.se=T,show.aic=T,show.r2 = F,show.dev = T,
                     pred.labels = c("Intercept","Day","Day^2","Day^3","Monday","Tuesday","Sunday","Friday","Wednesday","Saturday"),
                     collapse.se=T,dv.labels=c("Quadratic model","Cubic model","Labour day model"),
                     CSS=list(css.tdata='line-height: 150%;'))
    taula$knitr
    
  })
  
  output$table_mmort_inc_a <- renderText({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1))
    dat$lab<-weekdays(dat$fecha)
    
    cas.model.quadratic <- glm(fallecidosf~dia+I(dia^2),family=poisson(link="log"),data=dat)
    cas.model.cubic <- glm(fallecidosf~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat)
    dat$lab<-as.factor(weekdays(as.Date(dat$fecha)))
    dat$lab<-factor(dat$lab,levels(dat$lab)[c(2,3,4,1,7,5,6)])
    dat$lab<-relevel(dat$lab,ref=levels(dat$lab)[1])
    cas.model.cubic.lab <- glm(fallecidosf~dia+I(dia^2)+I(dia^3)+lab,family=poisson(link="log"),data=dat)
    tab<-anova(cas.model.quadratic,cas.model.cubic,cas.model.cubic.lab,test = "Chisq")
    row.names(tab)<-c("Quadratic model","Cubic model","Labour day model")
    tab$`Pr(>Chi)`<-ifelse(tab$`Pr(>Chi)`<0.0001,"<0.0001",paste(round(tab$`Pr(>Chi)`,4)))
    kable(tab,digits=c(0,1,0,1),format="html") %>%
      kable_styling(bootstrap_options = c("hover", "condensed", "responsive"),full_width = F, position = "left") %>%
      add_header_above(c("Deviance analysis" = 6)) %>%
      add_header_above(c(" " = 6))  %>%
      add_header_above(c(" " = 6))  %>%
      add_header_above(c(" " = 6))
    
  })
  
  #Case fatality rate:
  
  casl_plot<-reactive({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(cfr=ifelse(fallecidos==0,0,round(100*fallecidos/casos,1)))

    dat$t<-seq(1:nrow(dat))
    model.all <- glm(fallecidos~t+ offset(log(casos)),family=poisson(link="log"),data=dat)
    model.all.c <- glm(fallecidos~t+I(t^2)+I(t^3)+offset(log(casos)),family=poisson(link="log"),data=dat)
    pred.date <- seq(min(dat$fecha), by = "day", length.out = as.integer(max(dat$fecha)-min(dat$fecha)+3+1))
    
    pre<-data.frame(fecha=dat$fecha,t=dat$t,fallecidos=dat$fallecidos,casos=1)
    p1<-predict(model.all,se.fit = T,type = "response",newdata = pre)$fit
    dat$p1<-round(100*c(p1),1)
    dat$up1<-dat$p1 + 1.96*sqrt(dat$p1)
    dat$lp1<-dat$p1 - 1.96*sqrt(dat$p1)
    p2<-predict(model.all.c,se.fit = T,type = "response",newdata = pre)$fit
    dat$p2<-round(100*c(p2),1)
    dat$up2<-dat$p2 + 1.96*sqrt(dat$p2)
    dat$lp2<-dat$p2 - 1.96*sqrt(dat$p2)
    dat
  })
  
  output$where_casl<-renderText({
    if(length(input$country)>0 & length(input$lan)>0){
      unique(covid19$country[covid19$idcountry==input$country])
    }
  })
  
  output$plot_casl<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot<-plot_ly(data = casl_plot(), x = ~fecha, y = ~cfr,type="scatter",text = ~paste(paste0(Translate["date",input$lan],": "), fecha, '<br>',paste0(Translate["eix_casl",input$lan],':'), cfr,'<br>',paste0(Translate["exp_qua",input$lan],': ',round(p2,1)),"%"),hoverinfo = 'text',name=Translate["eix_casl",input$lan]) %>%
        layout(xaxis = list(title=Translate["date",input$lan]), yaxis = list(title=Translate["eix_casl",input$lan]),
               margin = list(b = 130),range=c(0,50),
               annotations =   list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                    xanchor='right', yanchor='auto', xshift=0, yshift=0),
               legend=list(traceorder="normal")) %>%
        #add_trace(y = ~p1, name = Translate["exp",input$lan], mode = 'lines',legendgroup = 'group1')%>%
        add_trace(y = ~p2, name = Translate["exp_qua",input$lan], mode = 'lines',legendgroup = 'group2') %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 750,
          height = 300
        ))
    }
  })
  
  #Error plots:
  
  data_mort_inc_error<-reactive({
    
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=fallecidos,totalf=fallecidosf)
    
    dat$lab<-weekdays(dat$fecha)
    
    devl<-vector()
    resl<-vector()
    phil<-vector()
    devq<-vector()
    resq<-vector()
    phiq<-vector()
    devc<-vector()
    resc<-vector()
    phic<-vector()
    dev4<-vector()
    res4<-vector()
    phi4<-vector()
    
    k<-1
    
    for(i in seq(5,NROW(dat),k) ) {
      
      cas.model <- glm(totalf~dia,family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model,"pearson")
      devl[i/k]<-cas.model$deviance
      phil[i/k]<-round(sum(pr^2)/df.residual(cas.model),2)
      resl[i/k]<-mean(residuals(cas.model,"pearson")^2)
      cas.model.quadratic <- glm(totalf~dia+I(dia^2),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.quadratic,"pearson")
      devq[i/k]<-cas.model.quadratic$deviance
      phiq[i/k]<-round(sum(pr^2)/df.residual(cas.model.quadratic),2)
      resq[i/k]<-mean(residuals(cas.model.quadratic,"pearson")^2)
      cas.model.cubic <- glm(totalf~dia+I(dia^2)+I(dia^3),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.cubic,"pearson")
      devc[i/k]<-cas.model.cubic$deviance
      phic[i/k]<-round(sum(pr^2)/df.residual(cas.model.cubic),2)
      resc[i/k]<-mean(residuals(cas.model.cubic,"pearson")^2)
      cas.model.4th <- glm(totalf~dia+I(dia^2)+I(dia^3)+I(dia^4),family=poisson(link="log"),data=dat[1:i,])
      pr <- residuals(cas.model.4th,"pearson")
      dev4[i/k]<-cas.model.4th$deviance
      phi4[i/k]<-round(sum(pr^2)/df.residual(cas.model.4th),2)
      res4[i/k]<-mean(residuals(cas.model.4th,"pearson")^2)
      #https://stats.stackexchange.com/questions/71720/error-metrics-for-cross-validating-poisson-models
      #https://online.stat.psu.edu/stat504/node/86/
      #https://stackoverflow.com/questions/2531489/understanding-glmresiduals-and-residglm
      #https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/
    }
    
    long<-max(seq(k,NROW(dat),k)/k)
    
    dades<-data.frame(seq=rep(seq(k,NROW(dat),k),k),coef=c(rep("l",long),rep("q",long),rep("c",long),rep("4º",long)),dev=c(devl,devq,devc,dev4),
                      res=c(resl,resq,resc,res4),phi=c(phil,phiq,phic,phi4))
    dades$dev<-ifelse(dades$dev>20000,NA,dades$dev)
    dades$phi<-ifelse(dades$phi>200,NA,dades$phi)
    dades$res<-ifelse(dades$res>200,NA,dades$res)
    dades$coef<-factor(dades$coef,levels=c("l","q","c","4º"),labels=c("linear","quadratic","cubic","4º"))
    dades
    
  } )
  
  output$plot_mort_inc_error1<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_mort_inc_error(), x = ~seq, y = ~phi,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(phi,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Overdispersion"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
  })
  
  output$plot_mort_inc_error2<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_mort_inc_error(), x = ~seq, y = ~res,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Estimador",':'), round(res,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Sum of Pearson's residuals"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
  })
  
  output$plot_mort_inc_error3<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0){
      
      plot1<-plot_ly(data = data_mort_inc_error(), x = ~seq, y = ~dev,type="scatter",mode = 'lines',color=~coef,text = ~paste(paste0(Translate["day",input$lan],": "), seq, '<br>',paste0("Beta",':'), round(dev,2)),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["day",input$lan],tick0=0,dtick=4,ticklen=1), yaxis = list(title="Deviance"),
               margin = list(b = 130),annotations = list(x = 0.65, y = -0.8, text = paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  "),showarrow = F, xref='paper', yref='paper',
                                                         xanchor='right', yanchor='auto', xshift=0, yshift=0),legend=list(traceorder="normal") ) %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 350,
          height = 650
        )) %>% layout(legend = list(orientation = 'h',x = 0, y = -0.2))
    }
  })
  
  
  #CAS INF:
  
  output$wheref<-renderText({
    if(length(input$country)>0 & length(input$lan)>0){
      unique(covid19$country[covid19$idcountry==input$country])
    }
  })
  
  casf_plot2<-reactive({
    caf<-"Total"
    
    dat<-covid19 %>% subset(idcountry==input$country)#%>% mutate(dia=as.numeric(fecha-head(fecha,1)+1))
    fecha<-seq.Date(dat$fecha[1]-14,dat$fecha[1]-1,"day")
    dat<-rbind(data.frame(fecha=fecha,country=input$country,casos=0,fallecidos=NA,casosf=0),dat[,c("fecha","country","casos","fallecidos","casosf")])
    dat$t<-seq(1:nrow(dat))
    
    incubacio<-vector()
    for (dia in 1:13) {
      incubacio[dia]<-plnorm(dia,1.621,0.418) #https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7081172/
    }
    incubacio[14]<-1
    
    dat$casosinf<-0
    
    for(i in 15:(nrow(dat)-14)) {
      for (j in 14:1) {
        if (j==14) {
          dat$casosinf[i-j]=dat$casosinf[i-j]+dat$casosf[i]*incubacio[15-j]
        } else {
          dat$casosinf[i-j]=dat$casosinf[i-j]+(dat$casosf[i]*incubacio[15-j]-dat$casosf[i]*incubacio[15-j-1])
        }
      }
    }
    
    dat$casosinf<-ifelse(dat$casosinf==0,NA,round(dat$casosinf,0))
    dat$lab<-weekdays(as.Date(dat$fecha))
    cas.model.quadratic <- glm(casosf~t+I(t^2)+lab,family=poisson(link="log"),data=dat)
    cas.model.cubic <- glm(casosf~t+I(t^2)+I(t^3)+lab,family=poisson(link="log"),data=dat)
    
    pred.date <- seq(min(dat$fecha), by = "day", to = as.Date(tail(dat$fecha,1))+7)
    pred<- predict(cas.model.quadratic,type="response",se.fit=TRUE,newdata=list(t=1:length(pred.date),lab=weekdays(pred.date)))
    pred2<- predict(cas.model.cubic,type="response",se.fit=TRUE,newdata=list(t=1:length(pred.date),lab=weekdays(pred.date)))
    
    fechaf<-seq.Date(tail(dat$fecha,1)+1,tail(dat$fecha,1)+7,"day")
    dat<-rbind(dat,data.frame(fecha=fechaf,country=input$country,casos=0,casosf=NA,fallecidos=NA,t=(nrow(dat)+1):(nrow(dat)+7),casosinf=NA,lab=weekdays(fechaf)))
    dat$caspredit<-round(pred$fit,0)
    dat$caspredit2<-round(pred2$fit,0)
    
    dat$casosdiamix<-ifelse(!is.na(dat$casosf),dat$casosf,dat$caspredit)
    dat$casosdiamix2<-ifelse(!is.na(dat$casosf),dat$casosf,dat$caspredit2)
    
    dat$casosinfe<-0
    dat$casosinfe2<-0
    
    for(i in 15:(nrow(dat))) {
      for (j in 14:1) {
        if (j==14) {
          dat$casosinfe[i-j]=dat$casosinfe[i-j]+dat$casosdiamix[i]*incubacio[15-j]
        } else {
          dat$casosinfe[i-j]=dat$casosinfe[i-j]+(dat$casosdiamix[i]*incubacio[15-j]-dat$casosdiamix[i]*incubacio[15-j-1])
        }
      }
    }
    
    dat$casosinfe<-ifelse(dat$casosinfe==0,NA,round(dat$casosinfe,0))
    
    for(i in 15:(nrow(dat))) {
      for (j in 14:1) {
        if (j==14) {
          dat$casosinfe2[i-j]=dat$casosinfe2[i-j]+dat$casosdiamix2[i]*incubacio[15-j]
        } else {
          dat$casosinfe2[i-j]=dat$casosinfe2[i-j]+(dat$casosdiamix2[i]*incubacio[15-j]-dat$casosdiamix2[i]*incubacio[15-j-1])
        }
      }
    }
    
    dat$casosinfe2<-ifelse(dat$casosinfe2==0,NA,round(dat$casosinfe2,0))
    
    dat$filtre<-0
    dat$filtre<-ifelse(dat$t>=(NROW(dat)-14),1,dat$filtre)
    dat$filtre<-format(dat$filtre,levels=c(0,1),labels=c("Observats","Predits"))
    dat$casosinfe2_2<-dat$casosinfe2
    dat$casosinfe2<-ifelse(dat$filtre==0,dat$casosinfe2,NA)
    dat$casosinfe2_2<-ifelse(dat$filtre==1,dat$casosinfe2_2,NA)
    dat
  })
  
  output$plotf2<-renderPlotly({
    
    plot<-plot_ly(data = casf_plot2(), x = ~fecha, y = ~casosf,name=Translate["diagno",input$lan] ,type="bar",text = ~paste(paste0(" ",Translate["date",input$lan],": "), fecha, '<br>',paste0(Translate["inf",input$lan],':'), casosinfe2,'<br>',paste0(Translate["inf2",input$lan],':'), casosinfe2_2,'<br>',paste0(Translate["diagno",input$lan],':'),round(casosf,0),'<br>'),hoverinfo = 'text',legendgroup = 'group1') %>%
      layout(xaxis = list(range=c(head(casf_plot2()$fecha,1)-1,tail(casf_plot2()$fecha,1)-7)),yaxis = list(range=c(0,5.1))) %>%
      layout(xaxis = list(title=Translate["date",input$lan]), yaxis = list(title=Translate["new_casf",input$lan],range=c(0,na.exclude(unique(casf_plot2()$casosf))*4)),
             margin = list(b = 130),legend=list(traceorder="normal"),
             annotations = list(x = c(0.67), y = c(-0.55), text = c(paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  ")),showarrow = F, xref='paper', yref='paper',
                                xanchor='right', yanchor='auto', xshift=0, yshift=0)) %>%
      add_trace(y = ~casosinfe2,  color=I("darkolivegreen"), name=Translate["inf",input$lan],legendgroup = 'group2')%>%
      add_trace(y = ~casosinfe2_2,  color=I("red"), name=Translate["inf2",input$lan],legendgroup = 'group2')%>%
      config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
        format = "png",
        width = 830,
        height = 300
      ))
    
  })
  
  #R0
  
  output$menu_r0<-renderText(
    Translate["menu_r0",input$lan]
  )
  output$title_r0<-renderText(
    paste(Translate["title_r0",input$lan],tail(covid19$fecha[covid19$idcountry==input$country],1)+3)
  )
  output$wherer0<-renderText({
    if(length(input$country)>0 & length(input$lan)>0){
      unique(covid19$country[covid19$idcountry==input$country])
    }
  })
  
  r0_plot<-reactive({
    dat<-covid19 %>% subset(idcountry==input$country) %>% mutate(dia=as.numeric(fecha-head(fecha,1)+1)) %>% rename(total=casos)
    
    dat$incident.cases <- dat$total-c(0,dat$total[1:(nrow(dat)-1)])
    dat$incident.cases<-ifelse(dat$incident.cases<0,0,dat$incident.cases)
    
    dat$lab<-as.factor(weekdays(as.Date(dat$fecha)))
    dat$lab<-factor(dat$lab,levels(dat$lab)[c(2,3,4,1,7,5,6)])
    dat$lab<-relevel(dat$lab,ref=levels(dat$lab)[1])
    
    cas.model<- glm(incident.cases~dia+I(dia^2)+I(dia^3)+I(dia^4)+lab,family=poisson(link="log"),data=dat)
    pred.date <- seq(min(dat$fecha), by = "day", to = as.Date(tail(dat$fecha,1))+3)
    pred<- predict(cas.model,type="response",se.fit=TRUE,newdata=list(dia=1:length(pred.date),lab=weekdays(pred.date)))
    date_fit<-data.frame(fecha=pred.date,incident.cases=pred$fit) %>% mutate(obs=0)
    dat<-dat%>% subset(!is.na(incident.cases))%>% dplyr::select(fecha,incident.cases) %>% mutate(obs=1)
    dat<-rbind(dat,date_fit[date_fit$fecha%in%pred.date[!pred.date%in%dat$fecha],]) %>% arrange(fecha)
    res <- estimate_R(incid=data.frame(dates=dat$fecha,I=dat$incident.cases),
                      method = "parametric_si",
                      config = make_config(list(mean_si = 4.7, std_si = 2.9)))
    dat$res<-c(rep(NA,7),res$R[,"Mean(R)"])
    dat$res_inf<-c(rep(NA,7),res$R[,"Quantile.0.025(R)"])
    dat$res_sup<-c(rep(NA,7),res$R[,"Quantile.0.975(R)"])
    
    res<-dat$res
    dat$res<-ifelse(dat$obs==1,dat$res,NA)
    dat$fit<-ifelse(dat$obs==0,res,NA)
    dat$fit_text<-dat$fit
    dat$fit[tail(which(dat$obs==1),1)]<-dat$res[tail(which(dat$obs==1),1)]
    inf<-dat$res_inf
    dat$res_inf<-ifelse(dat$obs==1,dat$res_inf,NA)
    dat$fit_inf<-ifelse(dat$obs==0,inf,NA)
    dat$fit_inf[tail(which(dat$obs==1),1)]<-dat$res_inf[tail(which(dat$obs==1),1)]
    sup<-dat$res_sup
    dat$res_sup<-ifelse(dat$obs==1,dat$res_sup,NA)
    dat$fit_sup<-ifelse(dat$obs==0,sup,NA)
    dat$fit_sup[tail(which(dat$obs==1),1)]<-dat$res_sup[tail(which(dat$obs==1),1)]
    dat
  })
  
  output$log_scale<-renderUI({
    if(!is.null(input$lan)){
      checkboxInput("log_scale", Translate["log_scale",input$lan]) 
    }
  })
  
  output$plotr0<-renderPlotly({
    
    if(length(input$country)>0 & length(input$lan)>0 & length(input$log_scale)>0){
      plot<-plot_ly(data = r0_plot(), x = ~fecha, y = ~res,type="scatter", mode = 'lines',name=Translate["r0_obs",input$lan],text = ~paste(paste0(" ",Translate["date",input$lan],": "), fecha, '<br>',paste0(Translate["r0_obs",input$lan],':'), round(res,2),'<br>',paste0(Translate["r0_esp",input$lan],':'),round(fit_text,2),'<br>'),hoverinfo = 'text') %>%
        layout(xaxis = list(title=Translate["date",input$lan]), yaxis = list(title=Translate["menu_r0",input$lan],range=c(0,ceiling(max(6,c(r0_plot()$res_sup,r0_plot()$fit_sup),na.rm=T)))),
               margin = list(b = 130),legend=list(traceorder="normal"),
               annotations =list(x = c(0.67), y = c(-0.55), text = c(paste0(Translate["from",input$lan],": https://ubidi.shinyapps.io/covid19/  ")),showarrow = F, xref='paper', yref='paper',
                                 xanchor='right', yanchor='auto', xshift=0, yshift=0)) %>%
        add_trace(y = ~res_inf, type = 'scatter', mode = 'lines',
                  fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                  showlegend = FALSE,hoverinfo="none")%>%
        add_trace(y = ~res_sup, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),showlegend = FALSE,hoverinfo="none")   %>%
        add_trace(y = ~fit_inf, type = 'scatter', mode = 'lines',
                  fillcolor='rgb(255,227,203)', line = list(color = 'transparent'),
                  showlegend = FALSE,hoverinfo="none")%>%
        add_trace(y = ~fit_sup, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',fillcolor='rgb(255,227,203)', line = list(color = 'transparent'),showlegend = FALSE,hoverinfo="none")%>%
        add_trace(y = ~fit, type = 'scatter', mode = 'lines',name=Translate["r0_esp",input$lan],color="rgb(255, 127, 14)")%>%
        add_segments(y = 1, yend =1,x=min(r0_plot()$fecha),xend=max(r0_plot()$fecha), mode='lines',linetype =I('dot'), color = I('black'),name="R=1") %>%
        config(displaylogo = FALSE,modeBarButtonsToRemove = c("sendDataToCloud","editInChartStudio","pan2d","select2d","lasso2d","zoomIn2d","zoom2d","toggleSpikelines","zoomOut2d","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian"),toImageButtonOptions = list(
          format = "png",
          width = 830,
          height = 300
        ))
      
      if(input$log_scale){
        plot<-plot %>% layout(yaxis=list(type = "log",range=NULL),dtick=1)
      }
      
      plot
    }
  })
  
}

shinyApp(ui, server)
