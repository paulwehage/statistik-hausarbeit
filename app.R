#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

library(shiny)
library(plotly)
library(gginference)


# Define UI for application that draws a histogram
ui <- fluidPage(
  

  # Application title
  titlePanel("Rechtsseitiger Zwei-Stichproben-Hypothesentest im Kontext von Chlorwaschungen"),
  
  h4("Nullhypothese: Die Chlorwaschungen als Hygienemaßnahmen sorgen nicht für eine geringere Müttersterblichkeit"),
  
  #:https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3807776/#:~:text=From%201840%20through%201846%2C%20the,were%20due%20to%20puerperal%20fever.
  # https://de.wikipedia.org/wiki/Ignaz_Semmelweis
  
  p("Ignaz Semmelweis (1818 - 1865) war ein ungarisch-österreichischer Chirurg und Geburtshelfer in der Wiener Entbindungsklinik, welche in zwei Unterkliniken unterteilt war -  in der ersten arbeiteten Studierende und Ärzte und in der zweiten nur Hebammen. Mit der Zeit fiel ihm auf, dass in der ersten Klinik weitaus mehr Mütter am Kindbettfieber nach der Entbindung starben als in der Zweiten und er führte es auf Fehler in Routine-Maßnahmen zurück. 
    Er beobachtete, dass die Mitarbeiter der ersten Klinik vor Entbindungen oftmals die Leichen der am Vortag verstorbenen Mütter obduzierten und dann ohne Hygienemaßnahmen weiter zur Entbindung gingen, wohingegen die Hebammen der zweiten Klinik keinen Kontakt zu Leichen hatten. Zwischen diesen Fakten stellte er eine Korrellation fest und das noch vor der Entdeckung der Rolle von Bakterien bei Krankheiten. Semmelweis Konsequenz darauf war, dass er die 
    Mitarbeiter der ersten Klinik dazu verpflichtete, vor einer Entbindung ihre Hände und das Werkzeug mit Chlorkalk zu desinfizieren. Das Ergebnis war eindeutig - von fast 10% Sterblichkeit auf 1,2%, was sogar die Hebammen schlug."),
  p("Die Zahlen und Vorgehensweisen hat er in seiner Studie 'The Etiology, Concept, and Prophylaxis of Childbed Fever' akribisch dokumentiert und der Fall gilt damit heute als der erste Fall evidenzbasierter Medizin und als Musterbeispiel wissenschaftlicher Hypothesen."),
  p("Auch wenn Semmelweis sehr vorbidlich gearbeitet hat, hätte Ihm ein interaktives Interface sicherlich geholfen seine Hypothese der Chlorwaschung zu unterstüzen."),
  
  br(),br(),br(),
  
  navbarPage("Darstellungen",
    tabPanel("Interaktiver Hypothesentest",
       sidebarPanel(
         width = 3,
         h5(strong("Ohne Chlorwaschung")),
         numericInput("mortWCl", "Todesfälle der Frauen", value = "60", width = 1000),
         numericInput("stichWCl", "Stichpropengröße der Patientinnen", value = "100", width = 1000),
         br(),br(),br(),
         sliderInput("vorChlor", "Jahr in Klinik 1",min=1840,max=1846,step = 1,value=1845,sep = "")
       ),
      
       mainPanel(align = "center",
                 sliderInput("konfniv", "Konfidenzniveau", min = 0, max = 1, step = .001, value = .95),
                 h5("P-Level-Approach"),
                 h3(htmlOutput("t")),
                 h5(htmlOutput("pValue")),
                 width = 6,
                 plotOutput("distPlot")
       ),
       sidebarPanel(
         width = 3,
         h5(strong("Nach Chlorwaschung")),
         numericInput("mortWoCl", "Todesfälle der Frauen", value = "60", width = 1000),
         numericInput("stichWoCl", "Stichpropengröße der Patientinnen", value = "100", width = 1000),
         br(),br(),br(),
         sliderInput("nachChlor", "Jahr in Klinik 2",min=1847,max=1852,step = 1,value=1847,sep="")
       ),
    ),
    tabPanel("Historische Daten",
             mainPanel(
               h3("Die historischen Daten von Semmelweis werden hier nochmal auf unterschiedliche Weise geplottet"),
               br(),br(),
                       tabsetPanel(id='tabset',
                           tabPanel("Vor/Nach Chlorwaschung vergleich",
                                            br(),br(),br(),
                                            plotlyOutput("lineClPlot")),
                          tabPanel("Bar-Graph Klinik 1",
                                   br(),br(),br(),
                                   plotlyOutput("barHistPlot")),
                          tabPanel("Linien-Graph Vergleich",
                                   br(),br(),br(),
                                   plotlyOutput("lineHistPlot"))
                          
                        ),
             )         
    ),
  )
)
  
  




# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  #Echte Daten von Semmelweis für den ersten Plot
  #Loudon I. The Tragedy of Childbed Fever, Oxford: Oxford University Press, 1997 p.94-95
  yearSlider = c(1840,1841,1842,1843,1844,1845,1846,1847,1848,1849,1850,1851,1852)
  birhts= c(2889,3036,3287,3060,3157,3492,4010,3490,3556,3858,3745,4194,4471)
  deaths= c(267,237,518,274,260,241,459,176,45,103,74,75,181)
  semmelDf <- data.frame(yearSlider,birhts,deaths)
  
  #Loudon I. The Tragedy of Childbed Fever, Oxford: Oxford University Press, 1997 p.94-95
  #Daten für die Plots im Tab "Historische"
  year <- seq(1840,1852,1)
  births1 <- c(2889,3036,3287,3060,3157,3492,4010,3490,3556,3858,3745,4194,4471)
  births2 <- c(2073,2442,2659,2739,2956,3241,3754,3306,3219,3371,3261,3395,3360)
  deaths1 <- c(267,237,518,274,260,241,459,176,45,103,74,75,181)
  deaths2 <- c(55,56,202,169,68,66,105,32,43,87,54,121,192)
  deaths1_wOCl <- c(267,237,518,274,260,241,459,NaN,NaN,NaN,NaN,NaN,NaN)/c(2889,3036,3287,3060,3157,3492,NaN,NaN,NaN,NaN,NaN,NaN)
  deaths1_wCl <- c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,176,45,103,74,75,181)/c(NaN,NaN,NaN,NaN,NaN,NaN,NaN,3490,3556,3858,3745,4194,4471)
  
  #Funktion, die das Ergebnis des prop.test() zurückgibt
  propTest <- function(mortWCl, mortWoCl, stichWCl, stichWoCl,konf) {
    return(prop.test(c(mortWCl,mortWoCl),c(stichWCl,stichWoCl),alternative = "two.sided",conf.level = konf, correct = FALSE))
  }
  
  
  
  #Funktion, um die Tode und Geburten pro Jahr aus dem Vektor herauszulesen und zurückzugeben
  getDataPerYear <- function(year) {
  todeProJahr <- semmelDf[semmelDf['yearSlider']==year, , drop=FALSE]
    return(todeProJahr)
  }
  
  #Beobachten der Slider und bei Änderungen wird der Wert für das jeweilige Jahr in die numeric Inputs eingetragen
  observeEvent(input$vorChlor, {
    updateNumericInput(session,'mortWCl',value = paste(getDataPerYear(input$vorChlor)[,3][1]))
    updateNumericInput(session,'stichWCl',value = paste(getDataPerYear(input$vorChlor)[,2][1]))
    })
  
  observeEvent(input$nachChlor, {
    updateNumericInput(session,'mortWoCl',value = paste(getDataPerYear(input$nachChlor)[,3][1]))
    updateNumericInput(session,'stichWoCl',value = paste(getDataPerYear(input$nachChlor)[,2][1]))
 })
  
  
  #https://www.rdocumentation.org/packages/gginference/versions/0.1.3/topics/ggproptest
  #https://www.itl.nist.gov/div898/handbook/eda/section3/eda3674.htm
  #Haupt-Plot auf der Startseite, der mit den dynamischen Werten der Startseite eine chisquare-Verteilung, einen kritischen Wert und eine Test-Statistic visualisiert
  output$distPlot <- renderPlot({
    #prop.test() für den p.value und als Argument für ggproptest()
    kVals <- propTest(input$mortWCl, input$mortWoCl, input$stichWCl, input$stichWoCl,input$konfniv )
    ggproptest(kVals,colaccept="green",colreject="red")
   
  })
  
  #Linienplot, der die Verhätnisse der Tode zu den Geburten in Klinik 1 und Klinik 2 von 1840 bis 1852 visualisiert
  output$lineClPlot <- renderPlotly({
    
    data <- data.frame(year,deaths1_wCl,deaths1_wOCl)
    
    fig <- plot_ly(data, x=~year,y=~deaths1_wOCl,type='scatter',mode='lines+markers' , name='Verhältnis Tode Klinik 1 ohne Chlorwaschung',line = list(color = 'red', width = 4))
    fig <- fig %>% add_trace(y=~deaths1_wCl,name='Verhältnis Tode Klinik 1 mit Chlorwaschung',line = list(color = 'green', width = 4))
    fig <- fig %>% layout(yaxis = list(title = 'Tode im Verhältnis Chlorwaschung'),xaxis=list(title='Jahr',tickmode='linear'),title='Verlauf Sterbefälle von Müttern in Klinik 1 von 1840 bis 1852 proportional zur Geburtenrate')
    
    fig 
    
  })
  
  #grouped Barplot, der die Geburten und Tode der Klinik1 von 1840-1852 visualiesiert
  output$barHistPlot <- renderPlotly({
    
    data <- data.frame(year,births1,births2,deaths1,deaths2)
    
    fig <- plot_ly(data, x=~year,y=~deaths1,type='bar',name='Tode der Mütter Klinik 1')
    fig <- fig %>% add_trace(y=~births1,name='Geburten Klinik 1')
    fig <- fig %>% layout(yaxis = list(title = 'Geburten/Tode'),xaxis=list(title='Jahr',tickmode='linear'),title='Verlauf der Sterbefälle von Müttern in der ersten Klinik von 1840 bis 1852')
  
    fig 
    
  })
  
  #Liniengraphen, die die Geburten sowie die Todesfälle von Klinik1 und Klinik2 über die Jahre 1840-1852 visualisieren
  output$lineHistPlot <- renderPlotly({
    
    data <- data.frame(year,births1,births2,deaths1,deaths2)
    
    fig <- plot_ly(data, x=~year,y=~deaths1,type='scatter',mode='lines',name='Tode der Mütter Klinik 1',line = list(color = 'rgb(205, 12, 24)', width = 4))
    fig <- fig %>% add_trace(y=~deaths2,name='Tode Klinik 2',line = list(color = 'blue', width = 4))
    fig <- fig %>% add_trace(y=~births1,name='Geburten Klinik 1',line = list(color = 'rgb(205, 12, 24)', width = 4, dash = 'dash'))
    fig <- fig %>% add_trace(y=~births2,name='Geburten Klinik 2',line = list(color = 'blue', width = 4, dash = 'dash'))
    fig <- fig %>% layout(yaxis = list(title = 'Geburten/Tode'),xaxis=list(title='Jahr',tickmode='linear'),title='Verlauf der Geburten und Sterbefälle von Müttern in der ersten und zweiten Klinik von 1840 bis 1852')
    
    fig 
    
  })
  
 
  
  #dynamische Ausgabe ob NUllhypothese erfüllt ist
  output$t <- renderText({
    kVals <- propTest(input$mortWCl, input$mortWoCl, input$stichWCl, input$stichWoCl,input$konfniv )
    ifelse((kVals$p.val <= 1-input$konfniv),
           paste("Ergebnis: Nullhypothese ","<font color=\"#FF0000\"><b>", "abgelehnt", "</b></font>"),
           paste("Ergebnis: Nullhypothese ","<font color=\"green\"><b>", "nicht abgelehnt", "</b></font>"))
  })
  
  #dynamische Ausgabe des P-Werts aus dem prop.test() und Signifikanznivau
  output$pValue <- renderText({
    kVals <- propTest(input$mortWCl, input$mortWoCl, input$stichWCl, input$stichWoCl,input$konfniv )
    paste("P-Value: ",kVals$p.val," </br> Signifikanzniveau: ", 1-input$konfniv)

  })
}
shinyApp(ui = ui, server = server)
