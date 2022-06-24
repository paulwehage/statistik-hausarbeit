#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)
library(dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Semmelweis Hypothesentest mittels Konfidenzintervallen"),
  
  h4("Nullhypothese: Die Chlorwaschungen als Hygienemaßnahmen sorgen für eine geringere Müttersterblichkeit"),
  
  p("Ignaz Semmelweis (1818 - 1865) war ein ungarisch-österreichischer Chirurg und Geburtshelfer in der Wiener Entbindungsklinik, welche in zwei Unterkliniken unterteilt war -  in der ersten arbeiteten Studierende und Ärzte und in der zweiten nur Hebammen. Mit der Zeit fiel ihn auf, dass in der ersten Klinik weitaus mehr Mütter am Kindbettfieber nach der Entbindung starben als in der Zweiten und er führte es auf Fehler in Routine-Maßnahmen zurück. 
    Er beobachtete, dass die Mitarbeiter der ersten Klinik vor Entbindungen oftmals die Leichen der am Vortag verstorbenen Mütter obduzierten und dann ohne Hygienemaßnahmen weiter zur Entbindung gingen, wohingegen die Hebammen der zweiten Klinik keinen Kontakt zu Leichen hatten. Zwischen diesen Fakten stellte er eine Korrellation fest und das noch der Entdeckung der Rolle von Bakterien bei Krankheiten. Semmelweis Konsequenz darauf war, dass er die 
    Mitarbeiter der ersten Klinik dazu verpflichtete, vor einer Entbindung ihre Hände und das Werkzeug mit Chlorkalk zu desinfizieren. Das Ergebnis war eindeutig - von fast 10% Sterblichkeit auf 1,2%, was sogar die Hebammen schlug."),
  p("Die Zahlen und Vorgehensweisen hat er in seiner Studie 'The Etiology, Concept, and Prophylaxis of Childbed Fever' akribisch dokumentiert und der Fall gilt damit heute als der erste Fall evidenzbasierter Medizin und als Musterbeispiel wissenschaftlicher Hypothesen."),
  p("Auch wenn Semmelweis sehr vorbidlich gearbeitet hat, hätte Ihm ein interaktives Interface sicherlich geholfen seine Hypothese der Chlorwaschung zu unterstüzen."),
  
  br(),br(),br(),
  
  sidebarPanel(
    width = 3,
    h5(strong("Klinik 1")),
    numericInput("mortWCl", "Todesfälle der Frauen", value = "61", width = 1000),
    numericInput("stichWCl", "Stichpropengröße der Patientinnen", value = "170", width = 1000),
  ),
  mainPanel(align = "center",
    sliderInput("konfniv", "Konfidenzniveau", min = 0, max = 1, step = .001, value = .95),
    h3(htmlOutput("t")),
    width = 6,
    plotOutput("distPlot"),
    
    sliderInput(sep="","jahr", "Echte Fallzahlen nach Jahre", min = 1847, max = 1852, step = 1, value = 1847),
  ),
  sidebarPanel(
    width = 3,
    h5(strong("Klinik 2")),
    numericInput("mortWoCl", "Todesfälle der Frauen", value = "60", width = 1000),
    numericInput("stichWoCl", "Stichpropengröße der Patientinnen", value = "150", width = 1000),
  ),
  )




# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  ## Berechnung des Z-Werts
  #this returns a value that can then be used to determine whether we accept h0 or not
  
  propTest = function(mortWCl, mortWoCl, stichWCl, stichWoCl,konf) {
    return(prop.test(c(mortWCl,mortWoCl),c(stichWCl,stichWoCl),alternative = "two.sided",conf.level = konf, correct = FALSE))
  }
  
  zValF = function(mortWCl, mortWoCl, stichWCl, stichWoCl) {
    pVal <- c(mortWCl/stichWCl,mortWoCl/stichWoCl)
    props <- (mortWCl+mortWoCl)/(stichWCl+stichWoCl)
    q <- 1-props
    return((pVal[1]-pVal[2])/sqrt((props*q)/stichWCl+(props*q)/stichWoCl))
  }
  
  conf_value = function(conflevel){
    return(qnorm(conflevel, 0, 1)); 
  }
  
  ## Überprüfung der Null-Hypothese
  #function to determine conf.level result, i.e. 1.64 for 95%. and then return t/f depending on if it's gt/lt result from compare
  res = function(conflevel, compareValue){
    tmp = conf_value(conflevel)
    return(ifelse(tmp >= compareValue & -tmp <= compareValue, TRUE, FALSE));
  }
  
  #Echte Daten von Semmelweis
  year = c(1841,1842,1843,1844,1845,1846,1847,1848,1849,1850,1851,1852,1841,1842,1843,1844,1845,1846,1847,1848,1849,1850,1851,1852)
  birhts= c(3036,3287,3060,3157,3492,4010,3490,3556,3858,3745,4194,4471,2442,2659,2739,2956,3241,3754,3306,3219,3371,3261,3395,3360)
  deaths= c(237,518,274,260,241,459,176,45,103,74,75,181,86,202,164,68,66,105,32,43,87,54,121,192)
  semmelDf <- data.frame(year,birhts,deaths)
  

  getDataPerYear <- function(year) {
    todeProJahr <- semmelDf[semmelDf['year']==year, , drop=FALSE]
    return(todeProJahr)
  }
  

  
 # observeEvent(input$jahr, {
  #  updateNumericInput(session,'mortWCl',value = paste(getDataPerYear(input$jahr)[,3][1]))
  #  updateNumericInput(session,'mortWoCl',value = paste(getDataPerYear(input$jahr)[,3][2]))
  #  updateNumericInput(session,'stichWCl',value = paste(getDataPerYear(input$jahr)[,2][1]))
  #  updateNumericInput(session,'stichWoCl',value = paste(getDataPerYear(input$jahr)[,2][2]))
  #  })
  
  
  output$distPlot <- renderPlot({
    
    #zVal <- qnorm(input$konfniv)
    kVals <- propTest(input$mortWCl, input$mortWoCl, input$stichWCl, input$stichWoCl,input$konfniv )
    zVal <- zValF(input$mortWCl, input$mortWoCl, input$stichWCl, input$stichWoCl)
    ##dynamische Anpassung des Plots
    #https://stackoverflow.com/questions/10543443/how-to-draw-a-standard-normal-distribution-in-r
    x <- seq(-1,1, length=1000) 
    y <- dnorm(x, mean=0, sd=1)
    plot(x, y, type="l", lwd=1)
    legend("topright", legend = c("Z", "Grenzen"), col = 1:2, pch = 19, bty = "o")
    #https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/abline
    abline(v=kVals$conf.int[1], col="red", lwd=3)
    abline(v=kVals$conf.int[2], col="red", lwd=3)
    abline(v=zVal, lwd=3)
  })
  
  ##dynamische Ausgabe ob NUllhypothese erfüllt ist
  output$t <- renderText({
    kVals <- propTest(input$mortWCl, input$mortWoCl, input$stichWCl, input$stichWoCl,input$konfniv )
    zVal <- zValF(input$mortWCl, input$mortWoCl, input$stichWCl, input$stichWoCl)
    ifelse(between(zVal,kVals$conf.int[1],kVals$conf.int[2]), 
           paste("Ergebnis: Nullhypothese ","<font color=\"green\"><b>", "nicht abgelehnt", "</b></font>"),
           paste("Ergebnis: Nullhypothese ","<font color=\"#FF0000\"><b>", "abgelehnt", "</b></font>"))
  })
}

shinyApp(ui = ui, server = server)
