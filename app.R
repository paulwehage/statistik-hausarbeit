#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/


library(shiny)

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
    h5(strong("Mit Chlorwaschung")),
    textInput("mortWCl", "Todesfälle der Frauen", value = "300", width = 1000, placeholder = "Todesfälle"),
    textInput("stdWCl", "Standardabweichung der Todesfälle", value = "3", width = 1000, placeholder = "Standardabweichung"),
    textInput("stichWCl", "Stichpropengröße der Patientinnen", value = "2000", width = 1000, placeholder = "Stichprobengröße"),
  ),
  mainPanel(align = "center",
    sliderInput("konfniv", "Konfidenzniveau", min = 0, max = 1, step = .001, value = .95),
    width = 6,
    plotOutput("distPlot"),
    sliderInput(sep="","jahr", "Echte Fallzahlen nach Jahre", min = 1841, max = 1852, step = 1, value = 1841),
  ),
  sidebarPanel(
    width = 3,
    h5(strong("Ohne Chlorwaschung")),
    textInput("mortWoCl", "Todesfälle der Frauen", value = "300", width = 1000, placeholder = "Todesfälle"),
    textInput("stdWoCl", "Standardabweichung der Todesfälle", value = "2", width = 1000, placeholder = "Standardabweichung"),
    textInput("stichWoCl", "Stichpropengröße der Patientinnen", value = "2000", width = 1000, placeholder = "Stichprobengröße"),
  ),
  )




# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  #Echte Daten von Semmelweis
  year = c(1841,1842,1843,1844,1845,1846,1847,1848,1849,1850,1851,1852,1841,1842,1843,1844,1845,1846,1847,1848,1849,1850,1851,1852)
  birhts= c(3036,3287,3060,3157,3492,4010,3490,3556,3858,3745,4194,4471,2442,2659,2739,2956,3241,3754,3306,3219,3371,3261,3395,3360)
  deaths= c(237,518,274,260,241,459,176,45,103,74,75,181,86,202,164,68,66,105,32,43,87,54,121,192)
  semmelDf <- data.frame(year,birhts,deaths)
  

  getDeathsPerYear <- function(year) {
    todeProJahr <- semmelDf[semmelDf['year']==year, , drop=FALSE]
    return(todeProJahr)
  }
  
  observeEvent(input$jahr, {
    updateTextInput(session,'mortWCl',value = paste(getDeathsPerYear(input$jahr)[,3][1]))
    updateTextInput(session,'mortWoCl',value = paste(getDeathsPerYear(input$jahr)[,3][2]))
    updateTextInput(session,'stichWCl',value = paste(getDeathsPerYear(input$jahr)[,2][1]))
    updateTextInput(session,'stichWoCl',value = paste(getDeathsPerYear(input$jahr)[,2][2]))
    })
  
  print(getDeathsPerYear(1847)[,2][1])
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    data.frame(
      Name = c("Konfidenzniveau",
               "Sterbezahlen mit Chlorwaschungen",
               "Avg. Wartezeit Selbstbedienung",
               "Varianz normale Kasse in Minuten",
               "Varianz Selbstbedienung in Minuten",
               "Stichprobengr????e normale Kasse",
               "Stichprobengr????e Selbstbedienung"),
      Value = as.character(c(input$konfniv,
                             input$mortWCl,
                             input$mortWoCl,
                             input$stdWCl,
                             input$stdWoCl,
                             input$StichWCl,
                             input$stichWoCl)),
      stringsAsFactors = FALSE)
  })
  
  #https://stackoverflow.com/questions/10543443/how-to-draw-a-standard-normal-distribution-in-r
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    sliderValues()
  })
  
  output$plot <- renderPlot({
    z_val <- compare(input$mortWCl, input$mortWoCl, input$stdWCl**2, input$stdWoCl**2, input$StichWCl, input$stichWoCl)
    ##dynamische Anpassung des Plots
    #https://stackoverflow.com/questions/10543443/how-to-draw-a-standard-normal-distribution-in-r
    x <- seq(ifelse(z_val < -4, z_val-2, -4), ifelse(z_val > 4, z_val+2, 4), length=1000) 
    y <- dnorm(x, mean=0, sd=1)
    plot(x, y, type="l", lwd=1)
    legend("topright", legend = c("Z", "Grenzen"), col = 1:2, pch = 19, bty = "o")
    #https://www.rdocumentation.org/packages/graphics/versions/3.6.2/topics/abline
    cvalue = conf_value(input$konfniv)
    abline(v=-cvalue, col="red", lwd=3)
    abline(v=cvalue, col="red", lwd=3)
    abline(v=z_val, lwd=3)
  })
  
  ##dynamische Ausgabe ob NUllhypothese erfüllt ist
  output$t <- renderText({
    ifelse(res(input$konfniv, compare(input$mortWCl, input$mortWoCl, input$stdWCl**2, input$stdWoCl**2, input$StichWCl, input$stichWoCl)), 
           paste("Ergebnis: Nullhypothese ","<font color=\"green\"><b>", "nicht abgelehnt", "</b></font>"), 
           paste("Ergebnis: Nullhypothese ","<font color=\"#FF0000\"><b>", "abgelehnt", "</b></font>"))
  })
}
shinyApp(ui = ui, server = server)
