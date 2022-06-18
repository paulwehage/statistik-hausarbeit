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
  
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Konfidenzniveau",
                  min = 0,
                  max = 1,
                  value = 0.95),
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

#Dies ist ein Test