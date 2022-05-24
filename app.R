library(shinythemes)
library(openxlsx)

ui <- fluidPage(
  theme=shinytheme("cyborg"),
  titlePanel( h1("Data mining", align="center")),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText(h6("Zaznacz poniżej czego oczekujesz od programu ")),
      
      radioButtons("wybor_narzedzia", label = h3("Jakie narzędzie do data mining wybrać?"), 
                         choices = list("analiza tekstu" = 1, "wykrywanie oszustw lub rotacji klientów" = 2, "wyciąganie informacji z baz danych" = 3,"przygotowanie, mieszanie i analizowanie danych w jednym narzędziu"=4,"tworzenie pięknych wizualizacji"=5,"funkcjonalność przeciągaj i upuść"=6,"przeznaczone dla zespołów"=7,"automatyzacja procesów przekierowywania"=8,"użyteczne zarówno przez początkujących, jak i zaawansowanych"=9,"pomocna społeczność"=10),
                         
      ),
      verbatimTextOutput("Wybory"),
      h6("Sugerowane narzędzie to:"),
      textOutput("wybory")),
    
    mainPanel( 
      br(), br(), h5(strong("Data mining"), "stanowi proces pozyskiwania nowych informacji ze zbiorów danych. Z tego względu technika ta związana jest z odkrywaniem wiedzy w bazach danych, czyli Knowledge Discovery in Databases. Techniki data mining pozwalają uzyskać dwa rodzaje wyników – pozwalające sformułować prognozy na przyszłość lub opisujące posiadane dane."),br(),br(),
  
      img(src="data_mining.png", align="center", width=560),
      hr(),
      h3("Postępowanie w Data mining"),br(),
      h4("1. Ustanowienie celu biznesowego", br()),
      "Jest to niewątpliwie najważniejszy krok. W którym określane są istotne pytania biznesowe, pozwalające sklaryfikowac cel projektu. Przykładowymi kwestiami wartymi rozważenia mogą być pytania o rodzaj badanych danych, używane metody i programy, po kwestie związane z tym czego unikać, a na co zwracać szczególną uwagę w danych.   ", br(),
      h4("2. Przygotowanie danych", br()),
      "W tym kroku specjaliści pozyskują dane z różnych źródeł, by później zająć się procesem ich obróbki. Obróbka danych to nic innego jak przystosowanie danych do ich dalszej analizy, mając na uwadze określone w pierwszym kroku potrzebne rodzaje danych. Proces przystosowania danych jest nazwany procesem", strong("data cleaning"), "lub", strong("data cleansing"),", polega on na wyeliminowaniu błędnych typów danych, radzeniu sobie z niepełnymi danymi, czy usuwaniu zduplikowanych danych. Proces data cleaning jest niezwykle istotny, gdyż błędy na tym etapie mogą doprowadzić do fałszywych wniosków i w konsekwencji do błędnych decyzji. ", br(),
      h4("3. Opracowanie modelu i szukanie wzroców", br()),
      "Ten krok łączy się z techinkami data mining, polegającymi na wykorzystaniu narzędzi/algorytmów obejmujących zagadanienia", strong("Machine Learning"),", wykorzystujących elementy nadzorowanych i nienadzorowanych metod nauki, oraz metod uczenia głębokiego - ",strong("Deep Learning"),". Techiniki data mining pozwalają lepiej wykorzystać duże ilości danych. Jednymi z podstawowych techink jest śledzenie wzroców (np. w jakim stopniu po zapowiedziach Premiera odnośnie wzrostów oprocentowań na lokatach, zwiększy się popyt na otwieranie rachunków lokatowych), kolejnymi techinkami są klasyfikacja, wykrywanie anomalii, czy asocjacja (szukanie powiązań między zmiennymi w określonych momentach).  ", br(),
      h4("4. Ocena wyników ", br()),
      "Ostatni krok procesu data mining polega na ewaluacji otrzymanych wyników. W tym kroku proponowane są zmiany i inne działania, których implementacja powina być korzystna dla firmy.  ", br(),
      hr(),
      h4("Przykładowy interaktywny wykres dotyczący cukrzycy"),"ilustrujący poziom hemoglobiny glikowanej, której poziom poniżej 7% jest głównym celem w zwalczaniu cukrzycy. Osoby poniżej 5,7% hemoglobiny glkowanej są w normie, osoby z przedziału 5,7%-6,4% mają stan przedcukrzycowy, z kolei osoby mające ten wskaźnik powyżej poziomu 6,5% uważane są za cukrzyków. Nad wykresem znajduje się suwak określający ilość supków, co za tym idzie ich szerokość. Im mniej słupków, tym bierzemy większe przedziały hemoglobiny glikowanej badanych 390 osób.  ",
      sliderInput(
        "bins","Ilość słupków:",
        min=20,max=80,
        value=40),
      plotOutput("distPlot"), br(),
      list(uiOutput("tab1")),
      hr(),
      list(uiOutput("tab"))
      )
      )
)
  

server <- function(input, output) {
  output$wybory <- renderText({ 
    if(input$wybor_narzedzia==1 || input$wybor_narzedzia==8){
    paste("MonkeyLearn")} 
    else if (input$wybor_narzedzia==2 || input$wybor_narzedzia==10) {
      paste("RapidMiner Studio")}
    else if (input$wybor_narzedzia==3 || input$wybor_narzedzia==7 || input$wybor_narzedzia==9) {
      paste("Sisense for Cloud Data Teams")}
    else if (input$wybor_narzedzia==4) {
      paste("Alteryx Designer")}
    else if (input$wybor_narzedzia==5 || input$wybor_narzedzia==6) {
      paste("Qlik Sense")}
    else {
      paste("Zaznacz wybrane opcje w pasku z lewej strony, by zobaczyć narzędzie dostosowane do Twoich potrzeb.")
    }
    })
  
  diabetes=read.csv("diabetes.csv")
  #alternatywnie#    diabetes <- read.csv("https://query.data.world/s/jrr23dpipskivzzakjlh3l4niclg7l", header=TRUE, stringsAsFactors=FALSE);
  
  output$distPlot <- renderPlot({
    x<-diabetes$glyhb
   bins<-seq(min(x), max(x),length.out=input$bins)
   hist(diabetes$glyhb, breaks=bins, freq=FALSE,xlim=range(2,bins),yaxp=c(0,1,10),xaxp=c(2,17,15), col='grey',xlab="Poziom hemoglobiny glikowanej w %",ylab="Częstliwość występowania", main="Poziom zagrożenia cukrzycowego")
   
   })
  
  
  url1 <- a("Diabetics_prediction", href="https://data.world/subeen/diabeticsprediction/workspace/data-dictionary")
  output$tab1 <- renderUI({
    tagList("Powyzszy wykres został opracowany na podstawie danych ze strony", url1)
  })
  
  url <- a("Data mining", href="https://en.wikipedia.org/wiki/Data_mining")
  output$tab <- renderUI({
    tagList("Kliknij", url,", by dowiedzieć się więcej!")
  })
}


shinyApp(ui=ui, server=server)

