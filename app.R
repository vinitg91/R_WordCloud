#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(devtools)
library(wordcloud2)
library(tm)
library(SnowballC)
library(wordcloud)

# Define UI for application that draws a histogram
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      
      radioButtons("fType","Select type of file to load",
                   choices = list("Excel","CSV"),selected = "Excel"),
      fileInput(inputId = "TextFile",label = "Choose File"),
      #fileInput(inputId = "Image",label = "Choose Image"),
      textInput("Exclude","Exclude words from cloud"),
      textInput("Shape","Shape of cloud"),
      h6("circle (default), cardioid , diamond (alias of square), 
         triangle-forward, triangle, pentagon, and star"),
      actionButton("load","Generate")
    ),
    mainPanel(
      tabsetPanel(tabPanel(title = "Data",dataTableOutput("table")),
                  tabPanel(title = "Summary",tableOutput("summ")),
                  tabPanel(title = "Wordcloud",
                           wordcloud2Output("wordcloud"))
                  #,tabPanel(title = "Test",textOutput("test"))
                  )
    )
  ) 
)

options(shiny.maxRequestSize = 100*1024^2)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  data1 = reactive({
        file1 = input$TextFile
        if(is.null(file1)){return()}
        if(input$fType=="Excel")
              f = read.xlsx(file1$datapath,sheetIndex = 1)
        if(input$fType=="CSV")
              f = read.table(file = file1$datapath,header = T,sep = ",")
        return(f)
  }) 
  
  wc = eventReactive(input$load,{
    if(is.null(data1())){return()}
  
    t = data1()$letlifein #store the text column in variable 't'
    t = as.character(t)
    #Make sure everything is ASCII. Remove everything that is non-ASCII
    Encoding(t)="latin1" 
    iconv(t,"latin1","ASCII",sub="")
    
    corp = Corpus(VectorSource(t)) #create a corpus from text
    #function to replace a specific pattern with space
    toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
    
    corp=tm_map(corp,toSpace,"/")
    corp=tm_map(corp,toSpace,"@")
    corp=tm_map(corp,toSpace,"\\|")
    
    excl = input$Exclude
    remWords = vector()
    remWords = unlist(strsplit(excl,","))
    
    corp=tm_map(corp,content_transformer(tolower)) #convert everything to lower case
    corp=tm_map(corp,removeNumbers) #remove numbers from text
    corp=tm_map(corp,removeWords,c(stopwords("english"),remWords)) #remove specific words
    corp=tm_map(corp,removePunctuation) #remove punctuation
    corp=tm_map(corp,stripWhitespace) #strip any extra white space
    #corp=tm_map(corp,stemDocument) #stem the words in the corpus
    
    
    dtm = TermDocumentMatrix(corp) #create a term document matrix from corpus
    m = as.matrix(dtm) #convert it into matrix
    v = sort(rowSums(m),decreasing=TRUE) #sort in the decreasing order of freq
    d = data.frame(word = names(v),freq=v) #convert it into data frame
    d
  })
  
  output$table = renderDataTable({
    if(is.null(data1())){return()}
    data1()
  }) 
  
  output$summ = renderTable({
    if(is.null(wc())){return()}
    wc()
  })
  
  output$wordcloud = renderWordcloud2({
    if(is.null(wc())){return()}
    image1 = input$Image
    wordcloud2(data = wc() ,size = 1,minRotation = -pi/6, maxRotation = -pi/6,
               rotateRatio = 1, shape = input$Shape )
  })
  
  
  output$test = renderText({
    excl = input$Exclude
    remWords = vector()
    remWords = unlist(strsplit(excl,","))
    return(remWords)
  })

  

}

# Run the application 
shinyApp(ui = ui, server = server)

