library(shiny)
library(ggplot2)

lyftdf1 <- read.csv("lyft_data.csv", stringsAsFactors = FALSE)

ui <- shinyUI(fluidPage(
  titlePanel("Twitter Data Analysis"),   # Application title
  navbarPage(title = "Content",
             tabPanel("Brand",
                      h1("Lyft"),
                      hr(),
                      h3("A ride whenever you need one"),
                      splitLayout(img(src = "logo.png"))),
             tabPanel("Sentiment Analysis",
                      plotOutput("sentiment_analysis")),  
             tabPanel("Word Cloud",
                      plotOutput("wordcloud_withoutsentiment"),
                      plotOutput("wordcloud_withsentiment")),
             tabPanel("Map",
                      plotOutput("map_data")) #end of tabPanel
  ) #end of navbar
) #end fluid page
) #end shiny UI




# Define server logic required to draw a histogram
server <- function(input, output) {
  output$sentiment_analysis<-renderPlot({
    lyftdf1_text %>%
      inner_join(get_sentiments("bing")) %>%
      dplyr::count(word, sentiment, sort = TRUE) %>%
      ungroup()
    
    bing_word_counts %>%
      group_by(sentiment) %>%
      top_n(10) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()
  })  
  
  output$wordcloud_withoutsentiment<-renderPlot({
    wordcloud_withoutsentiment <- lyftdf1_text %>%
      anti_join(stop_words) %>%
      dplyr::count(word) %>%
      with(wordcloud(word, n, max.words = 100))
  })
  
  output$wordcloud_withsentiment<-renderPlot({  
    wordcloud_withsentiment <- lyftdf1_text %>%
      inner_join(get_sentiments("bing")) %>%
      dplyr::count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                       max.words = 100,scale = c(4,.5),title.size=1.5)
  })
  
  output$map_data<-renderPlot({  
    ggplot(map.data) + 
      geom_map(aes(map_id = region),  
               map = map.data,  
               fill = "light grey",             
               color = "grey20", size = 0.25) + 
      expand_limits(x = map.data$long, y = map.data$lat) +            
      theme(axis.line = element_blank(),  
            axis.text = element_blank(),  
            axis.ticks = element_blank(),                     
            axis.title = element_blank(),  
            panel.background = element_blank(),  
            panel.border = element_blank(),                     
            panel.grid.major = element_blank(), 
            plot.background = element_blank(),                     
            plot.margin = unit(0 * c( -1.5, -1.5, -1.5, -1.5), "lines")) +  
      geom_point(data = netpoints,             
                 aes(x = x, y = y), size = 1,  
                 alpha = 1/5, color = "red")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

