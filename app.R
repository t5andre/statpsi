

library(shiny)
library(ggplot2)
library(patchwork)

# Define UI
ui <- fluidPage(
    
    # Application title
    titlePanel("Teorema central do limite"),
    
    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            radioButtons("dist",
                         "Formato da distribuição",
                         choices = c("Normal","Assimétrica")),
            sliderInput("n",
                        "Tamanho amostral",
                        min = 1,
                        max = 50,
                        value = 5),
            sliderInput("qtd",
                        "Número de amostras",
                        min = 1,
                        max = 500,
                        value = 5)
        ),
        mainPanel(
            plotOutput("popDist"),
            plotOutput("amostDist")
        )
    )
)


server <- function(input, output) {
    output$popDist <- renderPlot({
        set.seed(19)
        ifelse(input$dist == "Assimétrica",
                        pop <- sample(sort(rnorm(mean = 150, sd = 30, n = 19000))[6000:19000],19000, replace = TRUE),
                        pop <- rnorm(mean = 200, sd = 30, n = 19000))
        ggplot(data.frame(pop)) + geom_histogram(aes(x = data.frame(pop)[,1]), bins = 50) +
            theme_minimal() + labs(title = "Distribuição populacional",
                                   y = "", x = "Tempo de reação em ms") +
            theme(axis.text.y = element_blank(), panel.grid = element_blank()) +
            scale_x_continuous(breaks = c(100,150,200,250,300),
                               limits = c(0,350))+
            scale_y_continuous(limits = c(0,4000))
    })
      output$amostDist <- renderPlot({
          set.seed(19)
          ifelse(input$dist == "Assimétrica",
                 pop <- sample(sort(rnorm(mean = 150, sd = 30, n = 19000))[6000:19000],19000, replace = TRUE),
                 pop <- rnorm(mean = 200, sd = 30, n = 19000))
          amost_m <- data.frame()
        for (i in 1:input$qtd){
            amost_m <- rbind(amost_m,mean(sample(pop, size = input$n, replace = FALSE)))
        }
        ggplot(amost_m) + geom_histogram(aes(x = amost_m[,1]), bins = 50) +
            theme_minimal() + labs(title = "Distribuição das médias amostrais",
                                   y = "", x = "Tempo de reação em ms") +
            theme(axis.text.y = element_blank(), panel.grid = element_blank()) +
            scale_x_continuous(breaks = c(100,150,200,250,300),
                               limits = c(0,350)) +
            scale_y_continuous(limits = c(0,500))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
