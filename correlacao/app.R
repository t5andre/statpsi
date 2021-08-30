# Nome: Entendendo correlações
# Autor: Gabriel dos Reis Rodrigues
# June, 2021
# Last update: 25/06/2021
# ----------------------------------------
# Initial loading ====
if(!require("faux"))
    install.packages("faux"); library(faux)
if(!require("ggplot2"))
    install.packages("ggplot2"); library(ggplot2)
if(!require("shiny"))
    install.packages("shiny"); library(shiny)

# Functions ====
# Minimalist theme
project_theme <- 
    theme(legend.position = "none",
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          panel.border = element_blank(),
          plot.title = element_blank(),
          plot.subtitle = element_blank(),
          axis.text = element_blank(),
          axis.title = element_text(size = 14))

# Correlation plot function
corr_plot <- function(corr = 0,
                      sample = 1000,
                      line = T){
    set.seed(42)
    
    df <- rnorm_multi(n = sample, 
                      mu = c(20, 20),
                      sd = c(5, 5),
                      r = corr, 
                      varnames = c("X", "Y"),
                      empirical = T)
    
    if(line == T){
        ggplot(df, aes(x = X, y = Y)) +
            
            # Points
            geom_point(alpha = 0.5, position = 'jitter', color = "#006495") +
            
            # Line
            stat_smooth(method = "lm", se = F, color = "#011F5a", size = 1.2) +
            
            # Themes
            theme_classic() + project_theme} else{
                
                ggplot(df, aes(x = X, y = Y)) +
                    
                    # Points
                    geom_point(alpha = 0.5, position = 'jitter', color = "#006495") + # padronizar cor
                    
                    # Themes
                    theme_classic() + project_theme}
    
}

# Define UI for application ====
ui <- fluidPage(

    # Application title
    titlePanel("Visualizando correlações"),

    # Sidebar with inputs ====
    sidebarLayout(
        sidebarPanel(
            sliderInput("corr",
                        "Especifique a magnitude da correlação",
                        min = -.99, #acho que fica menos assustador com 2 dígitos
                        max = .99,
                        value = 0,
                        step = 0.01),
            sliderInput("samplesize",
                        "Mude o tamanho amostral",
                        min = 3,
                        max = 10000,
                        value = 1000,
                        step = 1,
                        sep = "."),
            selectInput("plotline",
                        "Quer ver a linha de regressão?",
                        c("Sim" = T, "Não" = F)),
            
            # Conditional Panels ====
            conditionalPanel("input.corr < 0.1 &
                             input.corr > -0.1",
                             h5(strong("Parece que X e Y NÃO ESTÃO 
                                       correlacionados.")),
                             h6("Baseando-se em Cohen (1988), isso acontece
                             quando o valor absoluto da correlação é menor que
                             0.1. Se estamos medindo a correlação com o r de
                             Pearson, uma correlação de no máximo 0.1 indica
                             que ambas as variáveis compartilham menos de 1% 
                             de sua variância."),
                             
                             h6("Conseguimos o valor de menos de 1% multiplicando
                             o valor da correlação r por ele mesmo (r*r = r²). 
                             Assim, se r = 0.09, r² = 0.09 * 0.09 = 0.0081. 
                             Isso equivale a 0.81% de variância compartilhada, 
                             o que é menos de 1%. Poderíamos dizer quase com
                             certeza de que X e Y são bastante diferentes.")),
            
            conditionalPanel("input.corr >= 0.1 & input.corr < 0.3|
                             input.corr <= -0.1 & input.corr > -0.3",
                             h5(strong('Parece haver uma correlação FRACA entre
                                       X e Y.')),
                             h6("Baseando-se em Cohen (1988), isso acontece
                             quando o valor absoluto da correlação é maior ou
                             igual a 0.1. Se estamos medindo a correlação com o r de
                             Pearson, uma correlação de no mínimo 0.1 indica
                             que ambas as variáveis compartilham pelo menos 1% 
                             de sua variância."),
                                       
                             h6("Conseguimos o valor de pelo menos 1% 
                             multiplicando o valor da correlação r por ele mesmo
                             (r*r = r²). Assim, se 
                             r = 0.1, r² = 0.1 * 0.1 = 0.01. Deve ser óbvio que
                             o valor de 1% representa uma porcentagem muito
                             pequena.")),
            
            conditionalPanel("input.corr >= 0.3 & input.corr < 0.5|
                             input.corr <= -0.3 & input.corr > -0.5",
                             h5(strong('Parece haver uma correlação MODERADA
                                       entre X e Y.')),
                             h6("Baseando-se em Cohen (1988), isso acontece
                             quando o valor absoluto da correlação é maior ou
                             igual a 0.3. Se estamos medindo a correlação com o 
                             r de Pearson, uma correlação de no mínimo 0.3 
                             indica que ambas as variáveis compartilham pelo 
                             menos 9% de sua variância."),
                                       
                             h6("Conseguimos o valor de pelo menos 9% 
                             multiplicando o valor da correlação r por ele mesmo
                             (r*r = r²). Assim, se 
                             r = 0.3, r² = 0.3 * 0.3 = 0.09.")),
            
            conditionalPanel("input.corr >= 0.5 & input.corr < 0.7|
                             input.corr <= -0.5 & input.corr > -0.7",
                             h5(strong('Parece haver uma correlação FORTE entre
                                       X e Y.')),
                             h6("Baseando-se em Cohen (1988), isso acontece
                             quando o valor absoluto da correlação é maior ou
                             igual a 0.5. Se estamos medindo a correlação com o 
                             r de Pearson, uma correlação de no mínimo 0.5 
                             indica que ambas as variáveis compartilham pelo 
                             menos 25% de sua variância."),
                             
                             h6("Conseguimos o valor de pelo menos 25% 
                             multiplicando o valor da correlação r por ele mesmo
                             (r*r = r²). Assim, se 
                             r = 0.5, r² = 0.5 * 0.5 = 0.25.")),
            
            conditionalPanel("input.corr >= 0.7 & input.corr < 0.9 |
                             input.corr <= -0.7 & input.corr > -0.9 ",
                             h5(strong('Parece haver uma correlação FORTE entre
                                       X e Y.')),
                             h6("Baseando-se em Cohen (1988), isso acontece
                             quando o valor absoluto da correlação é maior ou
                             igual a 0.5. Se estamos medindo a correlação com o 
                             r de Pearson, uma correlação de no mínimo 0.5 
                             indica que ambas as variáveis compartilham pelo 
                             menos 25% de sua variância."),
                             
                             h6("Conseguimos o valor de pelo menos 25% 
                             multiplicando o valor da correlação r por ele mesmo
                             (r*r = r²). Assim, se 
                             r = 0.5, r² = 0.5 * 0.5 = 0.25."),
                             
                             h6("Quando a correlação for maior ou igual a 0.7,
                                quase metade da variância entre as duas 
                                variáveis está sendo compartilhada, dado que
                                r² = 0.49, o que é equivalente a 49% de 
                                variância compartilhada. Talvez seja o momento
                                de se pensar se ambas as variáveis possuem uma
                                causa comum ou talvez se uma causa a outra.")),
            
            conditionalPanel("input.corr >= 0.9 |
                             input.corr <= -0.9",
                             h5(strong('Parece haver uma correlação FORTE entre
                                       X e Y.')),
                             h6("Baseando-se em Cohen (1988), isso acontece
                             quando o valor absoluto da correlação é maior ou
                             igual a 0.5. Se estamos medindo a correlação com o 
                             r de Pearson, uma correlação de no mínimo 0.5 
                             indica que ambas as variáveis compartilham pelo 
                             menos 25% de sua variância."),
                             
                             h6("Conseguimos o valor de pelo menos 25% 
                             multiplicando o valor da correlação r por ele mesmo
                             (r*r = r²). Assim, se 
                             r = 0.5, r² = 0.5 * 0.5 = 0.25."),
                             
                             h6("Quando a correlação for maior ou igual a 0.9,
                                quase toda a variância entre as duas 
                                variáveis está sendo compartilhada, dado que
                                r² = 0.81, o que é equivalente a 81% de 
                                variância compartilhada. Parece que ambas as 
                                variáveis possuem uma causa comum ou uma está
                                causando a outra.")),
            
            hr(),
            
            h6(strong("Referência")), #tirei fora do conditional por ser sempre igual
            
            h6("Cohen, J. (1988). Statistical power
                             analysis for the behavioral sciences (2nd edition).
                                      Lawrence Erlbaum."),
            
            hr(),
            strong(a(href = "https://forms.gle/5HwvjF6z54MQcv8c6",
                     "Nos conte o que você achou dessa visualização!")),
            hr(),       
            "Elaborado para a disciplina de Estatística Aplicada
            à Psicologia da Universidade Federal do Rio Grande do Sul.",
            br(),
            a(href = "https://github.com/t5andre/statpsi/tree/main/correlacao",
              "Código")
            
            ),

        
        # Show the correlation plot ====
        mainPanel(
           plotOutput("corrplot")
        )
    )
)

# Define server logic ====
server <- function(input, output) {

    output$corrplot <- renderPlot({
        corr_plot(corr = input$corr,
                  sample = input$samplesize,
                  line = input$plotline)
    })
}


# Run the application 
shinyApp(ui = ui, server = server)