library(shiny)
library(ggplot2)
options(OutDec = ",")
<<<<<<< HEAD

app_theme <- theme(axis.text.y = element_blank(),
                   panel.grid = element_blank(),
                   text = element_text(size = 18),
                   axis.text.x = element_text(size = 10),
                   axis.line.y = element_blank(),
                   axis.ticks.y = element_blank(),
                   axis.title = element_blank()) 

# fun pra parametros do gráfico conforme opções de cálculo
plot_params <- function(media = 0,
                        sd = 1,
                        calc = NULL,
                        xlim1 = NULL,
                        xlim2 = NULL,
                        lim_inf = NULL,
                        lim_sup = NULL,
                        escore = NULL,
                        ic_perc = NULL)
    {
        {if (calc == "Probabilidade abaixo de um escore") {
            xlim1 = -4*sd+media
            xlim2 = escore}
            
        else if(calc == "Probabilidade acima de um escore") {
            xlim1 = escore
            xlim2 = 4*sd+media}
        
        else if(calc == "Probabilidade entre dois valores") {
            xlim1 = lim_inf
            xlim2 = lim_sup}
        
        else if(calc == "Limites para um intervalo de confiança") {
            xlim1 = media-sd*qnorm((100-ic_perc)/200)
            xlim2 = media-sd*qnorm((100+ic_perc)/200)}
            
          }
    
    
   ggplot() + 
       stat_function(geom = "area",
                     fun = dnorm,
                     args = list(mean = media,
                                 sd = sd),
                     color = "#eb968e",
                     alpha = 0.2,
                     xlim = c(media-sd*4,
                              media+sd*4)) +
   stat_function(fun = dnorm,
                    args = list(mean = media,
                                sd = sd),
                    xlim = c(xlim1,
                             xlim2),
                    geom = "area",
                    fill = "#006495ff",
                    alpha = .3, 
                    color = "#006495") +
        {if (calc %in% c("Probabilidade abaixo de um escore",
                         "Probabilidade acima de um escore"))
        {geom_vline(aes(xintercept = escore), linetype = "dashed")}
            
            else if(calc == "Probabilidade entre dois valores")
            {geom_vline(aes(xintercept = c(lim_inf,lim_sup)),
                        linetype = "dashed")}
            
            else if(calc == "Limites para um intervalo de confiança")
            {geom_vline(aes(xintercept = c(media-sd*qnorm((100-ic_perc)/200),
                                           media-sd*qnorm((100+ic_perc)/200))),
                        linetype = "dashed")}
             }
        
    }
=======
>>>>>>> 119055378c56178b59e77dade3260dcbc83b813e

ui <- fluidPage(
    
    # Título
    titlePanel(h1("Curva normal", align = "center"),
               windowTitle = "Curva Normal"),
    
    # Painel
    sidebarLayout(
        sidebarPanel(
               radioButtons("calc",
                            "Cálculo",
                            choices = c("Probabilidade abaixo de um escore",
                                        "Probabilidade acima de um escore",
                                        "Probabilidade entre dois valores",
                                        "Limites para um intervalo de confiança")),
               numericInput("media",
                            "Média",
                            0,
                            min = -Inf,
                            max = Inf),
               numericInput("dp",
                            "Desvio-padrão",
                            1,
                            min = 0,
                            max = Inf,
                            step = 0.01),
               conditionalPanel("input.calc == 'Probabilidade abaixo de um escore' || input.calc == 'Probabilidade acima de um escore' ",
                                numericInput("escore",
                                             "Escore",
                                             0,
                                             min = -Inf,
                                             max = Inf)),
               conditionalPanel("input.calc == 'Probabilidade entre dois valores'",
               numericInput("lim_inf",
                            "Limite inferior do intervalo",
                            -1,
                            min = -Inf,
                            max = Inf),
               numericInput("lim_sup",
                            "Limite superior do intervalo",
                            1,
                            min = -Inf,
                            max = Inf)),
               conditionalPanel("input.calc == 'Limites para um intervalo de confiança'",
               numericInput("ic_perc",
                            "Percentual do Intervalo de confiança",
                            95,
                            min = 1,
                            max = 99)),
               hr(),
               strong(a(href = "https://forms.gle/1CbWRrAeHbUkQ5bY6",
                        "Nos conte o que você achou dessa visualização!")),
               hr(),       
               "Elaborado para a disciplina de Estatística Aplicada
            à Psicologia da Universidade Federal do Rio Grande do Sul.",
               br(),
               a(href = "https://github.com/t5andre/statpsi/tree/main/tabela_normal",
                 "Código")
               
               ),
    mainPanel(
        h4(textOutput("resultado")),
        plotOutput("probDist")
        )
    )
)

server <- function(input, output, session) {
    #não estou satisfeito com essa solução pra atualizar valores com base
    #em alterações da média
    observeEvent((input$media),{
        #abs() pra evitar DPs negativos
        if (input$media < 4){
        updateNumericInput(session, "dp", value = 1)
<<<<<<< HEAD
        updateNumericInput(session, "escore", value = round(input$media,0))
=======
        updateNumericInput(session, "escore", value = integer(input$media))
>>>>>>> 119055378c56178b59e77dade3260dcbc83b813e
        updateNumericInput(session, "lim_inf", value = input$media-2)
        updateNumericInput(session, "lim_sup", value = input$media+2)}
       
         else{
            updateNumericInput(session, "dp", value = round(abs(input$media)/6,0),min = 1)
<<<<<<< HEAD
            updateNumericInput(session, "escore", value = round(input$media,0) )
=======
            updateNumericInput(session, "escore", value = integer(input$media) )
>>>>>>> 119055378c56178b59e77dade3260dcbc83b813e
            updateNumericInput(session, "lim_inf", value = round(input$media-(input$media/6),0))
            updateNumericInput(session, "lim_sup", value = round(input$media+(input$media/6),0))
            }
            
    }, ignoreInit = TRUE)
    
    output$probDist <- renderPlot({
<<<<<<< HEAD
        
        plot_params(media = input$media,
                    sd = input$dp,
                    calc = input$calc,
                    xlim1 = NULL,
                    xlim2 = NULL,
                    lim_inf = input$lim_inf,
                    lim_sup = input$lim_sup,
                    escore = input$escore,
                    ic_perc = input$ic_perc) +
            theme_minimal() +
            app_theme + 
=======

         
        
         menor_q <- stat_function(fun = dnorm,
                                  args = list(mean = input$media, sd = input$dp),
                                  xlim = c(-4*input$dp+input$media,input$escore),
                                  geom = "area", fill = "#006495ff", alpha = .3, color = "#006495")
         
         maior_q <- stat_function(fun = dnorm,
                                  args = list(mean = input$media, sd = input$dp),
                                  xlim = c(input$escore,4*input$dp+input$media),
                                  geom = "area", fill = "#006495ff", alpha = .3, color = "#006495")
         
         entre <- stat_function(fun = dnorm,
                                args = list(mean = input$media, sd = input$dp),
                                xlim = c(input$lim_inf,input$lim_sup),
                                geom = "area", fill = "#006495ff", alpha = .3, color = "#006495")
         
         lim_ic <- stat_function(fun = dnorm,
                                 args = list(mean = input$media, sd = input$dp),
                                 xlim = c(input$media-input$dp*qnorm((100-input$ic_perc)/200),
                                          input$media-input$dp*qnorm((100+input$ic_perc)/200)),
                                 geom = "area", fill = "#006495ff", alpha = .3, color = "#006495")
        
        ggplot() +
            stat_function(geom = "area", fun = dnorm, args = list(mean = input$media, sd = input$dp),
                          color = "#eb968e", alpha = 0.2, xlim = c(input$media-input$dp*4,input$media+input$dp*4))  +
            {if (input$calc == "Probabilidade abaixo de um escore") {menor_q}
                else if(input$calc == "Probabilidade acima de um escore") {maior_q}
                else if(input$calc == "Probabilidade entre dois valores") {entre}
                else if(input$calc == "Limites para um intervalo de confiança") {lim_ic}
            }+ {if (input$calc %in% c("Probabilidade abaixo de um escore",
                                      "Probabilidade acima de um escore"))
                                     {geom_vline(aes(xintercept = input$escore), linetype = "dashed")}
                
                else if(input$calc == "Probabilidade entre dois valores")
                {geom_vline(aes(xintercept = c(input$lim_inf,input$lim_sup)),
                                linetype = "dashed")}
                
                else if(input$calc == "Limites para um intervalo de confiança")
                {geom_vline(aes(xintercept = c(input$media-input$dp*qnorm((100-input$ic_perc)/200),
                                               input$media-input$dp*qnorm((100+input$ic_perc)/200))),
                            linetype = "dashed")}
                } +
            theme_classic() +
            theme(axis.line.y = element_blank(), axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(), axis.title = element_blank()) +
>>>>>>> 119055378c56178b59e77dade3260dcbc83b813e
            scale_x_continuous(breaks = seq(input$media-input$dp*3,input$media+input$dp*3,input$dp))
            

    })
    
    output$resultado <- renderText({if (input$calc == "Probabilidade abaixo de um escore") {
        paste0("A probabilidade de um valor abaixo de ",input$escore," é de ",
               round(diff(pnorm(c(-Inf, input$escore),input$media,input$dp)),3)*100,"%")}
        
        else if(input$calc == "Probabilidade acima de um escore") {
            paste0("A probabilidade de um valor acima de ",input$escore," é de ",
                   round(diff(pnorm(c(input$escore,Inf),input$media,input$dp)),3)*100,"%")}
        
        else if(input$calc == "Probabilidade entre dois valores") {
            paste0("A probabilidade de um valor entre ",input$lim_inf," e ",input$lim_sup," é de ",
                   round(diff(pnorm(c(input$lim_inf, input$lim_sup),input$media,input$dp)),3)*100,"%")}
        
        else if(input$calc == "Limites para um intervalo de confiança") {
            paste0(input$ic_perc, "% dos valores dessa distribuição se encontram entre ",
                   round(qnorm(((100-input$ic_perc)/200), input$media, input$dp),2)," e ",
                   round(qnorm(((100+input$ic_perc)/200), input$media, input$dp),2))}
    })
}

shinyApp(ui = ui, server = server)
