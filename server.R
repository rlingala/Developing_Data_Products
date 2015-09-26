library(shiny)
library(ggplot2)

function(input, output) {
        
        dataset <- reactive({
                diamonds[sample(nrow(diamonds), input$sampleSize),]
        })
        
        output$plot <- renderPlot({
                
                plot <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
                
                if (input$color != 'None')
                        plot <- plot + aes_string(color=input$color)
                
                 facets <- paste(input$facet_row, '~', input$facet_col)
                 if (facets != '. ~ .')
                       plot <- plot + facet_grid(facets)
                
                 if (input$jitter)
                        plot <- plot + geom_jitter()
                
                 if (input$smooth)
                       plot <- plot + geom_smooth()
                
                print(plot)
                
        }, height=600)
        
}
