
ui <- fluidPage(
    titlePanel('Easyreporting Volcano Plot'),
    sidebarLayout(
        sidebarPanel(
            sliderInput(inputId="slider_pval", label=h3("P-value threshold"), min=0,
                        max=1, value=0.05, step=0.01),
            textAreaInput(inputId="comment_text", 
                          label="Place here the comment to the code chunk (optional)",
                          rows=10),
            fluidRow(
                column(6, align="left",
                       actionButton(inputId="do", label="Perform Plot", width="100%")
                ),
                column(6, align="right",
                       actionButton(inputId="do2", label="Compile Report", width="100%")
                )
            )
            
            # 
            # 
            
        ),
        
        mainPanel(
            plotOutput('plot1')
        )
    )
)
