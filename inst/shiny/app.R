ui <- shiny::fluidPage(
  "Hello, world!"
)
server <- function(input, output, session) {
}
shiny::shinyApp(ui, server)
