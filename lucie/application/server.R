function(input, output) {
  
  table_liaisons = reactive({
    return(
      summary_stat_airport(
        create_data_frame_from_input(
          aeroports,
          year(input$date),
          month(input$date)
        )
      )
    )
  })
  
  output$table <- render_gt(
    create_table_airports(table_liaisons())
  )
  
  output$carte <- renderLeaflet(
    map_leaflet_airport(
      aeroports, localisations_aeroports,
      month(input$date), year(input$date)
    )
  )
  
  output$lineplot <- renderPlotly(
    plot_airport_line(aeroports, input$select)
  )
  
}