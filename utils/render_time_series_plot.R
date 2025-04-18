render_time_series_plot <- function(data, station_id, month) {
  # Check if station_id exists in tavg_meta
  if (!station_id %in% tavg_meta$ID) {
    stop("Station ID not found in metadata.")
  }
  
  # Fit a linear model to calculate the slope
  linear_model <- lm(VALUE ~ YEAR, data = data)
  slope <- coef(linear_model)["YEAR"]
  
  # Create the title with slope
  title_text <- paste(
    month, min(data$YEAR), max(data$YEAR), "-", tavg_meta$NAME[tavg_meta$ID == station_id], station_id
  )
  
  # Main plot
  plot <- plot_ly(data, x = ~YEAR, y = ~VALUE, type = 'scatter', mode = 'lines',
                  name = 'TAVG',   # Set neutral color for points
                  line = list(color = 'red')) %>% # Set neutral color for the line
    layout(
      title = list(
        text = title_text,
        x = 0,
        y = 0.99,
        xanchor = 'left',
        font = list(size = 12)
      ),
      xaxis = list(
        zeroline = FALSE, 
        gridcolor = 'lightgray',
        title = "",
        fixedrange = TRUE
      ),
      yaxis = list(
        title = list(text = paste(month, '(°C)'), font = list(size = 10)), 
        zeroline = FALSE, 
        gridcolor = 'lightgray',
        fixedrange = TRUE
      ),
      showlegend = FALSE,
      plot_bgcolor = 'rgba(255, 255, 255, 0)',
      paper_bgcolor = 'rgba(255, 255, 255, 0)',
      margin = list(l = 20, r = 5, t = 20, b = 3),  # Increase bottom margin to accommodate text
      shapes = list(
        list(
          type = 'rect',
          x0 = 0, x1 = 1, y0 = 0, y1 = 1,
          xref = 'paper', yref = 'paper',
          fillcolor = 'rgba(255, 255, 255, 0)',
          line = list(width = 0)
        )
      ),
      hovermode = 'x unified',
      hoverlabel = list(
        bgcolor = 'white',
        bordercolor = 'rgba(255, 255, 255, 0)'
      ),
      annotations = list(
        list(
          x = 0.5,  # Center the text horizontally
          y = -0.12,  # Position it further below the plot
          xref = 'paper',
          yref = 'paper',
          showarrow = FALSE,
          text = paste("Slope:", round(slope, 3), "°C/year | Mean: ", round(mean(data$VALUE), 1), "°C"),
          xanchor = 'center',
          yanchor = 'top',
          font = list(size = 11, color = 'black')
        )
      )
    ) %>%
    add_trace(
      x = ~YEAR,
      y = fitted(linear_model),
      mode = 'lines',
      name = 'Linear Trend',
      line = list(color = 'gray'),
      hoverinfo = 'x+y'  # Display only x and y without custom text
    )
  
  
  # Configure the mode bar to show only the download button
  plot <- plot %>%
    config(
      modeBarButtonsToRemove = list(
        'zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d'
      ),
      displaylogo = FALSE,
      toImageButtonOptions = list(
        format = 'png',  # Choose your desired format (png, jpeg, svg, webp)
        filename = paste0(station_id, "_", month, "_time_series") 
      )
    )
  
  return(plot)
}