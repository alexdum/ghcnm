render_time_series_plot <- function(data, station_id, month) {
  plot_ly(data, x = ~YEAR, y = ~VALUE, type = 'scatter', mode = 'lines+markers', name = 'TAVG') %>%
    layout(
      title = list(
        text = paste(station_id, tavg_meta$NAME[tavg_meta$ID == station_id]),
        x = 0,  # Align title to the left
        y = 0.99,
        xanchor = 'left',  # Anchor title to the left
        font = list(size = 12)
      ),
      xaxis = list(
        zeroline = FALSE, 
        gridcolor = 'lightgray',
        title = "",  # Remove x-axis title
        fixedrange = TRUE  # Disable zoom on the x-axis
      ),
      yaxis = list(
        title = list(text = paste(month, '(°C)'), font = list(size = 10)), 
        zeroline = FALSE, 
        gridcolor = 'lightgray',
        fixedrange = TRUE  # Disable zoom on the y-axis
      ),
      showlegend = FALSE,
      plot_bgcolor = 'rgba(255, 255, 255, 0)',  # Semi-transparent background
      paper_bgcolor = 'rgba(255, 255, 255, 0)', # Semi-transparent background
      
      # Shrinking the margins as much as possible
      margin = list(
        l = 20,  # Left margin
        r = 5,  # Right margin
        t = 20,  # Top margin (leave a bit of space for the title)
        b = 2   # Bottom margin
      ),
      shapes = list(
        list(
          type = 'rect',
          x0 = 0, x1 = 1, y0 = 0, y1 = 1,
          xref = 'paper', yref = 'paper',
          fillcolor = 'rgba(255, 255, 255, 0)',  # Invisible border (roundness effect)
          line = list(width = 0)
        )
      )
    ) %>%
    add_trace(x = ~YEAR, y = fitted(lm(VALUE ~ YEAR, data = data)), mode = 'lines', name = 'Linear Trend') %>%
    config(
      modeBarButtonsToRemove = list(
        'zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d'
      ),
      displaylogo = FALSE  # Optionally, remove the Plotly logo
    )
}
