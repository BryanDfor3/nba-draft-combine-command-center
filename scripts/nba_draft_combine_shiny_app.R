library(dplyr) #for data transformation
library(ggradar) #for radar charts
library(ggplot2) #for lollipop charts
library(patchwork) #for chart layout
library(shiny) #for app interface
library(stringr) #for str_detect
library(bslib) #for navset_card_pill()
library(tidyr) #for data pivoting
library(purrr) #for map function
library(shadowtext) #for shadowed text on lollipop chart mark labels
library(gt) #for gt table
library(glue) #for table formatting

# Read in the data and make initial transformations
all_data <- read.csv('nba_draft_combine_ptiles (old).csv')
all_data <- all_data %>%
  mutate(PLUS_WS = if_else(PLUS_WS >=0, paste0("+", PLUS_WS), as.character(PLUS_WS)))

# Function to standardize dual positions (e.g., PG-SG and SG-PG become PG-SG)
standardize_position <- function(position) {
  sapply(position, function(pos) {
    if (str_detect(pos, "-")) {
      position_parts <- unlist(str_split(pos, "-"))
      sorted_position <- paste(sort(position_parts), collapse = "-")
      return(sorted_position)
    }
    return(pos)  # Return single positions as-is
  })
}

# Function to calculate means based on position type
get_position_means <- function(data, position) {
  standardized_position <- standardize_position(position)
  
  if (str_detect(position, "-")) {
    # For dual positions, include rows where POSITION matches any combination
    position_parts <- unlist(str_split(standardized_position, "-"))
    
    filtered_data <- data %>%
      filter(
        standardize_position(POSITION) == standardized_position | 
          POSITION %in% position_parts
      )
    
  } else {
    # For single positions, include only exact matches
    filtered_data <- data %>%
      filter(POSITION == position)
  }
  
  # Calculate means across all measurement percentiles
  means <- filtered_data %>%
    summarise(across(c('HEIGHT_WO_SHOES_PTILE', 'WEIGHT_PTILE', 'WINGSPAN_PTILE',
                       'STANDING_REACH_PTILE', 'STANDING_VERTICAL_LEAP_PTILE', 
                       'MAX_VERTICAL_LEAP_PTILE', 'LANE_AGILITY_TIME_PTILE', 
                       'THREE_QUARTER_SPRINT_PTILE', 'HAND_SIZE_PTILE'), 
                     function(x) mean(x, na.rm = TRUE))) %>%
    mutate(POSITION = standardized_position) %>%
    select(POSITION, everything())
  return(means)
}

# Get unique positions from the dataset
all_positions <- unique(all_data$POSITION)

# Standardize dual positions to avoid redundant computations
standardized_positions <- unique(sapply(all_positions, standardize_position))

# Apply the function to all unique positions (single and dual)
position_means <- do.call(rbind, lapply(standardized_positions, function(pos) get_position_means(all_data, pos)))

#Rename the position_means fields 
names(position_means) <- c('POSITION','HEIGHT','WEIGHT',
                           'WINGSPAN','STAND.REACH','STAND.VERT',
                           'MAX VERT','LANE AGILITY','SPRINT','HAND SIZE')

#Fill NAs with zeros
all_data[,c('HEIGHT_WO_SHOES_PTILE', 'WEIGHT_PTILE', 'WINGSPAN_PTILE',
            'STANDING_REACH_PTILE', 'STANDING_VERTICAL_LEAP_PTILE', 
            'MAX_VERTICAL_LEAP_PTILE', 'LANE_AGILITY_TIME_PTILE', 
            'THREE_QUARTER_SPRINT_PTILE', 'HAND_SIZE_PTILE')][is.na(all_data[,c('HEIGHT_WO_SHOES_PTILE', 'WEIGHT_PTILE', 'WINGSPAN_PTILE',
                                                                                'STANDING_REACH_PTILE', 'STANDING_VERTICAL_LEAP_PTILE', 
                                                                                'MAX_VERTICAL_LEAP_PTILE', 'LANE_AGILITY_TIME_PTILE', 
                                                                                'THREE_QUARTER_SPRINT_PTILE', 'HAND_SIZE_PTILE')])] <- 0
                
#Add dummy columns to the position means dataframe
position_means <- cbind(position_means, SEASON=0, PLAYER_NAME="", PLUS_WS="NA", WEIGHT_t="NA", HEIGHT_t="NA", STANDARDIZED_POSITION="NA", zero_count = 0)

#Use the standardize_position function to standardize the position names in all_data
all_data <- all_data |>
  mutate(STANDARDIZED_POSITION = standardize_position(POSITION))

all_data <- all_data %>%
  mutate(STANDARDIZED_POSITION = recode(STANDARDIZED_POSITION,
                                        'C-PF' = 'PF-C',
                                        'PF-SF' = 'SF-PF',
                                        'SF-SG' = 'SG-SF'))

position_means <- position_means %>%
  mutate(POSITION = recode(POSITION,
                                        'C-PF' = 'PF-C',
                                        'PF-SF' = 'SF-PF',
                                        'SF-SG' = 'SG-SF'))

#Join all_data with position_means on the standardized position
all_data <- all_data |> left_join( position_means,
                                   by=c('STANDARDIZED_POSITION'= 'POSITION'), suffix=c("",".x"), keep=FALSE)

#Create a calculated field for Performance
all_data$PERFORMANCE <- (all_data$HEIGHT_WO_SHOES_PTILE +
                            all_data$WINGSPAN_PTILE +
                            all_data$STANDING_REACH_PTILE +
                            all_data$STANDING_VERTICAL_LEAP_PTILE +
                            all_data$MAX_VERTICAL_LEAP_PTILE +
                            all_data$LANE_AGILITY_TIME_PTILE +
                            all_data$THREE_QUARTER_SPRINT_PTILE) -
  (all_data$HEIGHT.x +
     all_data$WINGSPAN.x +
     all_data$`STAND.REACH` +
     all_data$`STAND.VERT` +
     all_data$`MAX VERT` +
     all_data$`LANE AGILITY` +
     all_data$THREE_QUARTER_SPRINT_PTILE)

#Sort the data in descending order of Performance (Default sorting order)
all_data <- all_data[order(all_data$PERFORMANCE, decreasing=TRUE),]

#Count the number of measurements collected for each player (nonzeros)
all_data$zero_count <- rowSums(all_data[,c('HEIGHT_WO_SHOES_PTILE', 'WEIGHT_PTILE', 'WINGSPAN_PTILE',
                                          'STANDING_REACH_PTILE', 'STANDING_VERTICAL_LEAP_PTILE', 
                                          'MAX_VERTICAL_LEAP_PTILE', 'LANE_AGILITY_TIME_PTILE', 
                                          'THREE_QUARTER_SPRINT_PTILE', 'HAND_SIZE_PTILE')] != 0, na.rm = TRUE)

#Create a subset of all_data called selected_players
selected_players = all_data[, c('SEASON', 'PLAYER_NAME','POSITION','PLUS_WS',
                                'WEIGHT','HEIGHT','HEIGHT_WO_SHOES_PTILE','WEIGHT_PTILE',
                                'WINGSPAN_PTILE', 'STANDING_REACH_PTILE','STANDING_VERTICAL_LEAP_PTILE', 
                                'MAX_VERTICAL_LEAP_PTILE', 'LANE_AGILITY_TIME_PTILE','THREE_QUARTER_SPRINT_PTILE',
                                'HAND_SIZE_PTILE','STANDARDIZED_POSITION', 'zero_count')]

#Rename columns in selected_players to prepare for radar charts
names(selected_players) <- c('SEASON','PLAYER_NAME','POSITION','PLUS_WS', "WEIGHT_t", 'HEIGHT_t',
                             'HEIGHT','WEIGHT','WINGSPAN','STAND.REACH','STAND.VERT',
                             'MAX VERT','LANE AGILITY','SPRINT','HAND SIZE', 'STANDARDIZED_POSITION', 'zero_count')

#Reorder columns in position_means to prepare for rbind
position_means <- position_means[,c(11, 12, 1, 13, 14, 15,2:10, 16, 17)]

#Set the order for the position filter to display
position_order <- c("PG", "PG-SG", "SG", "SG-SF", "SF", "SF-PF", "PF", "PF-C", "C")

#Set up variables for slider height and wingspan filters
initial_height <- floor(min(all_data$HEIGHT_WO_SHOES, na.rm = TRUE))
feet <- floor(initial_height / 12)
inches <- initial_height %% 12
formatted_height <- paste0(feet, "'", inches, '"')
initial_wingspan <- floor(min(all_data$WINGSPAN, na.rm = TRUE))
ft <- floor(initial_wingspan / 12)
in_ <- initial_wingspan %% 12
formatted_wingspan <- paste0(ft, "'", in_, '"')

#Add photo url to player id for headshots in the gt table
all_data$PLAYER_ID <- paste0('https://cdn.nba.com/headshots/nba/latest/1040x760/',
                              all_data$PLAYER_ID,
                             '.png')

#Create list of dropdown options for sorting
measurements <- c("N/A", "Height", "Weight", "Wingspan", "Standing Reach", "Standing Vertical", "Max Vertical", "Lane Agility", "3/4 Court Sprint", "Hand Size")

# 1.0 USER INTERFACE ----
ui <- fluidPage(
  
  #Add GitHub icons to UI using font awesome 5.15.4
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
  ),  
  
  navbarPage(
    #Add Navbar with title, led by NBA Draft Combine logo   
    title = div(
        style = "display: flex; align-items: center; height: 100%",
          img(
            src='https://www.legends-mag.com/assets/media/issue_14/2018-nba-draft-combine.png',
            width = 80
            ),
            span(
              "NBA Draft Combine Command Center",
            style = "margin-left: 10px; font-size: 24px;"
          )
        ),
      #Tab 1 page 1: Measurements
      tabPanel("Measurements",
         sidebarLayout(
           sidebarPanel(
             #Add text to sidebar panel
             HTML(
               paste0(
                 "<h3>NBA Draft Combine Measurements Explorer</h3>",
                 "<strong> KEY </strong>",
                 "<ul>",
                 "<li>Player measurement percentiles are displayed in <strong style='color:#FFB71B; text-decoration: underline;'>GOLD</strong></li>",
                 "<li>Player positional average percentiles are displayed in <strong style='color:gray; text-decoration: underline;'>GRAY</strong></li>",
                 "<li>The 50th percentile across all players is displayed in <strong style='color:red; text-decoration: underline;'>RED</strong></li>",
                 "</ul>"
               )
             ),
             
             #Add filters and sorting options
             selectizeInput(
               "name", 
               "Filter by Name(s)", 
               choices = NULL, 
               multiple=TRUE
             ),
             selectizeInput(
               "year", 
               "Filter by Draft Combine Year(s)", 
               choices = NULL,
               multiple=TRUE,
             ),
             selectizeInput(
               "school", 
               "Filter by School(s)",
               choices = NULL,
               multiple=TRUE
             ),
             
             selectInput("sort_column", "Sort By Measurement", choices = measurements),
             
             radioButtons("sort_direction", "Sort Direction",
                          c("Descending" = "desc",
                            "Ascending" = "asc"),
                          inline = TRUE),
             
             sliderInput(
               inputId = "min_height",
               label = "Minimum Height:",
               min = min(66, na.rm = TRUE),
               max = max(90, na.rm = TRUE),
               value = initial_height,
               step = 1
             ),
             textOutput("formatted_height"),
             
             sliderInput(
               inputId = "min_wingspan",
               label = "Minimum Wingspan:",
               min = min(70, na.rm = TRUE),
               max = max(100, na.rm = TRUE),
               value = initial_wingspan,
               step = 1
             ),
             textOutput("formatted_wingspan"),
             
             HTML("<strong> Filter by Position(s) </strong>"),
             
             # Add 'Select All' and 'Unselect All' filter buttons for player position
             
             shiny::fluidRow(
               column(12,
                      actionLink("select_all_positions", "Select All Positions"),
                      span(" | "),
                      actionLink("unselect_all_positions", "Unselect All Positions"),
               )
             ),
             
             br(),
             
             uiOutput("height_display"),
             
             # Add checkbox filters for player positions in a 3x3 grid
             shiny::splitLayout(
               checkboxGroupInput("position1", label = NULL, choices = position_order[1:3], selected = position_order),
               checkboxGroupInput("position2", label = NULL, choices = position_order[4:6], selected = position_order),
               checkboxGroupInput("position3", label = NULL, choices = position_order[7:9], selected = position_order)
             ),
             
             actionButton("Prev", "Previous Page"),
             actionButton("Next", "Next Page"),
             
             HTML(paste0("<br>","<br>")),
             
             textOutput("page_info"),
             
             #Add hyperlink to GitHub page and to athletic test explanations & video

             HTML(paste0(
               "<br>",
               "<strong> External Links</strong>",
               "<br>",
               "<a href='https://github.com/BryanDfor3/nba-draft-combine-command-center' target='_blank' style='text-decoration: none; font-size: 14px;'>",
               "<i class='fab fa-github'></i> GitHub Repository</a>",
               "<br>",
               "<a href='https://www.nba.com/stats/draft/combine-anthro' target='_blank' style='text-decoration: none; font-size: 14px;'>",
               "<i class='fas fa-basketball-ball'></i> NBA.com Draft Combine Data</a>",
               "<br>",
               "<a href='https://www.checkmyathletics.com/basketball-combine' target='_blank' style='text-decoration: none; font-size: 14px;'>",
               "<i class='fas fa-basketball-ball'></i> Athletic Test Explanations & Video Examples</a>",
               "<br>"
               )),
                  
             width = 4
           ),
           
           #Add navset_card_pill to click between different views and add each plot to the appropriate pill
           mainPanel(
             navset_card_pill(
               nav_panel("Radar Chart View",
                plotOutput("radar_plots", height = "875px")
               ),
               nav_panel("Percentile Values View",
                plotOutput("lollipops", height = "875px")
               ),
               nav_panel("Raw Measurements View",
                gt_output("table")
               )
           )
         )
         )
      ),
      #Add an additional panel for shooting (TBD on adding visuals here)
      #tabPanel("Shooting")
    
    )
)
  

# 2.0 SERVER ----
server <- function(input, output, session) {
  #Update the NULL selectize input options
  updateSelectizeInput(
    session,
    "name",
    choices = sort(all_data$PLAYER_NAME, decreasing = FALSE),
    server = TRUE
  )

  updateSelectizeInput(
    session,
    "year",
    choices = sort(all_data$SEASON, decreasing = TRUE),
    selected = max(all_data$SEASON),
    server = TRUE
  )
  
  updateSelectizeInput(
    session,
    "school",
    choices = sort(all_data$School, decreasing = FALSE),
    server = TRUE
  )
  
#Filter players based on selected year, position, school, and height / wingspan  range
  
  filtered_players <- reactive({
    df <- all_data
    
    if (!is.null(input$year) && length(input$year) > 0) {
      df <- df[df$SEASON %in% input$year, ]
    }
    
    if (!is.null(input$name) && length(input$name) > 0) {
      df <- df[df$PLAYER_NAME %in% input$name, ]
    }
    
    if (!is.null(input$school) && length(input$school) > 0) {
      df <- df[df$School %in% input$school, ]
    }   
    
    selected_positions <- c(input$position1, input$position2, input$position3)
    if (length(selected_positions) > 0) {
      df <- df[df$STANDARDIZED_POSITION %in% selected_positions, ]
    }
    
    if (!is.null(input$min_height)) {
      df <- df[df$HEIGHT_WO_SHOES >= input$min_height, ]
    }
    
    if (!is.null(input$min_wingspan)) {
      df <- df[df$WINGSPAN >= input$min_wingspan, ]
    }
    
  #Update the visuals if any of the sorting options are selected
    
    if (input$sort_column == "Height" && input$sort_direction == "desc") {
      df <- arrange(df, desc(df$HEIGHT_WO_SHOES_PTILE))
    }
    
    if (input$sort_column == "Height" && input$sort_direction == "asc") {
      df <- arrange(df, df$HEIGHT_WO_SHOES_PTILE)
    }
    
    if (input$sort_column == "Weight" && input$sort_direction == "desc") {
      df <- arrange(df, desc(df$WEIGHT_PTILE))
    }
    
    if (input$sort_column == "Weight" && input$sort_direction == "asc") {
      df <- arrange(df, df$WEIGHT_PTILE)
    }
    
    if (input$sort_column == "Wingspan" && input$sort_direction == "desc") {
      df <- arrange(df, desc(df$WINGSPAN_PTILE))
    }
    
    if (input$sort_column == "Wingspan" && input$sort_direction == "asc") {
      df <- arrange(df, df$WINGSPAN_PTILE)
    }
    
    if (input$sort_column == "Standing Reach" && input$sort_direction == "desc") {
      df <- arrange(df, desc(df$STANDING_REACH_PTILE))
    }
    
    if (input$sort_column == "Standing Reach" && input$sort_direction == "asc") {
      df <- arrange(df, df$STANDING_REACH_PTILE)
    }
    
    if (input$sort_column == "Standing Vertical" && input$sort_direction == "desc") {
      df <- arrange(df, desc(df$STANDING_VERTICAL_LEAP_PTILE))
    }
    
    if (input$sort_column == "Standing Vertical" && input$sort_direction == "asc") {
      df <- arrange(df, df$STANDING_VERTICAL_LEAP_PTILE)
    }
    
    if (input$sort_column == "Max Vertical" && input$sort_direction == "desc") {
      df <- arrange(df, desc(df$MAX_VERTICAL_LEAP_PTILE))
    }
    
    if (input$sort_column == "Max Vertical" && input$sort_direction == "asc") {
      df <- arrange(df, df$MAX_VERTICAL_LEAP_PTILE)
    }
    
    if (input$sort_column == "Lane Agility" && input$sort_direction == "desc") {
      df <- arrange(df, desc(df$LANE_AGILITY_TIME_PTILE))
    }
    
    if (input$sort_column == "Lane Agility" && input$sort_direction == "asc") {
      df <- arrange(df, df$LANE_AGILITY_TIME_PTILE)
    }
    
    if (input$sort_column == "3/4 Court Sprint" && input$sort_direction == "desc") {
      df <- arrange(df, desc(df$THREE_QUARTER_SPRINT_PTILE))
    }
    
    if (input$sort_column == "3/4 Court Sprint" && input$sort_direction == "asc") {
      df <- arrange(df, df$THREE_QUARTER_SPRINT_PTILE)
    }
    
    if (input$sort_column == "Hand Size" && input$sort_direction == "desc") {
      df <- arrange(df, desc(df$HAND_SIZE_PTILE))
    }
    
    if (input$sort_column == "Hand Size" && input$sort_direction == "asc") {
      df <- arrange(df, df$HAND_SIZE_PTILE)
    }
    
    #Account for null results
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    return(df)
  })
  
  #Limit each plot to 9 per page and configure reactive behavior of the app (e.g. page navigation, page numbers, selecting/deselecting filters) 
  plots_per_page <- 9
  total_pages <- reactive({
    df <- filtered_players()
    if(is.null(df)) return(1)
    ceiling(nrow(df) / plots_per_page)
  })
  current_page <- reactiveVal(1)

  observeEvent(input$Next, {
    if(current_page() < total_pages()) {
      current_page(current_page() + 1)
    }
  })
  
  observeEvent(input$Prev, {
    if(current_page() > 1) {
      current_page(current_page() - 1)
    }
  })
  
  observeEvent(list(input$year, input$name, input$school, input$position1, input$position2, input$position3,
                    input$min_height, input$min_wingspan), {
    if(current_page() > total_pages()) {
      current_page(1)
    }
  })
  
  observeEvent(input$select_all_positions, {
    updateCheckboxGroupInput(session, "position1", selected = position_order[1:3])
    updateCheckboxGroupInput(session, "position2", selected = position_order[4:6])
    updateCheckboxGroupInput(session, "position3", selected = position_order[7:9])
  })
  
  observeEvent(input$unselect_all_positions, {
    updateCheckboxGroupInput(session, "position1", selected = character(0))
    updateCheckboxGroupInput(session, "position2", selected = character(0))
    updateCheckboxGroupInput(session, "position3", selected = character(0))
  })
  
  updateSliderInput(session, "min_height", label = formatted_height)
  
  output$page_info <- renderText({
    paste("Page", current_page(), "of", total_pages())
  })
  
  observe({
    # Get current height value and update slider label
    height_in_inches <- input$min_height
    feet <- floor(height_in_inches / 12)
    inches <- height_in_inches %% 12
    formatted_height <- paste0(feet, "'", inches, '"')
    
    updateSliderInput(session, "min_height", label=paste("Minimum Height:", formatted_height))
  })
  
  observe({
    # Get current wingspan value and update slider label
    wingspan_in_inches <- input$min_wingspan
    ft <- floor(wingspan_in_inches / 12)
    in_ <- wingspan_in_inches %% 12
    formatted_wingspan <- paste0(ft, "'", in_, '"')
    
    updateSliderInput(session, "min_wingspan", label=paste("Minimum Wingspan:", formatted_wingspan))
  })
  
  #Render radar plots dynamically
  output$radar_plots <- renderPlot({
    
    selected_players <- filtered_players()[, c('SEASON', 'PLAYER_NAME','POSITION','PLUS_WS',
                                               'WEIGHT','HEIGHT','HEIGHT_WO_SHOES_PTILE','WEIGHT_PTILE',
                                               'WINGSPAN_PTILE', 'STANDING_REACH_PTILE','STANDING_VERTICAL_LEAP_PTILE', 
                                               'MAX_VERTICAL_LEAP_PTILE', 'LANE_AGILITY_TIME_PTILE','THREE_QUARTER_SPRINT_PTILE',
                                               'HAND_SIZE_PTILE','STANDARDIZED_POSITION', 'zero_count')]
    
    names(selected_players) <- c('SEASON','PLAYER_NAME','POSITION','PLUS_WS', "WEIGHT_t", 'HEIGHT_t',
                          'HEIGHT','WEIGHT','WINGSPAN','STAND.REACH','STAND.VERT',
                          'MAX VERT','LANE AGILITY','SPRINT','HAND SIZE','STANDARDIZED_POSITION', 'zero_count')
    
    if (is.null(selected_players) || nrow(selected_players) == 0) {
      return(NULL)
    }

    #Select data slice based on the app's current page
    start_idx <- (current_page() -1) * plots_per_page + 1
    end_idx <- min(start_idx + plots_per_page -1, nrow(selected_players))
    selected_players <- selected_players[start_idx:end_idx, , drop=FALSE]
    standardized_pos <- selected_players$STANDARDIZED_POSITION
    
    #Perform an rbind with the position means to include both datasets
    new_selected_players <- rbind(position_means,selected_players)
    new_selected_players$PLAYER_NAME <- factor(new_selected_players$PLAYER_NAME)
    
    # Create Radar Chart
    
    # Apply function to all non-positional mean rows (individual player measurement data)
    plot_list <- lapply((nrow(position_means)+1):nrow(new_selected_players), function(i) {
      
      # Access the player's position by manipulating the index of the standardized_pos dataframe
      player_position <- standardized_pos[i - nrow(position_means)]

      # Use the player's position to retrieve the index value in the new_selected_players dataframe 
      j <- match(player_position, new_selected_players$POSITION)

      # Continue to run the function using the index of the player's positional average (j) and measurements (i) across selected columns
      radar_data <- new_selected_players[c(j, i), c('PLAYER_NAME','HEIGHT','WEIGHT',
                                                    'WINGSPAN','STAND.REACH','STAND.VERT',
                                                    'MAX VERT','LANE AGILITY','SPRINT','HAND SIZE')]
      
      radar_data$Group <- factor(c("Player", "Position Average"), levels = c("Player", "Position Average"))
      
      ggradar(new_selected_players[c(j,i),c('PLAYER_NAME','HEIGHT','WEIGHT',
                                            'WINGSPAN','STAND.REACH','STAND.VERT',
                                            'MAX VERT','LANE AGILITY','SPRINT','HAND SIZE') ],
              fill = TRUE,
              fill.alpha = 0.55,
              values.radar = c(0, 0.5, 1), 
              grid.min = 0, grid.mid = 0.5, grid.max = 1, 
              axis.label.size = 2.9,  
              group.point.size = 1,  
              grid.label.size = 0,  
              group.colours = c('gray',"#FFB71B")[1:nrow(radar_data)],
              group.line.width = 1,
              gridline.mid.colour = "red",
              background.circle.colour = "floralwhite") +
        ggtitle(paste(new_selected_players$PLAYER_NAME[i], "-", new_selected_players$POSITION[i],'\n',
                      new_selected_players$HEIGHT_t[i], new_selected_players$WEIGHT_t[i],"lbs", "|",
                      new_selected_players$PLUS_WS[i], "wingspan"),
                subtitle = paste(new_selected_players$SEASON[i], "-", new_selected_players$zero_count[i],"of 9 attributes measured"))+
        theme_minimal() +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 9, hjust = 0.5),
              axis.text = element_blank(),
              panel.grid = element_blank(),
              plot.background = element_rect(fill = "floralwhite", color = NA),
              legend.position = "none",
              plot.margin = margin(10, 25, 10, 25),
              text = element_text(family="menlo"))
    })
    
    final_plot <- wrap_plots(plot_list, ncol=3)
    final_plot
  })
  
  #Prep data for lollipop charts
  
  output$lollipops <- renderPlot({
    player_data <- filtered_players()[, c('SEASON', 'PLAYER_NAME','POSITION','PLUS_WS',
                                          'WEIGHT','HEIGHT','HEIGHT_WO_SHOES_PTILE','WEIGHT_PTILE',
                                          'WINGSPAN_PTILE', 'STANDING_REACH_PTILE','STANDING_VERTICAL_LEAP_PTILE', 
                                          'MAX_VERTICAL_LEAP_PTILE', 'LANE_AGILITY_TIME_PTILE','THREE_QUARTER_SPRINT_PTILE',
                                          'HAND_SIZE_PTILE','STANDARDIZED_POSITION', 'zero_count')]
    
    names(player_data) <- c('SEASON','PLAYER_NAME','POSITION','PLUS_WS', "WEIGHT_t", 'HEIGHT_t',
                                 'HEIGHT','WEIGHT','WINGSPAN','STAND.REACH','STAND.VERT',
                                 'MAX VERT','LANE AGILITY','SPRINT','HAND SIZE','STANDARDIZED_POSITION', 'zero_count')
    
    if (is.null(player_data) || nrow(player_data) == 0) {
      return(NULL)
    }
    
    start_idx <- (current_page() -1) * plots_per_page + 1
    end_idx <- min(start_idx + plots_per_page -1, nrow(player_data))
    player_data <- player_data[start_idx:end_idx, , drop=FALSE]
    
    standardized_pos <- player_data$STANDARDIZED_POSITION
    
    transformed_data <- player_data %>% 
      pivot_longer(
        cols = c('HEIGHT','WEIGHT','WINGSPAN','STAND.REACH',
                 'STAND.VERT','MAX VERT','LANE AGILITY','SPRINT', 'HAND SIZE'),
        names_to = "MEASUREMENT",
        values_to = "PERCENTILE"
      ) %>%
      mutate(
        MEASUREMENT = case_when(
          MEASUREMENT == "HEIGHT" ~ "HEIGHT",
          MEASUREMENT == "WEIGHT" ~ "WEIGHT",
          MEASUREMENT == "WINGSPAN" ~ "WINGSPAN",
          MEASUREMENT == "HAND SIZE" ~ "HAND SIZE",
          MEASUREMENT == "STAND.REACH" ~ "STANDING REACH",
          MEASUREMENT == "LANE AGILITY" ~ "LANE AGILITY",
          MEASUREMENT == "STAND.VERT" ~ "STANDING VERT",
          MEASUREMENT == "MAX VERT" ~ "MAX VERTICAL",
          MEASUREMENT == "SPRINT" ~ "3/4 COURT SPRINT",
        ),
          MEASUREMENT = factor(MEASUREMENT, 
                             levels = c("HAND SIZE", "3/4 COURT SPRINT","LANE AGILITY","MAX VERTICAL",
                                        "STANDING VERT", "STANDING REACH","WINGSPAN", "WEIGHT","HEIGHT"))
      )
    
    transformed_data <- transformed_data %>% mutate(across(c('PERCENTILE'), round, 2))
    
    transformed_data$PERCENTILE <- transformed_data$PERCENTILE * 100
  
    #Split the data by Player Name & Combine Year, while simultaneously maintaining row order of the data
    transformed_data$uniqueid <- paste0(transformed_data$PLAYER_NAME, transformed_data$SEASON)
    player_order <- unique(transformed_data$uniqueid)
  
    split_list <- lapply(player_order, function(uniqueid) {
      transformed_data[transformed_data$uniqueid == uniqueid, ]
      
    })
    
    #Generate the lollipop charts
    l_plot_list <- 
      map(split_list, ~ {
        ggplot(.x, aes(MEASUREMENT, PERCENTILE)) +
          geom_segment(aes(xend = MEASUREMENT, y = 0, yend = PERCENTILE),
                       color = "#FBE5AD", lwd = 3) +
          geom_point(size = 8, pch = 21, bg = "#FFB71B", col = "#D4A017") +
          #Add shadowtext on the plot labels
          geom_shadowtext(aes(label = PERCENTILE), color = "white", fontface = "bold", size = 4, bg.color ="black") +
          coord_flip() +
          ggtitle(paste(.x$PLAYER_NAME, "-", .x$POSITION, '\n',
                        .x$HEIGHT_t, .x$WEIGHT_t,"lbs", "|",
                        .x$PLUS_WS,"wingspan"),
                  subtitle = paste(.x$SEASON,"-",.x$zero_count,"of 9 attributes measured")) +
          theme(
            axis.text.x = element_blank(),
            axis.text.y = element_text(size = 9, hjust=0),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            axis.title.x = element_blank(),
            panel.grid = element_blank(),
            plot.background = element_rect(fill = "floralwhite", color = "floralwhite"),
            plot.margin = margin(10, 50, 10, 15),
            plot.title = element_text(size = 14, face = "bold", hjust=0.5),
            plot.subtitle = element_text(size = 8.5, hjust = 0.5),
            panel.background = element_rect(fill = "floralwhite", color = "floralwhite"),
            text = element_text(family="menlo"),
            aspect.ratio = 4 / 3
          ) +
          ylim(-5, 105)
      })
    
    wrap_plots(plotlist = l_plot_list, ncol=3)
    
  })
    
  # Create gt Table
  output$table <- render_gt({
  
  dframe <- filtered_players() |> select('PLAYER_ID', 'PLAYER_NAME', 'SEASON', 'STANDARDIZED_POSITION', 'School', 'Country', 'HEIGHT_WO_SHOES_FT_IN',
                   'WEIGHT', 'WINGSPAN_FT_IN', 'STANDING_REACH_FT_IN', 'STANDING_VERTICAL_LEAP', 
                   'MAX_VERTICAL_LEAP', 'LANE_AGILITY_TIME', 'THREE_QUARTER_SPRINT', 'HAND_LENGTH', 'HAND_WIDTH')
  
  if (is.null(dframe) || nrow(dframe) == 0) {
    return(NULL)
  }
  
  start_idx <- (current_page() -1) * plots_per_page + 1
  end_idx <- min(start_idx + plots_per_page -1, nrow(dframe))
  dframe <- dframe[start_idx:end_idx, , drop=FALSE]
  standardized_pos <- dframe$STANDARDIZED_POSITION
  
  
  dframe |>
    
  gt() |>
     
    #Horizontally align table cell text to center 
    tab_style(
       style = cell_text(font='menlo', align = 'center'),
       locations = cells_body(columns = everything())
     ) |>
    
    #Vertically align column labels to center
    tab_style(
      style = cell_text(font='menlo', v_align = 'middle'),
      locations = cells_column_labels(columns = everything())
    ) |>
    
    #Align the PLAYER_NAME column cells to the left
     tab_style(
       style = cell_text(align = 'left'),
       locations = cells_body(columns = PLAYER_NAME)
     ) |>
     
    #Configure the column labels 
    cols_label(PLAYER_ID = "",
                PLAYER_NAME = "PLAYER",
                SEASON = "",
                STANDARDIZED_POSITION = " ",
                School = "",
                Country = "",
                HEIGHT_WO_SHOES_FT_IN = "HEIGHT (ft, in)",
                WEIGHT = "WEIGHT (lbs)",
                WINGSPAN_FT_IN = "WINGSPAN (ft, in)",
                STANDING_REACH_FT_IN = "STANDING REACH (inches)",
                STANDING_VERTICAL_LEAP = "STANDING VERTICAL (inches)",
                MAX_VERTICAL_LEAP = "MAX VERTICAL (inches)",
                LANE_AGILITY_TIME = "LANE AGILITY (seconds)",
                THREE_QUARTER_SPRINT = "3/4 COURT SPRINT (seconds)",
                HAND_LENGTH = 'HAND LENGTH (inches)',
                HAND_WIDTH = 'HAND WIDTH (inches)'
       ) |>
     
    #Convert the player_id url to player headshots 
    text_transform(
       locations = cells_body(c(PLAYER_ID)),
       fn = function(x) {
         web_image(url = x,
                   height = px(50))
       }
     ) |>
    
    #Merge name, school, country, season, and position into a single column (PLAYER_NAME), and configure the formatting with HTML
     cols_merge(
       columns = c(PLAYER_NAME, School, Country, SEASON, STANDARDIZED_POSITION),
       pattern = "{1}||{2}||{3}||{4}||{5}"
     ) |>
    
     text_transform(
       locations = cells_body(columns = PLAYER_NAME),
       fn = function(x){
          split_vals <- strsplit(x, "\\|\\|", fixed = FALSE)
          
          lapply(split_vals, function(parts) {
          
          name <- parts[1]  
          school <- parts[2]
          country <- parts[3]
          szn <- parts[4]
          position <- parts[5]
          
          glue::glue(
          "<div>
            <span style='font-weight:bold; font-size:14.5px'>{name}</span>
            <span style='font-weight:bold;color:grey;font-size:12px';> {position}</span>
          </div>
          
          <div style='line-height:16px'>
            <span style ='font-weight:normal;color:grey;font-size:11.5px'>{szn} | {country}</span>
          </div>
          
          <div style='line-height:16px'>
          <span style ='font-weight:normal;color:grey;font-size:11.5px'>{school} </span>
          </div>"
          )
        })
       }
          
    ) |>
     
    #Align column labels to the center for the measurement columns
     tab_style(
       style = cell_text(align = 'center'),
       locations = cells_column_labels(columns = c('HEIGHT_WO_SHOES_FT_IN','WEIGHT', 'WINGSPAN_FT_IN', 
                                                   'STANDING_REACH_FT_IN', 'STANDING_VERTICAL_LEAP', 
                                                   'MAX_VERTICAL_LEAP', 'LANE_AGILITY_TIME', 'THREE_QUARTER_SPRINT', 'HAND_LENGTH', 'HAND_WIDTH'))
     ) |>
     
    #Align cell text to the center for the measurement columns
     cols_align(
       align = 'center',
       columns = c('HEIGHT_WO_SHOES_FT_IN','WEIGHT', 'WINGSPAN_FT_IN', 
                   'STANDING_REACH_FT_IN', 'STANDING_VERTICAL_LEAP', 
                   'MAX_VERTICAL_LEAP', 'LANE_AGILITY_TIME', 'THREE_QUARTER_SPRINT', 'HAND_LENGTH', 'HAND_WIDTH')
     ) |>
    
    #Configure the column width for measurement columns
      cols_width(c('HEIGHT_WO_SHOES_FT_IN','WEIGHT', 'WINGSPAN_FT_IN',
                    'STANDING_REACH_FT_IN', 'STANDING_VERTICAL_LEAP', 
                    'MAX_VERTICAL_LEAP', 'LANE_AGILITY_TIME', 'THREE_QUARTER_SPRINT', 'HAND_LENGTH', 'HAND_WIDTH') ~ px(85)) |>
    
    #Configure the column width for PLAYER_NAME
    cols_width(c('PLAYER_NAME') ~ px(200)) |>
     
    #Configure cell borders in the table
     tab_style(
       style = list(
         cell_borders(
           side = "top",
           color = 'gray35',
           weight = px(2),
         )
       ),
       locations = cells_body(rows=everything())
     ) |>
     
    #Configure table formatting options
       tab_options(
         table.background.color = 'floralwhite',
         column_labels.font.size = 13.5,
         table.font.size = 13.5,
         table.margin.left = px(5),
         table.margin.right = px(5),
         heading.title.font.size = 24,
         heading.title.font.weight = 'bold',
         heading.subtitle.font.size = 14,
         table.font.color = 'black',
         table.font.names = 'Menlo',
         table.border.top.color = "transparent",
         data_row.padding = px(7),
         column_labels.background.color = '#666666',
         column_labels.font.weight = '1000'
      
      )
})
}

# 3.0 RUN THE APP ----
shinyApp(ui = ui, server = server)

