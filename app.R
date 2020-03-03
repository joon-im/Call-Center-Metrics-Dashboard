# Load libraries
library(shiny)         # Dashboard Framework
library(bs4Dash)       # Style Extension
library(shinyWidgets)  # Widgets Extension
library(tidyverse)     # Data Manipulation
library(plotly)        # Data Visualization
library(readxl)        # Read Excel Files
library(ggthemes)      # Visualization Themes
library(formattable)   # Format Outputs
library(flexdashboard) # UI Elements
library(lubridate)     # Time Series Manipulation
library(tidyquant) 

# Fonts
dir.create('~/.fonts')
file.copy("www/Roboto-Regular.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

# Load data
# Call data
raw_call_data <- read_excel("call_center_results.xlsx", sheet = 10)
# Rename columns
names(raw_call_data) <- tolower(names(raw_call_data))

# Target data
targets <- read_csv("call_center_targets.csv") %>% 
  spread(key = METRIC, value = TARGET)
# Rename columns
names(targets) <- tolower(names(targets))
# Joinable table
targets_join <- targets %>%
  mutate(handle_time_avg_target = aht / 60) %>%
  select(-aht) %>%
  rename(call_regen_rate_target          = call_regen,
         calls_offer_applied_rate_target = applied_per_call,
         call_breakage_rate_target       = breakage,
         transfer_rate_target            = transfers)

# Full Team Data
team_data_grouped <- raw_call_data %>%
  group_by(yr_mo, call_date, team_lead_name, agent_name) %>%
  summarize(calls = sum(calls),
            handle_time = sum(handle_time),
            handle_time_avg = handle_time / calls,
            call_regen = sum(call_regen),
            call_regen_rate = call_regen / calls,
            calls_with_offer = sum(calls_with_offer),
            calls_with_offer_rate = calls_with_offer / calls,
            calls_with_accept = sum(calls_with_accept),
            calls_with_accept_rate = calls_with_accept / calls,
            calls_offer_applied = sum(calls_offer_applied),
            calls_offer_applied_rate = calls_offer_applied / calls,
            call_transfers = sum(transfers),
            call_transfer_rate = transfers / calls) %>% 
  mutate(call_breakage_count =  (calls_with_accept - calls_offer_applied),
         call_breakage_rate = (calls_with_accept - calls_offer_applied) / calls_with_accept) %>%
  ungroup() %>%
  left_join(targets_join, by="yr_mo")

# Call Type Ratio Heatmap Data
pct_calls_by_agent <- team_data_grouped %>%
  
  select(agent_name, calls_with_offer_rate, calls_with_accept_rate, calls_offer_applied_rate,
         call_transfer_rate, call_breakage_rate) %>%
  group_by(agent_name) %>%
  summarize(calls_with_offer_rate = mean(calls_with_offer_rate),
            calls_with_accept_rate = mean(calls_with_accept_rate),
            calls_offer_applied_rate = mean(calls_offer_applied_rate),
            call_transfer_rate = mean(call_transfer_rate),
            call_breakage_rate = mean(call_breakage_rate)) %>%
  ungroup() %>%
  mutate(agent_name = as.factor(agent_name) %>% fct_rev())  %>%# Reverse order
  rename(`Offer Rate`= calls_with_offer_rate,
         `Accept Rate` = calls_with_accept_rate,
         `Applied Rate` = calls_offer_applied_rate,
         `Transfer Rate` = call_transfer_rate,
         `Breakage Rate` = call_breakage_rate) %>%
  gather(metric, pct, -agent_name) %>%
  mutate(metric = as.factor(metric) %>% fct_relevel("Offer Rate", "Accept Rate",
                                                    "Applied Rate", "Breakage Rate","Transfer Rate")) 

# Top Agents
top_agents_tbl <- team_data_grouped %>%
  select(agent_name, calls, calls_offer_applied) %>%
  mutate(agent_name = as_factor(agent_name) %>% fct_lump(n = 10, w = calls)) %>%
  group_by(agent_name) %>%
  summarize(total_calls = sum(calls),
            offers_applied = sum(calls_offer_applied)) %>%
  ungroup() %>%
  mutate(agent_name = agent_name %>% fct_reorder(offers_applied),
         agent_name = agent_name %>% fct_relevel("Other", after = 0)) %>%
  arrange(desc(agent_name)) %>%
  # Offer Applied Text
  mutate(offers_applied_text = scales::number(offers_applied, big.mark = ",")) %>%
  # Cumulative Percent
  mutate(cum_pct = cumsum(offers_applied) / sum(offers_applied),
         cum_pct_text = scales::percent(cum_pct)) %>%
  # Rank
  mutate(rank = row_number(),
         rank = case_when(
           rank == max(rank) ~ NA_integer_,
           TRUE ~ rank
         )) %>%
  # Label text
  mutate(label_text = str_glue("Rank: {rank}\nOffers Applied: {offers_applied_text}\nCumPct: {cum_pct_text}"))

# 3-Week Rolling Average 
### Dates need to be aligned at last day of the week
rolling_avg_3_week_tbl <- team_data_grouped %>%
  select(call_date, team_lead_name, agent_name, calls_offer_applied) %>%
  mutate(call_date = ymd(call_date),
         week_end = ceiling_date(call_date, unit = "week") - period(1, unit = "day")) %>%
  group_by(team_lead_name, agent_name, week_end) %>%
  summarize(total_offers_applied = sum(calls_offer_applied)) %>% 
  # Rolling Avg
  mutate(rolling_avg_3 = rollmean(total_offers_applied, k=3, na.pad = TRUE, align="right")) %>%
  ungroup() 


# Picker Input: Date Range
date_range <- dateRangeInput(
  inputId = "date_range",
  label   = NULL,
  start   = min(team_data_grouped$call_date),
  end     = max(team_data_grouped$call_date),
  min     = min(team_data_grouped$call_date),
  max     = max(team_data_grouped$call_date),
  format  = "m-d-yy",
  width = "100%"
)

# Picker Input: Team Lead Name
picker_lead <- pickerInput(
  inputId  = "picker_lead",
  label    = NULL,
  choices  = sort(unique(team_data_grouped$team_lead_name)),
  selected = unique(team_data_grouped$team_lead_name),
  multiple = FALSE, # Allow multiple options
  options  = list(size = 10),
  width = "100%"
)

# Picker Input: Agent Name
picker_agent <- pickerInput(
  inputId  = "picker_agent",
  label    = NULL,
  choices  = NULL,
  multiple = TRUE,
  options  = list(
    `actions-box` = TRUE,  # Note back ticks
    size = 10,
    `selected-text-format` = "count > 3"),
  width = "100%"
)

# Action Button
action_button <- actionButton(inputId = "apply", 
             label   = NULL, 
             icon    = icon("play"),
             width   = '100%')



# Shiny App Layout
shinyApp(
  
  # UI 
  ui = bs4DashPage(
    
    # Nav Bar
    navbar = bs4DashNavbar(
      fixed = FALSE,
      skin = "dark",
      status = "dark",
      border = FALSE
    ),
    
    # Side Bar
    sidebar_collapsed = TRUE,
    sidebar = bs4DashSidebar(
      skin = "dark",
      status = "secondary",
      brandColor = "secondary",
      elevation = 3,
      opacity = 0.8,
      
      # Side Bar Menu
      bs4SidebarMenu(
        # bs4SidebarHeader(""),
        bs4SidebarMenuItem(
          text = "Agent",
          tabName = "agent",
          icon = "users"
          )
        ,
        bs4SidebarMenuItem(
          text = "Lead",
          tabName = "lead",
          icon = "user")
        )),
    
    # Body
    bs4DashBody(
      
      # Include CSS file
      includeCSS("custom.css"),
      
      # Tab 1: Agent Performance
      bs4TabItems(
        
        bs4TabItem(
          tabName = "agent",
          
          # Row 1: Dropdowns
          fluidRow(
            bs4InfoBox(title = h6("Date"),
                       icon = "calendar",
                       iconElevation = 0,
                       width = 3,
                       elevation = 2,
                       status = "dark",
                       value = date_range),
            bs4InfoBox(title = h6("Lead"),
                       icon = "user",
                       iconElevation = 0,
                       width = 3,
                       elevation = 2,
                       status = "dark",
                       picker_lead),
            bs4InfoBox(title = h6("Agent"),
                       icon = "users",
                       iconElevation = 0,
                       width = 3,
                       elevation = 2,
                       status = "dark",
                       picker_agent),
            bs4InfoBox(title = h6("Apply"),
                       icon = "check",
                       iconElevation = 0,
                       width = 3,
                       elevation = 2,
                       status = "dark",
                       h2(action_button))
          ),
         
          # Row 2: Offers
          fluidRow(
            bs4Card(title = "Offers",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    headerBorder = FALSE,
                    solidHeader = TRUE,
                    gradientColor = "dark",
                    closable = FALSE,
                    elevation = 2,
                    maximizable = TRUE,
                    status = "dark",
                    overflow = FALSE,
                    width = 12,
                    labelStatus = "transparent",
                    labelText = "🔑️️",
                    labelTooltip = "Green = Good | Yellow = OK | Red = Bad",
                    # Row 2.1: Call Offer Metrics
                    fluidRow(
                      column(width = 3,
                             descriptionBlock(number = h3(textOutput("offer_given_count")),  
                                              number_icon = "file-invoice",
                                              header   = "Calls with Offer",
                                              number_color = "info",
                                              right_border = FALSE,
                                              margin_bottom = FALSE)),
                      column(width = 3,
                             descriptionBlock(number = h3(textOutput("offer_accept_count")),  
                                              number_icon = "file-invoice",
                                              header   = "Calls with Offer Accepted",
                                              number_color = "info",
                                              right_border = FALSE,
                                              margin_bottom = FALSE)),
                      column(width = 3,
                             descriptionBlock(number = h3(textOutput("offer_applied_count")),  
                                              number_icon = "file-invoice",
                                              header   = "Calls with Offer Applied",
                                              number_color = "info",
                                              right_border = FALSE,
                                              margin_bottom = FALSE)),
                      column(width = 3,
                             descriptionBlock(number = h3(textOutput("call_breakage_count")),  
                                              number_icon = "file-invoice",
                                              header   = "Calls with Breakage",
                                              number_color = "info",
                                              right_border = FALSE,
                                              margin_bottom = FALSE))),
                    # Row 2.2: Call Offer Gauges
                    fluidRow(
                      column(width = 3,
                             descriptionBlock(number = gaugeOutput(outputId = "offer_given_rate_gauge",
                                                                   width = "100%",
                                                                   height = "100%"),
                                              header   = "Offer Rate",
                                              number_color = "secondary",
                                              right_border = FALSE,
                                              margin_bottom = FALSE)),
                      column(width = 3,
                             descriptionBlock(number = gaugeOutput(outputId = "offer_accept_rate_gauge",
                                                                   width = "100%",
                                                                   height = "100%"),
                                              header   = "Offer Accepted Rate",
                                              number_color = "secondary",
                                              right_border = FALSE,
                                              margin_bottom = FALSE)),
                      column(width = 3,
                             descriptionBlock(number = gaugeOutput(outputId = "offer_applied_rate_gauge",
                                                                   width = "100%",
                                                                   height = "100%"),
                                              header   = "Offer Applied Rate",
                                              number_color = "secondary",
                                              right_border = FALSE,
                                              margin_bottom = FALSE)),
                      column(width = 3,
                             descriptionBlock(number = gaugeOutput(outputId = "call_breakage_rate_gauge",
                                                                   width = "100%",
                                                                   height = "100%"),
                                              header   = "Breakage Rate",
                                              number_color = "secondary",
                                              right_border = FALSE,
                                              margin_bottom = FALSE)))
                    
                      )
            ),
          
          
          # Row 3: Value Boxes
          fluidRow(
            # bs4ValueBox(
            #   elevation = 4,
            #   value = h4(valueBoxOutput("calls_by_day_avg", "")),
            #   subtitle = "Calls Per Day (Avg)",
            #   status = "primary",
            #   icon = "phone"
            # ),
            bs4ValueBox(
              elevation = 2,
              width = 4,
              value = h4(bs4ValueBoxOutput("handle_time_avg", "")),
              subtitle = "Handle Time (Avg in Mins)",
              status = "dark",
              icon = "clock"
            ),
            bs4ValueBox(
              elevation = 2,
              width = 4,
              value = h4(bs4ValueBoxOutput("call_regen_by_agent", "")),
              subtitle = "Regeneration Rate",
              status = "dark",
              icon = "reply"
            ),
            bs4ValueBox(
              elevation = 2,
              width = 4,
              value = h4(bs4ValueBoxOutput("transfer_rate_avg", "")),
              subtitle = "Transfer Rate",
              status = "dark",
              icon = "exchange-alt"
            )
          ),
          
          
          # Row 4: Calls by Day Plot
          fluidRow(
            # Row 3.1 Calls by Day Plot
            bs4Card(title = "Calls",
                    collapsible = TRUE,
                    collapsed = FALSE,
                    headerBorder = FALSE,
                    solidHeader = TRUE,
                    gradientColor = "dark",
                    closable = FALSE,
                    elevation = 2,
                    maximizable = TRUE,
                    status = "primary",
                    overflow = FALSE,
                    width = 12,
                    # height = "90%",
                    labelStatus = "transparent",
                    labelText = "🔑️️",
                    labelTooltip = "The Data is Cyan. The Average is Blue.",
                    
                    fluidRow(
                      column(width = 10,
                             plotlyOutput("calls_per_plot")),
                      column(width = 2,
                             bs4ValueBox(
                               elevation = 2,
                               width = "100%",
                               value = h4(valueBoxOutput("calls_by_day_avg", "")),
                               subtitle = "Average Per Day",
                               status = "secondary",
                               icon = "chart-line"),
                             br(),
                             bs4ValueBox(
                               elevation = 2,
                               width = "100%",
                               value = h4(valueBoxOutput("calls_by_day_total", "")),
                               subtitle = "Total Calls",
                               status = "secondary",
                               icon = "chart-bar")
                             ))
                    )
                  )
          
          
        ),
        
        ##### TAB 2: LEAD
        bs4TabItem(
          tabName = "lead",
          
          # Row 1: 
          fluidRow(
            bs4Card(
              title = "Call Rates",
              collapsible = TRUE,
              collapsed = FALSE,
              headerBorder = FALSE,
              solidHeader = TRUE,
              gradientColor = "dark",
              closable = FALSE,
              elevation = 2,
              maximizable = TRUE,
              status = "primary",
              overflow = FALSE,
              labelStatus = "transparent",
              labelText = "🔑️️",
              labelTooltip = "",
              plotlyOutput("heatmap")
            ),
            bs4Card(
              title = "Top Agents by Offers Applied",
              width = 6,
              collapsible = TRUE,
              collapsed = FALSE,
              headerBorder = FALSE,
              solidHeader = TRUE,
              gradientColor = "dark",
              closable = FALSE,
              elevation = 2,
              maximizable = TRUE,
              status = "primary",
              overflow = FALSE,
              labelStatus = "transparent",
              labelText = "🔑️️",
              labelTooltip = "",
              plotlyOutput("top_agents")
            )
          ),
          fluidRow(
            bs4Card(
              title = "Call Rates",
              width = 12, 
              collapsible = TRUE,
              collapsed = FALSE,
              headerBorder = FALSE,
              solidHeader = TRUE,
              gradientColor = "dark",
              closable = FALSE,
              elevation = 2,
              maximizable = TRUE,
              status = "primary",
              overflow = FALSE,
              labelStatus = "transparent",
              labelText = "🔑️️",
              labelTooltip = "",
              plotOutput("rolling_avg")
            )
          )
        )
        
      )
    ),
    footer = bs4DashFooter(
      copyrights = a(href = "https://www.linkedin.com/in/joonhoim/", 
                     target = "_blank", "@Joon"))
  ),
  
  # Server
  server = function(session, input, output) {
    
    ###### PAGE: AGENT
    ###### ROW 1: DROPDOWN
    # Observe TL Dropdown, then update Agent Dropdown
    observe({
      print(input$picker_lead)
      updatePickerInput(session = session, 
                        inputId = "picker_agent", 
                        label = NULL, 
                        choices = team_data_grouped %>% 
                          filter(team_lead_name == input$picker_lead) %>%
                          select(agent_name) %>% 
                          rename(`👤` = agent_name) %>%   # Emoji hack for unnecessary row  
                          unique(),
                        choicesOpt = list(
                          style = rep(("color: white; font-weight: normal;"), 10)))
    })
    
    ###### ROW 2: OFFERS 
    ### CALLS WITH OFFER GIVEN
    # Description Block: Calls with Offer Given Count
    output$offer_given_count <- renderText(processed_data_filtered() %>%
                                               # group_by(agent_name) %>%
                                               summarize(calls_with_offer = sum(calls_with_offer)) %>%
                                               pull(calls_with_offer) %>%
                                               formatC(format="f", big.mark=",", digits=0))
    
    
    # Gauge: Calls with Offer Given Rate Gauge
    output$offer_given_rate_gauge <- renderGauge({
      gauge(min = 0,
            max = 100,
            symbol = '%',
            gaugeSectors(success = c(90, 100), 
                         warning = c(75, 90),
                         danger  = c(0, 75),
                         colors = c("#20c997", "#c99720", "#c92052")),
            value = (processed_data_filtered() %>%
                       summarize(calls_with_offer_rate = mean(calls_with_offer_rate)) %>%
                       pull(calls_with_offer_rate) %>%
                       round(3)) * 100)
    })
    
    ### CALLS WITH OFFER ACCEPTED
    # Description Block: Calls with Offer Accepted Count
    output$offer_accept_count <- renderText(processed_data_filtered() %>%
                                             summarize(calls_with_accept = sum(calls_with_accept)) %>%
                                             pull(calls_with_accept) %>%
                                             formatC(format="f", big.mark=",", digits=0))

    # Gauge: Calls with Offer Accepted Rate Gauge
    output$offer_accept_rate_gauge <- renderGauge({
      gauge(min = 0,
            max = 100,
            symbol = '%',
            gaugeSectors(success = c(75, 100), 
                         warning = c(65, 75),
                         danger  = c(0, 65),
                         colors = c("#20c997", "#c99720", "#c92052")),
            value = (processed_data_filtered() %>%
                       summarize(calls_with_accept_rate = mean(calls_with_accept_rate)) %>%
                       pull(calls_with_accept_rate) %>%
                       round(3)) * 100)
    })
    
    
    ### CALLS WITH OFFER APPLIED RATE 
    # Description Block: Calls With Offer Applied Count
    output$offer_applied_count <- renderText(processed_data_filtered() %>%
                                               summarize(calls_offer_applied = sum(calls_offer_applied)) %>%
                                               pull(calls_offer_applied) %>%
                                               formatC(format="f", big.mark=",", digits=0))
    
    # Gauge: Offer Applied Rate
    output$offer_applied_rate_gauge <- renderGauge({
      gauge(min = 0,
            max = 100,
            symbol = '%',
            gaugeSectors(success = c(50, 100), 
                         warning = c(40, 50),
                         danger  = c(0, 40),
                         colors = c("#20c997", "#c99720", "#c92052")),
            value = (processed_data_filtered() %>%
                       summarize(calls_offer_applied_rate = mean(calls_offer_applied_rate)) %>%
                       pull(calls_offer_applied_rate) %>%
                       round(3)) * 100)
    })
    
    ### CALLS WITH BREAKAGE 
    # Description Block: Calls with Breakage Count
    output$call_breakage_count <- renderText(processed_data_filtered() %>%
                                               summarize(call_breakage_count = sum(call_breakage_count)) %>%
                                               pull(call_breakage_count) %>%
                                               formatC(format="f", big.mark=",", digits=0))
    
    # Gauge: Calls with Offer Accepted Rate Gauge
    output$call_breakage_rate_gauge <- renderGauge({
      gauge(min = 0,
            max = 100,
            symbol = '%',
            gaugeSectors(success = c(0, 20), 
                         warning = c(20, 30),
                         danger  = c(30, 100),
                         colors = c("#20c997", "#c99720", "#c92052")),
            value = (processed_data_filtered() %>%
                       summarize(call_breakage_rate = mean(call_breakage_rate)) %>%
                       pull(call_breakage_rate) %>%
                       round(3)) * 100)
    })
    
  
    ##### REACTIVE EVENTS
    ### Waits until a button (Apply) is clicked to run reactive code; find repetitive code and put here
    processed_data_filtered_tbl <- eventReactive(
      eventExpr = req(input$apply),

      valueExpr = {

        team_data_grouped %>%

          # Date Range filter
          filter(call_date %>% between(left = as_datetime(input$date_range[1]),
                                       right = as_datetime(input$date_range[2]))) %>%

          # Agent Filter
          filter(agent_name %in% input$picker_agent)

      },
      ignoreNULL = FALSE  # Don't pass data as default: run code when app loads
    )
    
    
    # Reactive Event: Summarized Processed Filtered Table
    processed_data_filtered <- eventReactive(
      eventExpr = req(input$apply),
      
      valueExpr = {
        
        team_data_grouped %>%
          
          # Date Range filter
          filter(call_date %>% between(left = as_datetime(input$date_range[1]),
                                       right = as_datetime(input$date_range[2]))) %>%
          
          # Agent Filter
          filter(agent_name %in% input$picker_agent) %>%
          
          group_by(agent_name) %>%
          summarize(calls_avg = mean(calls),
                    calls_total = sum(calls),
                    handle_time_avg = mean(handle_time_avg / 60),
                    call_regen = mean(call_regen_rate),
                    call_transfer_rate = mean(call_transfer_rate),
                    call_breakage_rate = mean(call_breakage_rate),
                    calls_offer_applied_rate = mean(calls_offer_applied_rate),
                    calls_with_offer = sum(calls_with_offer),
                    calls_with_offer_rate = mean(calls_with_offer_rate),
                    calls_with_accept = sum(calls_with_accept),
                    calls_with_accept_rate = mean(calls_with_accept_rate),
                    call_breakage_count = sum(call_breakage_count),
                    call_breakage_rate = mean(call_breakage_rate),
                    calls_offer_applied = sum(calls_offer_applied),
                    calls_offer_applied_rate = mean(calls_offer_applied_rate)
                    )
        
      },
      ignoreNULL = FALSE  # Don't pass data as default: run code when app loads
    )
    
    
    ###### REACTIVE VALUES
    # Value Box: Average Calls by Day
    output$calls_by_day_avg <- renderText(processed_data_filtered() %>%
                                                summarise(calls_avg = sum(calls_avg)) %>%
                                                pull(calls_avg) %>%
                                                formatC(format="f", big.mark=",", digits=0))
    
    # Value Box: Total Calls by Day
    output$calls_by_day_total <- renderText(processed_data_filtered() %>%
                                            summarise(calls_total = sum(calls_total)) %>%
                                            pull(calls_total) %>%
                                            formatC(format="f", big.mark=",", digits=0))
    
    # Value Box: Avg Handle Time 
    output$handle_time_avg <- renderText(processed_data_filtered() %>%
                                           summarise(handle_time_avg = mean(handle_time_avg)) %>%
                                           pull(handle_time_avg) %>%
                                           formatC(format="f", big.mark=",", digits=2))
    
    # Value Box: Call Regeneration Rate
    output$call_regen_by_agent <- renderText(processed_data_filtered() %>%
                                               summarise(call_regen = mean(call_regen)) %>%
                                               pull(call_regen) %>%
                                               round(3) %>%
                                               scales::percent())  
    
    # Value Box: Avg Transfer Rate
    output$transfer_rate_avg <- renderText(processed_data_filtered() %>%
                                             summarise(call_transfer_rate = mean(call_transfer_rate)) %>%
                                             pull(call_transfer_rate) %>%
                                             round(3) %>%
                                             scales::percent())  
    
    # Value Box: Avg Breakage Rate
    output$breakage_rate_avg <- renderText(processed_data_filtered() %>%
                                             summarise(call_breakage_rate = mean(call_breakage_rate)) %>%
                                             pull(call_breakage_rate) %>%
                                             round(3) %>%
                                             scales::percent())  
    
    ### OFFER APPLIED 
    # Info Box: Offer Applied Rate
    output$offer_applied_rate <- renderText(str_glue("Offer Applied Rate: ",  processed_data_filtered() %>%
                                             summarise(calls_offer_applied_rate = mean(calls_offer_applied_rate)) %>%
                                             pull(calls_offer_applied_rate) %>%
                                             round(3) %>%
                                             scales::percent())) 

    ###### PLOTS
    ### CALLS
    # Plot: Calls Per Day
    output$calls_per_plot <- renderPlotly(
      ggplotly(
        processed_data_filtered_tbl() %>%
          group_by(call_date) %>%
          summarize(calls = sum(calls)) %>%
          mutate(label_text = str_glue("Date: {call_date}
                                Calls: {calls}")) %>%
          ggplot(aes(x=call_date, y=calls)) +
          geom_line(color = "#20c997", alpha=0.5) + # Actual Rate
          geom_point(aes(text = label_text), color = "#20c997", size = 0.1, alpha=0) +
          geom_smooth(method = "loess", span = 0.2) + 
          theme_excel_new() + 
          theme(axis.text = element_text(size = 11,
                                         colour = "white"),
                text = element_text(family = "Roboto-Regular")),
        tooltip = "text") %>%
        layout(plot_bgcolor= "transparent") %>% 
        layout(paper_bgcolor= "transparent"))
    
    
    ###### PAGE 2: OVERVIEW
    output$heatmap <- renderPlotly(
      ggplotly(
        pct_calls_by_agent %>%
          mutate(label_text = str_glue("Agent: {agent_name}
                                {metric}: {pct}")) %>%
          ggplot(aes(metric, agent_name)) +
          geom_tile(aes(fill = metric, alpha=pct),
                    color = "#f8f9fa") +
          geom_text(aes(label = scales::percent(pct)),
                    size = 3,
                    color = "white") +

          scale_x_discrete("", expand = c(0, 0)) +
          scale_y_discrete("", expand = c(0, 0)) +
          theme_grey(base_size = 10) +
          theme(legend.position = "none",
                axis.ticks.y = element_line(colour = "white"),
                axis.text = element_text(size = 8, colour = "white"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.spacing = unit(0.75, "cm"),
                # panel.background = element_rect(fill = "transparent"),
                # plot.background = element_rect(fill = "transparent", color = NA)
                ) +
          scale_fill_tableau()) %>%
          layout(plot_bgcolor= "transparent") %>%
          layout(paper_bgcolor= "transparent")
    )
    
    
    # Top Agents
    output$top_agents <- renderPlotly(
      ggplotly(
        top_agents_tbl %>%
          ggplot(aes(offers_applied, agent_name)) +
          geom_point(color = "white",
                     aes(text = label_text)) +
          geom_segment(aes(xend = 0, yend=agent_name),
                       color = "white") +
          # geom_label(aes(label = label_text),
          #            hjust = "inward",
          #            size = 3,
          #            color = "white") +
          # scale_x_log10() +
          theme_grey(base_size = 10) + 
          theme(legend.position = "none",
                axis.ticks = element_blank(),
                axis.text = element_text(size = 8, colour = "white"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent"),
                plot.background = element_rect(fill = "transparent", color = NA)) +
          labs(x="", y=""),
        tooltip = "text"
      ) %>%
        layout(plot_bgcolor= "transparent") %>% 
        layout(paper_bgcolor= "transparent")
    )
    
    
    # Top Agents
    output$top_agents <- renderPlotly(
      ggplotly(
        top_agents_tbl %>%
          ggplot(aes(offers_applied, agent_name)) +
          geom_point(color = "white",
                     aes(text = label_text)) +
          geom_segment(aes(xend = 0, yend=agent_name),
                       color = "white") +
          # geom_label(aes(label = label_text),
          #            hjust = "inward",
          #            size = 3,
          #            color = "white") +
          # scale_x_log10() +
          theme_grey(base_size = 10) + 
          theme(legend.position = "none",
                axis.ticks = element_blank(),
                axis.text = element_text(size = 8, colour = "white"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent"),
                plot.background = element_rect(fill = "transparent", color = NA)) +
          labs(x="", y=""),
        tooltip = "text"
      ) %>%
        layout(plot_bgcolor= "transparent") %>% 
        layout(paper_bgcolor= "transparent")
    )
    
    
    # 3-week Rolling Average
    output$rolling_avg <- renderPlot(
    # ggplotly(
        rolling_avg_3_week_tbl %>%
          rename(`Team Lead` = team_lead_name) %>%
          ggplot(aes(week_end, total_offers_applied, color=`Team Lead`)) + 
          geom_point() +
          geom_line(aes(y=rolling_avg_3), color="blue", size=0.5) + 
          facet_wrap(~agent_name, scales = "free_y") + 
          labs(x="",y="Offers Applied by Week") + 
          theme_tq() + 
          theme(panel.spacing = unit(0.75, "cm"),
                legend.position = "none",
                axis.ticks = element_blank(),
                axis.text = element_text(size = 10, colour = "white"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "transparent"),
                plot.background = element_rect(fill = "transparent", color = NA)) + 
          scale_color_tableau()
      )
    # )
    

    
    
    
    
    }
)


    
    
    
    










