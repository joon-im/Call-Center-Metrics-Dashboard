# DS4B 101-R: R FOR BUSINESS ANALYSIS ----
# TIME-BASED MATH ----


# Load libraries
library(tidyverse)     # Data Manipulation
library(lubridate)     # Time Series Manipulation
library(plotly)        # Data Visualization
library(readxl)        # Read Excel Files
library(ggthemes)      # Visualization Themes
library(tidyquant)     # Visualization Themes

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

# Calls
calls_day_tbl <- team_data_grouped %>%
  mutate(call_date = ymd(call_date)) %>%
  group_by(call_date) %>%
  summarise(calls = sum(calls))

calls_weekly_tbl <- calls_tbl %>%
  mutate(week_num = week(call_date)) %>%
  group_by(week_num) %>%
  summarize(calls = sum(calls))

calls_month_tbl <- team_data_grouped %>%
  mutate(call_date = ymd(call_date),
         year = year(call_date),
         month = month(call_date, label=TRUE, abbr=FALSE)) %>%
  group_by(year, month) %>%
  summarise(calls = sum(calls))


# Business Analysis: Measuring Change ----

# Lagging: Difference from most recent observation ----



calculate_pct_diff <- function(data) {
  
  data %>%
    mutate(calls_lag_1 = lag(calls, n=1)) %>%
    
    # Handle NA
    mutate(calls_lag_1 = case_when(
      is.na(calls_lag_1) ~ calls,
      TRUE ~ calls_lag_1
    )) %>%
    
    # Diffs & Pct Diffs
    mutate(diff_1 = calls - calls_lag_1,
           pct_diff_1 = diff_1 / calls_lag_1,
           pct_diff_1_chr = scales::percent(pct_diff_1)) 
}

calculate_pct_diff(calls_weekly_tbl)

# Difference from first observation ----

calls_month_tbl %>%
  mutate(calls_june         = first(calls),
         diff_june          = calls - calls_june,
         pct_diff_june      = diff_june / calls_june,
         pct_diff_june_chr  = scales::percent(pct_diff_june))

# Cumulative Calculations ----

# By Month: Single Year
calls_month_tbl %>%
  mutate(cumulative_calls = cumsum(calls))

# By Month: Compare by Year
calls_month_tbl %>%
  group_by(month) %>%
  mutate(cumulative_calls = cumsum(calls))


# Rolling Window Calculations ----

# Rolling Mean: a simple way to expose the trend by reducing effect of outliers. Don't group moving averages. 

calls_weekly_tbl %>%
  
  mutate(roll_mean_2 = rollmean(calls, 
                                k = 2,  # 2-Week Window Calculation
                                na.pad = TRUE,
                                align = "right",
                                fill = 0),  
         roll_mean_4 = rollmean(calls, 
                                k = 4,  # 4-Week Window Calculation
                                na.pad = TRUE,
                                align = "right",
                                fill = 0))  


# Top N Agents: Offers Applied
n <- 10
top_agents_tbl <- team_data_grouped %>%
  
  select(agent_name, calls, calls_offer_applied) %>%
  
  mutate(agent_name = as_factor(agent_name) %>% fct_lump(n = n, w = calls)) %>%
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

# Top N Agents: Offers Applied
ggplotly(
top_agents_tbl %>%
  ggplot(aes(offers_applied, agent_name)) + 
  geom_point(color = palette_light()[1]) +
  # geom_segment(aes(xend = 0, yend=agent_name), 
  #              color = palette_light()[1]) + 
  # geom_label(aes(label = label_text), 
  #            hjust = "inward",
  #            size = 3,
  #            color = palette_light()[1]) + 
  scale_x_log10() + 
  theme_tq() + 
  scale_color_tq() + 
  theme(legend.position = "none",
        axis.ticks.y = element_line(colour = "white"),
        axis.text = element_text(size = 8, colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  labs(x="Offers Applied", y="")
)



top_agents_tbl %>%
  ggplot(aes(y=offers_applied, x=agent_name)) + 
  geom_bar(color = palette_light()[1], stat="identity", fill="white") + 
  coord_flip() + 
  scale_y_log10() + 
  theme_tq() + 
  scale_color_tq() + 
  theme(legend.position = "none",
        axis.ticks.y = element_line(colour = "white"),
        axis.text = element_text(size = 8, colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)) + 
  labs(x="Offers Applied", y="")

# Call Type Ratio Heatmap 

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
ggplotly(
pct_calls_by_agent %>%
  
  ggplot(aes(metric, agent_name)) + 
  
  geom_tile(aes(fill = metric, alpha=pct)) + 
  
  geom_text(aes(label = scales::percent(pct)),
            size = 3) +
  
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  theme_grey(base_size = 10) + 
  theme(legend.position = "none",
        axis.ticks = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_tableau()
)

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

ggplotly(
rolling_avg_3_week_tbl %>%
  rename(`Team Lead` = team_lead_name) %>%
  ggplot(aes(week_end, total_offers_applied, color=`Team Lead`)) + 
  geom_point() +
  geom_line(aes(y=rolling_avg_3), color="blue", size=0.5) + 
  facet_wrap(~agent_name, scales = "free_y") + 
  labs(x="",y="Offers Applied by Week") + 
  theme_tq() + 
  theme(panel.spacing = unit(0.75, "cm")) + 
  scale_color_tableau()
)

# K-Means

# Get Agent Trends with normalized data
agent_trends_tbl <- team_data_grouped %>%
  
  select(agent_name, calls_with_offer_rate, calls_with_accept_rate,
         call_transfer_rate, call_breakage_rate, call_regen_rate, calls_offer_applied_rate) %>%
  group_by(agent_name) %>%
  summarize(calls_with_offer_rate = mean(calls_with_offer_rate),
            calls_with_accept_rate  = mean(calls_with_accept_rate),
            call_transfer_rate = mean(call_transfer_rate),
            call_breakage_rate = mean(call_breakage_rate),
            call_regen_rate = mean(call_regen_rate),
            calls_offer_applied_rate = mean(calls_offer_applied_rate)) %>%
  ungroup()

# Perform K-means
kmeans_obj <- agent_trends_tbl %>%
  select(-c(agent_name, team_lead_name)) %>%
  kmeans(centers = 3, nstart = 100)

# Cluster values
kmeans_obj$cluster

# Tidy K-means with broom 
library(broom)
tidy(kmeans_obj) %>% glimpse()

# Determine # of clusters: Total within Sum of Squares
glance(kmeans_obj)

# Augment trends with cluster assignment 
augment(kmeans_obj, data = agent_trends_tbl) %>%
  select(agent_name, .cluster)

### How many centers (agent groups) to use? 

# Function that works on 1 element
kmeans_mapper <- function(centers = 3) {
  
  agent_trends_tbl %>%
    select(-agent_name) %>%
    kmeans(centers = centers, nstart = 100)
  
}

# First calculate tot.withinss for many centers
kmeans_mapper(3) %>% glance()

# Map function to many elements
kmeans_mapped_tbl <- tibble(centers = 1:10) %>%
  mutate(k_means = centers %>% map(kmeans_mapper)) %>%
  mutate(glance = k_means %>% map(glance))


# Scree Plot:
library(ggrepel)
kmeans_mapped_tbl %>% 
  unnest(glance) %>%
  select(centers, tot.withinss) %>%
  
  ggplot(aes(centers, tot.withinss)) + 
  geom_point(color = "#2c3e50", size=3) + 
  geom_line(color = "#2c3e50", size=1) + 
  ggrepel::geom_label_repel(aes(label=centers), color = "#2c3e50") + 
  theme_tq() + 
  labs(
    title = "Skree Plot",
    subtitle = "Measures the distance each of the agents are from the closest K-Means center",
    caption = "Based on this Scree Plot, select 3 clusters to segment the agent base.",
    x = "Centers", 
    y = "Total within Sum of Squares"
  )


# UMAP
### Dimensionality reduction: converts many numeric columns into two columns 
library(umap)
umap_obj <- agent_trends_tbl %>%
  select(-agent_name) %>%
  umap::umap()

# Agent by Team Lead
agent_team_lead <- team_data_grouped %>%
  select(agent_name, team_lead_name) %>%
  group_by(agent_name) %>%
  unique()

# UMAP results
umap_results_tbl <- umap_obj$layout %>%
  as_tibble() %>%
  set_names(c("x", "y")) %>%
  bind_cols(agent_trends_tbl %>% select(agent_name)) 

# Get K-Means with center=3 data
kmeans_3_obj <- kmeans_mapped_tbl %>%
  pull(k_means) %>%
  pluck(3)

# Augment K-Means data with clusters
kmeans_3_clusters_tbl <- kmeans_3_obj %>%
  augment(agent_trends_tbl) %>%
  select(agent_name, .cluster)

# Add K-Means to UMAP results
umap_kmeans_3_results_tbl <- umap_results_tbl %>% 
  left_join(kmeans_3_clusters_tbl, by="agent_name") 

umap_kmeans_3_results_lead_tbl <- umap_results_tbl %>% 
  left_join(kmeans_3_clusters_tbl, by="agent_name") %>%
  left_join(agent_team_lead, by="agent_name")


umap_kmeans_3_results_lead_tbl %>%
  mutate(label_text = str_glue("Agent: {agent_name}
                               Lead: {team_lead_name}
                               Cluster: {.cluster}")) %>%
  ggplot(aes(x, y, color=.cluster)) +
  geom_point() + 
  geom_label_repel(aes(label=label_text), size=3) + 
  
  theme_tq() + 
  scale_color_tableau() + 
  theme(legend.position = "none") + 
  labs(title = "Agent Performance Segmentation",
       subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
       caption = "Conclusion: 3 Agent Performance Segments Identified")


# Join cluster data to agent trends data
### Bin the main metric
agent_trends_tbl %>%
  pull(calls_offer_applied_rate) %>%
  quantile(probs = c(0, 0.3, 0.6, 1))

cluster_trends_tbl <- agent_trends_tbl %>%
  left_join(umap_kmeans_3_results_lead_tbl) %>%
  mutate(calls_offer_applied_rate_bin = case_when(
    calls_offer_applied_rate <= 0.4 ~ "low",
    calls_offer_applied_rate <= 0.512 ~ "medium",
    TRUE ~ "high"
  )) %>%
  
  select(.cluster, team_lead_name, agent_name, 
         calls_with_offer_rate:call_regen_rate, -x, -y, 
         calls_offer_applied_rate_bin, calls_offer_applied_rate) %>%
  
  # Summarize offer applied rate by cluster and call attributes
  group_by_at(.vars = vars(.cluster:calls_offer_applied_rate_bin)) %>%
  summarise(avg_offer_applied_rate = mean(calls_offer_applied_rate)) %>%
  ungroup() 
  
  
# Cluster 1: All Team Quinton Agents + Low Offer Applied Rate
get_cluster_trends <- function(cluster) {
  
  cluster_trends_tbl %>% 
    filter(.cluster == cluster) %>%
    arrange(desc(avg_offer_applied_rate)) 
}

get_cluster_trends(1) %>% View()

# Cluster 2: All Team Bill Agents + Medium Offer Applied Rate
get_cluster_trends(2) %>% View()

# Cluster 3: All Team Aceona Agents + High Offer Applied Rate
get_cluster_trends(3) %>% View()


# Cluster Labels Tibble
cluster_label_tbl <- tibble(.cluster = 1:3,
                            .cluster_label = c("Offer Applied Rate: Low",
                                               "Offer Applied Rate: Medium",
                                               "Offer Applied Rate: High")
                            ) %>%
  mutate(.cluster = as_factor(as.character(.cluster)))

# Join Cluster Labels to UMAP 
umap_kmeans_3_results_lead_tbl %>%
  left_join(cluster_label_tbl) %>%
  mutate(label_text = str_glue("Agent: {agent_name}
                               Lead: {team_lead_name}
                               Cluster: {.cluster}
                               {.cluster_label}")) %>%
  ggplot(aes(x, y, color=.cluster)) +
  geom_point() + 
  geom_label_repel(aes(label=label_text), 
                   size=3) + 
  
  theme_tq() + 
  scale_color_tableau() + 
  theme(legend.position = "none") + 
  labs(title = "Agent Performance Segmentation",
       subtitle = "UMAP 2D Projection with K-Means Cluster Assignment",
       caption = "Conclusion: 3 Agent Performance Segments Identified")


