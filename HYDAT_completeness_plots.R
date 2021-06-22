rm(list=ls())
library(tidyverse)
library(plotly) # interactive available data plotting
library(naniar) # for summarizing available data
library(tidyhydat)
library(cowplot) # ggplot subplot

# example list of stations
stn_list <- c("08FC003","08FC005","08GD008","08KE016","08KE032","08KF001","08KG001","08KG003","08LA027","08LB012","08LB024","08LE020",
              "08LE021","08LE075","08LF002","08LF007","08LF027","08LF062","08LF094","08LG006","08LG007","08MA001","08MA003","08MA006",
              "08MB005","08MB006","08MB007","08MC045","08MD035","08ME002","08ME025","08NM138","08NM165","08NM174","08NM176") %>% sort()

# generate a placeholder/container to combine all the data
combined_daily <- tibble(Date = seq.Date(base::as.Date("1867-07-01"), 
                                         base::as.Date("2099-01-01"), 
                                         by = "day")
                         )

combined_annual <- tibble(Year = seq(1867, 2099, 1))
# loop through each station & extract daily flow from HYDAT
for(STN in stn_list) {
  message(STN)
  # individual station extract daily flow
  individual_daily <- hy_daily_flows(station_number = STN) %>% 
    filter(Parameter == "Flow") %>%
    select(Date, Value) %>%
    rename(!!STN := Value)
    
  individual_instMAX <- hy_annual_instant_peaks(station_number = STN) %>%
    filter(Parameter == "Flow", PEAK_CODE == "MAX") %>%
    mutate(Year = lubridate::year(Date)) %>%
    select(Year, Value) %>%
    rename(!!STN := Value)
  
  
  # put in the big dataframe
  combined_daily <- combined_daily %>% left_join(individual_daily, by = "Date")
  combined_annual <- combined_annual %>% left_join(individual_instMAX, by = "Year")
}

# trim  dataframe to range of date with observations
daily_filter_range <- c(combined_daily[rowSums(is.na(combined_daily))==(length(combined_daily)-2),]$Date %>% min(),
                        combined_daily[rowSums(is.na(combined_daily))==(length(combined_daily)-2),]$Date %>% max())

annual_filter_range <- c(combined_annual[rowSums(is.na(combined_annual))==(length(combined_annual)-2),]$Year %>% min(),
                         combined_annual[rowSums(is.na(combined_annual))==(length(combined_annual)-2),]$Year %>% max())

combined_daily_trim <- combined_daily %>% 
  filter(Date >= daily_filter_range[1] &
           Date <= daily_filter_range[2]) %>% 
  mutate(Year = lubridate::year(Date))


combined_annual_trim <- combined_annual %>% 
  filter(Year >= annual_filter_range[1] &
         Year <= annual_filter_range[2]) 


# % missing plotting
# Qdaily
gg_daily <- gg_miss_fct(combined_daily_trim %>% select(-Date), Year) +
                  theme_dark()+
                  theme(panel.background = element_rect(fill = NA), panel.ontop = TRUE)+
                  labs(title = "Daily Flow",
                       y = "WSC Station Number") +
                  scale_x_continuous(expand = c(0, 0), limits = c(lubridate::year(min(combined_daily_trim$Date)),
                                                                  lubridate::year(Sys.Date())))
# Qinstmax
gg_inst <- ggplot(combined_annual_trim %>% 
                    reshape2::melt(id = "Year") %>% 
                    drop_na(value),
                  aes(x = Year, y = variable)
              ) +
              geom_miss_point()+
              theme_light()+
              theme(legend.position = "none") +
              labs(title = "Annual Instantaneous Max Flow",
                   y = "WSC Station Number") +
              guides(color=guide_legend(title="Instantaenous Flow"))

# multiple ggplot on one figure
plot_grid(
  gg_daily, gg_inst,
  labels = c("(i)", "(ii)"), nrow = 2
)
