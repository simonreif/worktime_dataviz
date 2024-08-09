library(here) # Get relative paths
library(tidyverse) # Data wrangling
library(data.table) # Data wrangling
library(lubridate) # Working with dates
library(RColorBrewer) # Nicer Color Schemes
library(ggpubr) # Combine Graphs
library("xkcd") # Comic graphs

# Define Graphs in Comic style
theme_xkcd <- theme(
  panel.background = element_rect(fill="white"), 
  axis.ticks = element_line(colour=NA),
  panel.grid = element_line(colour="white"),
  axis.text.y = element_text(colour="black"), 
  axis.text.x = element_text(colour="black"),
  text = element_text(size=10, family="xkcd Script"),
  plot.title = element_text(hjust = 0.5),
  legend.text=element_text(size=11)
)

# Get data
timetrack_df <- read_csv("/Users/srf/Dropbox/1_orga/Timetracking/Clockify_Time_Report_Detailed_01_01_2023-12_31_2023.csv")
timetrack_old_df <- read_csv("/Users/srf/Dropbox/1_orga/Timetracking/Clockify_Time_Report_Detailed_01_01_2022-12_31_2022.csv")

# Clean Data old years
timetrack_old_mod <- timetrack_old_df %>%
  # Make indicators for the 6 main task areas
  mutate(task_area = "",
         task_area = ifelse(substr(Project,1,4)=="Orga","Orga",task_area),
         task_area = ifelse(substr(Project,1,7)=="Project","Project",task_area),
         task_area = ifelse(substr(Project,1,8)=="Research","Research",task_area),
         task_area = ifelse(substr(Project,1,8)=="Learning","Learning",task_area),
         task_area = ifelse(substr(Project,1,8)=="Teaching","Teaching",task_area),
         task_area = ifelse(substr(Project,1,8)=="Outreach","Outreach",task_area)
  ) %>%
  # Make dow and month indicators
  mutate(date = as.Date(`Start Date`, "%m/%d/%Y"),
         week = week(date),
         month = month(date),
         dow = wday(date),
         start_dt = as.POSIXct(paste(`Start Date`,`Start Time`, sep = " "), format="%m/%d/%Y%H:%M:%S", tz = "UTC"),
         end_dt = as.POSIXct(paste(`End Date`,`End Time`, sep = " "), format="%m/%d/%Y%H:%M:%S", tz = "UTC")
  ) %>% # Rename Orga
  mutate(task_area = ifelse(task_area == "Orga","Admin", task_area))


# Clean Data current year
timetrack_mod <- timetrack_df %>%
  # Make indicators for the 6 main task areas
  mutate(task_area = "",
         task_area = ifelse(substr(Project,1,4)=="Orga","Orga",task_area),
         task_area = ifelse(substr(Project,1,7)=="Project","Project",task_area),
         task_area = ifelse(substr(Project,1,8)=="Research","Research",task_area),
         task_area = ifelse(substr(Project,1,8)=="Learning","Learning",task_area),
         task_area = ifelse(substr(Project,1,8)=="Teaching","Teaching",task_area),
         task_area = ifelse(substr(Project,1,8)=="Outreach","Outreach",task_area)
         ) %>%
  # Make dow and month indicators
  mutate(date = as.Date(`Start Date`, "%m/%d/%Y"),
         week = week(date),
         month = month(date),
         dow = wday(date),
         start_dt = as.POSIXct(paste(`Start Date`,`Start Time`, sep = " "), format="%m/%d/%Y%H:%M:%S", tz = "UTC"),
         end_dt = as.POSIXct(paste(`End Date`,`End Time`, sep = " "), format="%m/%d/%Y%H:%M:%S", tz = "UTC")
  ) %>% # Rename Orga
  mutate(task_area = ifelse(task_area == "Orga","Admin", task_area))

#### Minutes per Hour of the Day ####
start <- seq(as.POSIXct("2023-01-01 0:00:00", tz = "UTC"), as.POSIXct("2023-12-31 23:00:00", tz="UTC"), by="hour")
end <- seq(as.POSIXct("2023-01-01 1:00:00", tz = "UTC"), as.POSIXct("2024-01-01 00:00:00", tz="UTC"), by="hour")
df_times <- data.table(start, end)
setkey(df_times, start, end)

# Make data table with actual start-end times
timetrack_hours_dt <- data.table(timetrack_mod %>%
                                   select(start_dt,end_dt))

# Data Table solution giving weired values
 df <- foverlaps(timetrack_hours_dt, df_times, by.x = c('start_dt', 'end_dt'), by.y = c('start', 'end'))

# Get minutes per hour 
spells_df <- as.data.frame(df)
spells_df <- spells_df %>%
  rowwise() %>%
  mutate(end_count = min(end, end_dt),
         start_count = max(start, start_dt))

spells_df$diff = spells_df$end_count - spells_df$start_count

spells_df$diff_min = round(as.integer(spells_df$diff)/60)
spells_df <- spells_df %>%
  mutate(start_hour = as.character(start)) %>%
  mutate(start_hour_str = substr(start_hour,12,13)) %>%
  select(start_hour_str, diff_min) %>%
  group_by(start_hour_str) %>%
  summarise(minutes_worked = sum(diff_min))
    
spells_df <- spells_df %>%
  mutate(start_hour_str=ifelse(start_hour_str == "", "00", start_hour_str)) %>%
  mutate(start_hour_int = as.integer(start_hour_str))

# Make nice graph
set.seed(123) # for reproducibility

# Coordinates for arrow
dff <- data.frame(x1 = 16, x2 = 12, y1 = 11000, y2 = 5000,z=1)
spells_df$xmin <- spells_df$start_hour_int - 0.35
spells_df$xmax <- spells_df$start_hour_int + 0.35
xrange <- range(min(spells_df$start_hour_int)-0.1, max(spells_df$start_hour_int) + 0.1)
yrange <- range(min(spells_df$minutes_worked), max(spells_df$minutes_worked) + 1000)
set.seed(123) # for reproducibility
p_day <- ggplot(data=spells_df, aes(x=start_hour_int, y=minutes_worked)) +
  xkcdrect(data=spells_df, aes(xmin=xmin,ymin=0,xmax=xmax,ymax=minutes_worked)) +
  theme_xkcd +
  xlab("") + ylab("") +
  xkcdaxis(xrange,yrange) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) +
  scale_x_continuous(breaks = c(0,3,6,9,12,15,18,21)) +
  annotate("text", x = 17.95, y = c(11420, 9100), label = c("A lunch-break in", "the distribution"),family="xkcd Script",colour="red") +
  ggtitle("By hour of the day") + 
  geom_curve(aes(x = x1, y = y1, xend = x2, yend = y2,group=z),data=dff,
             colour="red", 
             size=0.8, 
             curvature = +0.2,
             arrow = arrow(length = unit(0.05, "npc")))+ theme(legend.position="none")


#### Time per Task Area ####
task_area_hours <- timetrack_mod %>%
  group_by(task_area) %>%
  summarise(hours = sum(`Duration (decimal)`)) 

task_area_hours_old <- timetrack_old_mod %>%
  group_by(task_area) %>%
  summarise(hours = sum(`Duration (decimal)`)) 

task_area_hours_old <- left_join(task_area_hours_old %>% mutate(hours_old = hours), task_area_hours, by="task_area")
task_area_hours_old <- task_area_hours_old %>% mutate(
  change = hours.y - hours_old,
  rel_hours.y = hours.y/sum(task_area_hours_old$hours.y),
  rel_hours_old = hours_old/sum(task_area_hours_old$hours_old),
  rel_change = rel_hours.y - rel_hours_old ) %>%
  select(task_area, change, rel_change)

datalines <- data.frame(xbegin=-150,ybegin=-150,
                        xend=-650, yend=-650)

# Bar chart for change
p_change <- ggplot(data=task_area_hours_old) +
  geom_col(aes(rel_change, task_area, fill = task_area)) +
  theme_xkcd +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none",
        legend.title = element_blank()) +
  xlab("") + ylab("") + ggtitle("Shares compared to last year") + 
  scale_x_continuous(
    breaks = c(-0.1, 0.1),
    label = c("less time", "more time")
  ) + 
  scale_fill_brewer(palette = "Set3") +
  xkcdline(aes(x=0,y=0,xend=0,yend=7),
           datalines, xjitteramount = 0.001) 


# Pie Chart
p_areas <- ggplot(data=task_area_hours, aes(x="", y=hours, fill=task_area)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_xkcd +
  theme(axis.text.x = element_blank(),
        legend.title = element_blank()) +
  xlab("") + ylab("") + 
  ggtitle("Distribution 2023") + 
  scale_fill_brewer(palette = "Set3") +
  theme(legend.position="bottom")

p_areas <- ggarrange(p_areas, p_change, 
                      nrow=1, heights = c(3, 3)) 

#### Work time per Month ####
month_hours <- timetrack_mod %>%
  group_by(month, task_area) %>%
  summarise(hours = sum(`Duration (decimal)`)) %>%
  mutate(month_str = as.character(month),
         month_str = ifelse(substr(month_str,2,2)=="",paste0("0",month_str),month_str))

p_year <- ggplot(data=month_hours, aes(x=month_str, y=hours, fill=task_area)) +
  geom_bar(stat="identity") +
  theme_xkcd +
  theme(axis.text.y = element_blank(),
        legend.position = "none") +
  xlab("") + ylab("") + 
  scale_x_discrete(labels= c("01"="Jan", 
                             "02" = "Feb",
                             "03" = "Mar",
                             "04" = "Apr",
                             "05" = "May",
                             "06" = "Jun",
                             "07" = "Jul",
                             "08" = "Aug",
                             "09" = "Sep",
                             "10" = "Oct",
                             "11" = "Nov",
                             "12" = "Dec")) +
  scale_fill_brewer(palette = "Set3")


#### Hours per weekday ####
dow_hours <- timetrack_mod %>%
  mutate(dow = ifelse(dow==1,8,dow),
         dow = dow-1) %>%
  group_by(dow) %>%
  summarise(hours = sum(`Duration (decimal)`),
            sd = sd(`Duration (decimal)`)) %>%
  mutate(dow_str = as.character(dow))

dow_hours$xmin <- dow_hours$dow - 0.35
dow_hours$xmax <- dow_hours$dow + 0.35
xrange <- range(min(dow_hours$dow)-0.6, max(dow_hours$dow) + 0.1)
yrange <- range(0, max(dow_hours$hours) + 30)

# Plot
p_dow <- ggplot(data=dow_hours, aes(x=dow_str, y=hours)) +
  xkcdrect(data=dow_hours, aes(xmin=xmin,ymin=0,xmax=xmax,ymax=hours)) +
  theme_xkcd  +
  xlab("") + ylab("") +
  xkcdaxis(xrange,yrange) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank()) + 
  scale_x_discrete(labels= c("1"="Mon", 
                             "2" = "Tue",
                             "3" = "Wed",
                             "4" = "Thu",
                             "5" = "Fri",
                             "6" = "Sat",
                             "7" = "Sun")) +
  ggtitle("By day of week") +
  geom_errorbar(aes(ymin=hours-sd, ymax=hours+sd),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))


# Add dataman
# Empty plot
set.seed(123)
h <- rnorm(1000, 160, 10)
w <- rnorm(1000, 0, 10)/(pmin(abs(h-160), 1)) + (h-60)*0.75
d <- data.frame(x=w, y=h)
p <- ggplot(data=d, aes(x=x,y=y)) + geom_point(colour = "white", alpha=0.5, size = 5)

dataman <- data.frame( x= -2000, y=-1000,
                       scale = 1000 ,
                       ratioxy = 0.5,
                       angleofspine = -pi/2 ,
                       anglerighthumerus =  -pi/6,
                       anglelefthumerus =  -pi/2 - pi/6,
                       anglerightradius = pi/5,
                       angleleftradius =  -pi/5,
                       angleleftleg = 3*pi/2 + pi / 12 ,
                       anglerightleg = 3*pi/2 - pi / 12,
                       angleofneck = runif(1, 3*pi/2-pi/10, 3*pi/2+pi/10))

mapping <- aes(x=y, y=y, scale, ratioxy, angleofspine,
                anglerighthumerus, anglelefthumerus,
                anglerightradius, angleleftradius,
                anglerightleg, angleleftleg, angleofneck)

datalines <- data.frame(xbegin=-150,ybegin=-150,
                xend=-650, yend=-650)

p_man <- p + xkcdman(mapping,dataman) + theme_xkcd() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks = element_blank()) +
  xlab("") + ylab("") +
  annotate("text", x=100, y = -800,
           label = "At least in relative terms, \n I get more research time!", family="xkcd Script" ) +
  xkcdline(aes(x=xbegin,y=ybegin,xend=xend,yend=yend),
            datalines, xjitteramount = 140)

##### Comine Graphs #####
p_com1 <- ggarrange(p_man, p_areas,
                    ncol = 2, nrow = 1, widths = c(3,4.5))
p_com2 <- ggarrange(p_dow,  p_year,
                   ncol = 2, nrow = 1)

arranged <- ggarrange(p_com1, p_com2, p_day,
          nrow=3, heights = c(5, 3, 3.3)) 

final <- annotate_figure(arranged, bottom = text_grob("github.com/simonreif/", 
                                      color = "grey", face = "bold", size = 10))

ggsave("timetrack2023.jpg", final, width=2900, height = 2000, units = "px")