## Dataset 6

# df6: Events (sents, opens and clicks) related to the marketing email communications
  
# Variables are:
  
#- `ID_EVENT`: identify the feedback event (**Key**);
#- `ID_CLI`: identify the client (*Foreign Key*);
#- `ID_CAMP`: identify the email campaign (*Foreign Key*);
#- `ID_DELIVERY`: identify the delivery;
#- `TYP_EVENT`: identify the feedback event:
  + S = Send;
  + V = Open;
  + C = Click;
  + B = Bounce;
  + E = Error;
#- `EVENT_DATE`: identify the datetime event.

#### FIRST LOOK of df_6 ####

str(df_6_camp_event)
summary(df_6_camp_event)

#### START CLEANING df_6 ####

df_6_camp_event_clean <- df_6_camp_event

#### CLEANING DATA TYPES in df_6 ####

## formatting dates and times ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(EVENT_DATETIME = as.POSIXct(EVENT_DATE, format="%Y-%m-%dT%H:%M:%S")) %>%
  mutate(EVENT_HOUR = hour(EVENT_DATETIME)) %>%
  mutate(EVENT_DATE = as.Date(EVENT_DATETIME))

#### CONSISTENCY CHECK ID_CLI in df_1/df_6 ####

cons_idcli_df1_df6 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  distinct() %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CLI) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_6) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df6

#!!! NOTE: all ID_CLI in df_6 are mapped in df_1, but not all ID_CLI in df_1 are mapped in df_6 !!!#  

#### CONSISTENCY CHECK ID_CAMP in df_5/df_6 ####

cons_idcamp_df5_df6 <- df_5_camp_cat_clean %>%
  select(ID_CAMP) %>%
  distinct() %>%
  mutate(is_in_df_5 = 1) %>%
  distinct() %>%
  full_join(df_6_camp_event_clean %>%
              select(ID_CAMP) %>%
              distinct() %>%
              mutate(is_in_df_6 = 1) %>%
              distinct()
            , by = "ID_CAMP"
  ) %>%
  group_by(is_in_df_5, is_in_df_6) %>%
  summarise(NUM_ID_CAMPs = n_distinct(ID_CAMP)) %>%
  as.data.frame()

cons_idcamp_df5_df6

#!!! NOTE: all ID_CAMP in df_6 are mapped in df_5, but not all ID_CAMP in df_5 are mapped in df_6 !!!#

#### RESHAPING df_6 ####

## remapping TYPE_EVENT values "E" [ERROR] and "B" [BOUNCE] into a level "F" [FAILURE] ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  mutate(TYP_EVENT = as.factor(if_else(TYP_EVENT == "E" | TYP_EVENT == "B", "F", as.character(TYP_EVENT))))

## adding type from df_5 ##
df_6_camp_event_clean <- df_6_camp_event_clean %>%
  left_join(df_5_camp_cat_clean
            , by = "ID_CAMP")

## organize the data adding to each sending event the corresponding opens/clicks/fails

# sends
df_sends <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "S") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_S = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE = EVENT_DATE) %>%
  as.data.frame()
df_sends
# opens
# there could be multiple opens of the same communication
# 1- count the open events
# 2- consider explicitely only the first open

df_opens_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "V") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_O = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , OPEN_DATETIME = EVENT_DATETIME
         , OPEN_DATE = EVENT_DATE)

total_opens <- df_opens_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarise(NUM_OPENs = n_distinct(ID_EVENT_O))
  
df_opens <- df_opens_prep %>%
  left_join(total_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(OPEN_DATETIME == min(OPEN_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()
df_opens
# clicks
# there could be multiple clicks of the same communication
# 1- count the click events
# 2- consider explicitely only the first click

df_clicks_prep <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "C") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_C = ID_EVENT
       , ID_CLI
       , ID_CAMP
       , TYP_CAMP
       , ID_DELIVERY
       , CLICK_DATETIME = EVENT_DATETIME
       , CLICK_DATE = EVENT_DATE)

total_clicks <- df_clicks_prep %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  summarise(NUM_CLICKs = n_distinct(ID_EVENT_C))

df_clicks <- df_clicks_prep %>%
  left_join(total_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY")) %>%
  group_by(ID_CLI
           , ID_CAMP
           , ID_DELIVERY) %>%
  filter(CLICK_DATETIME == min(CLICK_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()
df_clicks

# fails
df_fails <- df_6_camp_event_clean %>%
  filter(TYP_EVENT == "F") %>%
  select(-TYP_EVENT) %>%
  select(ID_EVENT_F = ID_EVENT
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , FAIL_DATETIME = EVENT_DATETIME
         , FAIL_DATE = EVENT_DATE) %>%
  group_by(ID_CLI, ID_CAMP, ID_DELIVERY) %>%
  filter(FAIL_DATETIME == min(FAIL_DATETIME)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()
df_fails
# combine sends opens clicks and fails
df_6_camp_event_clean_final <- df_sends %>%
  left_join(df_opens
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(OPEN_DATE) | SEND_DATE <= OPEN_DATE) %>%
  left_join(df_clicks
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(CLICK_DATE) | OPEN_DATE <= CLICK_DATE) %>%
  left_join(df_fails
            , by = c("ID_CLI", "ID_CAMP", "ID_DELIVERY", "TYP_CAMP")
  ) %>%
  filter(is.na(FAIL_DATE) | SEND_DATE <= FAIL_DATE) %>%
  mutate(OPENED = !is.na(ID_EVENT_O)) %>%
  mutate(CLICKED = !is.na(ID_EVENT_C)) %>%
  mutate(FAILED = !is.na(ID_EVENT_F)) %>%
  mutate(DAYS_TO_OPEN = as.integer(OPEN_DATE - SEND_DATE)) %>%
  select(ID_EVENT_S
         , ID_CLI
         , ID_CAMP
         , TYP_CAMP
         , ID_DELIVERY
         , SEND_DATE
         
         , OPENED
         , OPEN_DATE
         , DAYS_TO_OPEN
         , NUM_OPENs
         
         , CLICKED
         , CLICK_DATE
         , NUM_CLICKs
         
         , FAILED
         )

#### EXPLORE VARIABLES in df_6 ####

### GENERAL OVERVIEW ###

## compute aggregate
df6_overview <- df_6_camp_event_clean_final %>% 
  summarise(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overview

### GENERAL OVERVIEW by TYP_CAMP ###

## compute aggregate
df6_overviewbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP) %>%
  summarise(MIN_DATE = min(SEND_DATE)
            , MAX_DATE = max(SEND_DATE)
            , TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI))

df6_overviewbytyp

## Plot 

ggplot(data=df6_overviewbytyp
         , aes(x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", fill="steelblue") +
    labs(title = "Type campaign",
         x     = " Campaign",
         y     = "Total events") + 
    theme(plot.title = element_text(hjust = 0.5)) + 
    
    theme_minimal()

### Variable OPENED ###


df6_dist_opened <- df_6_camp_event_clean_final %>%
  group_by(OPENED) %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_opened

## Plot 

ggplot(data = df6_dist_opened
       , aes(x = OPENED, y = TOT_EVENTs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = "Opened campaign",
       x     = "Opened",
       y     = "Total events") +               
  theme_minimal()                         
  theme(plot.title = element_text(hjust = 0.5))  
  guides(fill = FALSE)
  
### Variable OPENED by TYP_CAMP ###


df6_dist_openedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, OPENED)  %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , OPENED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_openedbytyp

## Plot 

ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    theme_minimal()+
    labs(title = " Opened by type campaign",
         x     = "Type campaign",
         y     = "Total events")  +
    theme(plot.title = element_text(hjust = 0.5)) + 
    guides(fill = FALSE)

## plot percent

ggplot(data=df6_dist_openedbytyp
         , aes(fill=OPENED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    labs(title = " Opened by type campaign (%)",
         x     = "Type campaign",
         y     = "Total events")+
  theme(plot.title = element_text(hjust = 0.5)) + 
    theme_minimal()

### Variable DAYS_TO_OPEN

## compute aggregate
df6_dist_daystoopen <- df_6_camp_event_clean_final %>%
  filter(OPENED) %>%
  group_by(ID_CLI) %>%
  summarise(AVG_DAYS_TO_OPEN = floor(mean(DAYS_TO_OPEN))) %>%
  ungroup() %>%
  group_by(AVG_DAYS_TO_OPEN) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI))

df6_dist_daystoopen

## plot aggregate

ggplot(data=df6_dist_daystoopen %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=TOT_CLIs)) +
    geom_bar(stat="identity", fill="steelblue") +
    labs(title = " Average days to open email",
         x     = "Average days to open",
         y     = "Total clients")  +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_minimal()

### DAYS_TO_OPEN vs CUMULATE PERCENT ###

## compute aggregate
df6_dist_daystoopen_vs_cumulate <- df6_dist_daystoopen %>%
  arrange(AVG_DAYS_TO_OPEN) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs))

## plot aggregate

ggplot(data=df6_dist_daystoopen_vs_cumulate %>%
           filter(AVG_DAYS_TO_OPEN < 14)
         , aes(x=AVG_DAYS_TO_OPEN, y=PERCENT_COVERED)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks=seq(0,14,2), minor_breaks=0:14) +
    labs(title = "Average days to open emai (%) ",
         x     = "Average days to open",
         y     = "Coverage percent")  +
    theme_minimal()


#### ???? TO DO df_6 ???? ####
# EXPLORE the following relevant variables in df_6_camp_event_clean_final:
# - CLICKED/CLICKED by TYP_CAMP
# - FAILED/FAILED by TYP_CAP
# - NUM_OPENs
# - NUM_CLICKs

### Variable CLICKED ###


df6_dist_clicked <- df_6_camp_event_clean_final %>%
  group_by(CLICKED) %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_clicked

## Plot 

  ggplot(data=df6_dist_clicked 
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    labs(title = "Campaign clicked ",
         x     = "Type campaign",
         y     = "Total events")  +
    
    theme_minimal()
  
### Variable CLICKED/CLICKED by TYP_CAMP ###
  

df6_dist_clickedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, CLICKED)  %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , CLICKED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
 )

df6_dist_clickedbytyp

## Plot 

  ggplot(data=df6_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    labs(title = " Clicked  by type campaign",
         x     = "Type campaign",
         y     = "Total events")+
    theme_minimal()


## Plot percent

  ggplot(data=df6_dist_clickedbytyp
         , aes(fill=CLICKED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    labs(title = " Clicked  by type campaign (%)",
         x     = "Type campaign",
         y     = "Total events")+
    theme_minimal()


### Variable FAILED ###


df6_dist_failed <- df_6_camp_event_clean_final %>%
  group_by(FAILED) %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(TYP_CAMP = 'ALL') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/df6_overview$TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/df6_overview$TOT_CLIs)

df6_dist_failed

## Plot 

  ggplot(data=df6_dist_failed
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity", position="fill") +
    labs(title = " Failed campaign",
         x     = "Type campaign",
         y     = "Total events")+
    theme_minimal()

# - FAILED/FAILED by TYP_CAP

df6_dist_failedbytyp <- df_6_camp_event_clean_final %>%
  group_by(TYP_CAMP, FAILED)  %>%
  summarise(TOT_EVENTs = n_distinct(ID_EVENT_S)
            , TOT_CLIs = n_distinct(ID_CLI)) %>%
  left_join(df6_overviewbytyp %>%
              select(TYP_CAMP
                     , ALL_TOT_EVENTs = TOT_EVENTs
                     , ALL_TOT_CLIs = TOT_CLIs)
            , by='TYP_CAMP') %>%
  mutate(PERCENT_EVENTs = TOT_EVENTs/ALL_TOT_EVENTs
         , PERCENT_CLIs = TOT_CLIs/ALL_TOT_CLIs) %>%
  select(TYP_CAMP
         , FAILED
         , TOT_EVENTs
         , TOT_CLIs
         , PERCENT_EVENTs
         , PERCENT_CLIs
  )

df6_dist_failedbytyp

## Plot 

  ggplot(data=df6_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(stat="identity") +
    labs(title = " Failed by type campaign",
         x     = "Type campaign",
         y     = "Total events")+
    theme_minimal()



## Plot percent

  ggplot(data=df6_dist_failedbytyp
         , aes(fill=FAILED, x=TYP_CAMP, y=TOT_EVENTs)) +
    geom_bar(position="fill", stat="identity") +
    labs(title = " Failed by type campaign (%)",
         x     = "Type campaign",
         y     = "Total events")+
    theme_minimal()

# - NUM_OPENs
### Variable NUM_OPENs
df6_dist_numopens <- df_6_camp_event_clean_final%>%
  group_by(NUM_OPENs)                        %>% 
  summarise(TOT_ID = n_distinct(ID_EVENT_S)) %>% 
  mutate(PERCENT = TOT_ID/sum(TOT_ID))       %>% 
  arrange(desc(PERCENT))                         
df6_dist_numopens

## Plot 
ggplot(data=df6_dist_numopens
       , aes( x=NUM_OPENs, y=TOT_ID)) +
  geom_bar(stat="identity") +
  labs(title = " Number opens email",
       x     = "Number opens",
       y     = "Total ID")+
  theme_minimal()

# - NUM_CLICKs
# EXPLORE NUM_CLICKs variable
df6_dist_numclicks<- df_6_camp_event_clean_final%>%
  group_by(NUM_CLICKs)                        %>% 
  summarise(TOT_ID = n_distinct(ID_EVENT_S)) %>% 
  mutate(PERCENT = TOT_ID/sum(TOT_ID))       %>% 
  arrange(desc(PERCENT))                         
df6_dist_numclicks

# Plot
ggplot(data=df6_dist_numclicks
       , aes( x=NUM_CLICKs, y=TOT_ID)) +
  geom_bar(stat="identity") +
           
  labs(title = " Number clicks email",
       x     = "Number clicks",
       y     = "Total ID")+
  theme_minimal()


#### FINAL REVIEW df_6_clean ####

str(df_6_camp_event_clean_final)
summary(df_6_camp_event_clean_final)

#information about this file:
#the most successful campaign is the product (25%) and personalized (23%), based on the open ones.
#while the most clicked are by product and national
  
                                                                                                                                                                             
