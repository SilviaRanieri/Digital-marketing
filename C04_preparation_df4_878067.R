## Dataset 4

# df4: Information on the privacy policies accepted by each customer
  
# Variables are:
  
#- `ID_CLI`: identify the client (*Foreign Key*);
#- `FLAG_PRIVACY_1`: identify the flag privacy (binomyal);
#- `FLAG_PRIVACY_2`: identify the flag profiling (*Foreign Key*);
#- `FLAG_DIRECT_MKT`: identify the flag direct marketing (binomyal).

#### FIRST LOOK of df_4 ####

str(df_4_cli_privacy)
summary(df_4_cli_privacy)

#### START CLEANING df_4 ####

df_4_cli_privacy_clean <- df_4_cli_privacy

#### CLEANING DUPLICATE VALUES in df_4 ####

## check for duplicates
df_4_cli_privacy_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_4 ####

## formatting boolean as factor ##
df_4_cli_privacy_clean <- df_4_cli_privacy_clean %>%
  mutate(FLAG_PRIVACY_1 = as.factor(FLAG_PRIVACY_1)) %>%
  mutate(FLAG_PRIVACY_2 = as.factor(FLAG_PRIVACY_2)) %>%
  mutate(FLAG_DIRECT_MKT = as.factor(FLAG_DIRECT_MKT))

#### CONSISTENCY CHECK ID_CLI in df_1/df_4 ####

cons_idcli_df1_df4 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_4_cli_privacy_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_4 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_4) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df4

#!!! NOTE: all ID_CLI in df_1 are also in df_4 and vice-versa !!!#

#### EXPLORE COLUMNS of df_4 ####

#a. *FLAG_PRIVACY_1*

df_4_cli_privacy_flag1  <- df_4_cli_privacy_clean %>% 
  group_by(FLAG_PRIVACY_1) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))
df_4_cli_privacy_flag1 


#Plot
ggplot(data = df_4_cli_privacy_flag1  
       , aes(x = FLAG_PRIVACY_1, y = TOT_CLIs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = " Clients privacy",
       x     = "Flag privacy",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)

#b. *FLAG_PRIVACY_2*

df_4_cli_privacy_flag2  <- df_4_cli_privacy_clean %>% 
  group_by(FLAG_PRIVACY_2) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))
df_4_cli_privacy_flag2 

# Plot
ggplot(data = df_4_cli_privacy_flag2   
       , aes(x = FLAG_PRIVACY_2, y = TOT_CLIs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = " Profiling clients",
       x     = " Profiling",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)

#c. *FLAG_DIRECT_MKT*
  
df_4_cli_privacy_mkt <- df_4_cli_privacy_clean %>% 
  group_by(FLAG_DIRECT_MKT) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))
df_4_cli_privacy_mkt


# Plot
ggplot(data = df_4_cli_privacy_mkt  
       , aes(x = FLAG_DIRECT_MKT, y = TOT_CLIs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = " Direct marketing ",
       x     = " Marketing",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)

#### FINAL REVIEW df_4_clean ####

str(df_4_cli_privacy_clean)
summary(df_4_cli_privacy_clean)

#Inofmation about this file:
#Privacy flag are distribution:
#privacy1: 1 (65%)
#privacy2: 1 (93%)
#marketing: 1 (67%)





