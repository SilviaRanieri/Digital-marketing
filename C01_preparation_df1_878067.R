## Dataset 1

# Contains information on the fidelty subscriptions of each costumer account:

# Variables are: 

#* `ID_CLI`: identify client (*Foreign Key*);
#* `ID_FID`: identify fidelty program (**Key**);
#* `ID_NEG`: identify reference store;
#* `TYP_CLI_FID`: identify the main account (Binomyal);
#* `COD_FID`:  type  fidelty program;
#* `STATUS_FID`: identify if an account is active (Binomyal);
#* `DT_ACTIVE`: identify the date of activation.

## 1. Fidelity

str(df_1_cli_fid)
summary(df_1_cli_fid)

#### START CLEANING df_1 ####

df_1_cli_fid_clean <- df_1_cli_fid

#### CLEANING DUPLICATE VALUES in df_1 ####

## check for duplicates
df_1_cli_fid_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ID_FIDs = n_distinct(ID_FID)
            , TOT_ID_CLIFIDs = n_distinct(paste0(as.character(ID_CLI),"-",as.character(ID_FID)))
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates for combination CLI-FID !!!#

#### CLEANING DATA TYPES in df_1 ####

## formatting dates ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(DT_ACTIVE = as.Date(DT_ACTIVE))

## formatting boolean as factor ##
df_1_cli_fid_clean <- df_1_cli_fid_clean %>%
  mutate(TYP_CLI_FID = as.factor(TYP_CLI_FID)) %>%
  mutate(STATUS_FID = as.factor(STATUS_FID))

#### CONSISTENCY CHECK on df1: number of fidelity subscriptions per client ####

## count the subscriptions for each client
num_fid_x_cli <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  summarise(NUM_FIDs =  n_distinct(ID_FID)
            , NUM_DATEs = n_distinct(DT_ACTIVE)
            )
num_fid_x_cli
tot_id_cli <- n_distinct(num_fid_x_cli$ID_CLI)

## compute the distribution of number of subscriptions
dist_num_fid_x_cli <- num_fid_x_cli %>%
  group_by(NUM_FIDs, NUM_DATEs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT_CLIs = TOT_CLIs/tot_id_cli)

dist_num_fid_x_cli

#!!! NOTE: there are clients with multiple fidelity subscriptions !!!#

## let examine in details clients with multiple subscriptions

num_fid_x_cli %>% filter(NUM_FIDs == 3)

# each subscription can have different dates
df_1_cli_fid %>% filter(ID_CLI == 621814)
# there could be subscriptions at the same dates [possibly for technical reasons]
df_1_cli_fid %>% filter(ID_CLI == 320880)

#### RESHAPING df_1 ####

## combining information

# from first subscription  --> registration date, store for registration
# from last subscription   --> type of fidelity, status
# from subscriptions count --> number of subscriptions made

df_1_cli_fid_first <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == min(DT_ACTIVE)) %>%
  arrange(ID_FID) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()
df_1_cli_fid_first 

df_1_cli_fid_last <- df_1_cli_fid_clean %>%
  group_by(ID_CLI) %>%
  filter(DT_ACTIVE == max(DT_ACTIVE)) %>%
  arrange(desc(ID_FID)) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  as.data.frame()
df_1_cli_fid_last

df_1_cli_fid_clean <- df_1_cli_fid_last %>%
  select(ID_CLI
         , ID_FID
         , LAST_COD_FID = COD_FID
         , LAST_TYP_CLI_FID = TYP_CLI_FID
         , LAST_STATUS_FID = STATUS_FID
         , LAST_DT_ACTIVE = DT_ACTIVE) %>%
  left_join(df_1_cli_fid_first %>%
              select(ID_CLI
                     , FIRST_ID_NEG = ID_NEG
                     , FIRST_DT_ACTIVE = DT_ACTIVE)
            , by = 'ID_CLI') %>%
  left_join(num_fid_x_cli %>%
              select(ID_CLI
                     , NUM_FIDs) %>%
              mutate(NUM_FIDs = as.factor(NUM_FIDs))
            , by = 'ID_CLI')

df_1_cli_fid_clean 
#### EXPLORE COLUMNS of df_1 ####

### variable LAST_COD_FID ###


df1_dist_codfid <- df_1_cli_fid_clean %>%
  group_by(LAST_COD_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_codfid

## Plot 
ggplot(data = df1_dist_codfid
       , aes(x = LAST_COD_FID, y = TOT_CLIs)
) +                        
  geom_bar(stat = "identity"                
          , fill="steelblue") +
  labs(title = "Type of fidelity",
     x     = "Type fidelity",
     y     = "Total clients") +               
  theme_minimal() +                              
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_discrete(labels = c("Premium", "Premium biz", "Standard","Standard biz")) + 
  guides(fill = FALSE)

# EXPLORE the remaining df_1_cli_fid_clean relevant variables

### variable LAST_TYP_CLI_FID ###
df1_dist_typfid <- df_1_cli_fid_clean %>%
  group_by(LAST_TYP_CLI_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_typfid

## Plot 
ggplot(data = df1_dist_type
       , aes(x = LAST_TYP_CLI_FID, y = TOT_CLIs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +
  labs(title = "Type clients of fidelity",
       x     = "Type clients fidelity",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  
  guides(fill = FALSE)

### variable LAST_STATUS_FID ###
df1_dist_status <- df_1_cli_fid_clean %>%
  group_by(LAST_STATUS_FID) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))
df1_dist_status
## Plot 
ggplot(data = df1_dist_status
       , aes(x = LAST_STATUS_FID, y = TOT_CLIs)
) +                        #-- Dataset to Plot
  geom_bar(stat = "identity"                #-- Bar Plot
           , fill="steelblue") +  
  labs(title = "Status fidelity",
       x     = "Account active",
       y     = "Total clients") +                #-- Labs
  theme_minimal() +                               #-- ggplot Theme
  theme(plot.title = element_text(hjust = 0.5)) + #-- Centering Title
  guides(fill = FALSE)

### variable LAST_DT_ACTIVE ###
df1_dist_active_last <- df_1_cli_fid_clean %>%
  group_by(LAST_DT_ACTIVE) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_active_last 

## Plot 
 
ggplot(data = df1_dist_active_last 
       , aes(x = LAST_DT_ACTIVE, y = TOT_CLIs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = "Last date of activation",
       x     = "Date of activation",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)


# compute and plot distribution of variable FIRST_DT_ACTIVE 
df1_dist_active_first <- df_1_cli_fid_clean        %>%
  group_by(FIRST_DT_ACTIVE)                %>%   
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%   
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%   
  arrange(desc(PERCENT))  
df1_dist_active_first 

## Plot 
ggplot(data = df1_dist_active_first 
       , aes(x = FIRST_DT_ACTIVE, y = TOT_CLIs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = " First date of activation",
       x     = " Date of activation",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)
### variable FIRST_ID_NEG ###


df1_dist_first_id_neg <- df_1_cli_fid_clean %>%
  group_by(FIRST_ID_NEG) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_first_id_neg 

## Plot 
ggplot(data = df1_dist_first_id_neg
       , aes(x = FIRST_ID_NEG, y = TOT_CLIs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = " Store",
       x     = "Reference store",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)

### variable NUM_FIDs ###


df1_dist_num <- df_1_cli_fid_clean %>%
  group_by(NUM_FIDs) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))

df1_dist_num

## Plot 

ggplot(data = df1_dist_num
       , aes(x = NUM_FIDs, y = TOT_CLIs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = " Clients fidelity",
       x     = "Number clients ",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)

#### FINAL REVIEW df_1_clean ####

str(df_1_cli_fid_clean)
summary(df_1_cli_fid_clean)

#information about this file
#Total client of the store are 369472 and total client fidelity are 367925 from  from January 2018 to May 2019. 
#loyal customers belong more to the standard group.

