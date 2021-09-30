## Dataset 2

# df2: Information on each customer account
  
# Variables are:
  
#- `ID_CLI`: identify the client (**Key**);
#- `EMAIL_PROVIDER`: identify the email account provider;
#- `W_PHONE`: identify if a phone number is added (Binomyal);
#- `ID_ADDRESS`: identify the address (*Foreign Key*);
#- `TYP_CLI_ACCOUNT`: identify the account type of the client;
#- `TYP_JOB`: identify the client job.

#### FIRST LOOK of df_2 ####

str(df_2_cli_account)
summary(df_2_cli_account)

#### START CLEANING df_2 ####

df_2_cli_account_clean <- df_2_cli_account

#### CLEANING DUPLICATE VALUES in df_2 ####

## check for duplicates
df_2_cli_account_clean %>%
  summarise(TOT_ID_CLIs = n_distinct(ID_CLI)
            , TOT_ROWs = n())

#!!! NOTE:  no duplicates !!!#

#### CLEANING DATA TYPES in df_2 ####

## format boolean as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = as.factor(W_PHONE))

## format numerical categories as factor ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(TYP_CLI_ACCOUNT = as.factor(TYP_CLI_ACCOUNT))

#### CLEANING MISSING VALUES in df_2 ####

## MISSING VALUES mapped as natural values ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(W_PHONE = fct_explicit_na(W_PHONE, "0"))

## MISSING VALUES mapped as new level in categorical columns ##
df_2_cli_account_clean <- df_2_cli_account_clean %>%  
  mutate(EMAIL_PROVIDER = fct_explicit_na(EMAIL_PROVIDER, "(missing)")) %>%
  mutate(TYP_JOB = fct_explicit_na(TYP_JOB, "(missing)"))

#### CONSISTENCY CHECK ID_CLI in df_1/df_2 ####

cons_idcli_df1_df2 <- df_1_cli_fid_clean %>%
  select(ID_CLI) %>%
  mutate(is_in_df_1 = 1) %>%
  distinct() %>%
  full_join(df_2_cli_account_clean %>%
              select(ID_CLI) %>%
              mutate(is_in_df_2 = 1) %>%
              distinct()
            , by = "ID_CLI"
  ) %>%
  group_by(is_in_df_1, is_in_df_2) %>%
  summarise(NUM_ID_CLIs = n_distinct(ID_CLI)) %>%
  as.data.frame()

cons_idcli_df1_df2

#!!! NOTE: all ID_CLI in df_1 are also in df_2 and vice-versa !!!#

#### EXPLORE COLUMNS of df_2 ####

### Variable EMAIL_PROVIDER ###

df_2_dist_emailprovider <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_emailprovider

tot_emailproviders <- n_distinct(df_2_dist_emailprovider$EMAIL_PROVIDER)

tot_emailproviders
#### RESHAPING df_2
## keep the most frequent EMAIL_PROVIDER values and add a common factor level "OTHER" for the remaining 
df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  as.data.frame() %>%
  head(20)
## always keep the (missing) level for technical reasons
## select levels that cover the 85% of the cases, the remaining 15% 
clean_email_providers <- df_2_dist_emailprovider %>%
  arrange(desc(PERCENT)) %>%
  mutate(PERCENT_COVERED = cumsum(TOT_CLIs)/sum(TOT_CLIs)) %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  mutate(AUX = if_else(PERCENT_COVERED < 0.85 | (PERCENT_COVERED > 0.85 & lag(PERCENT_COVERED) < 0.85), 1,0)) %>%
  mutate(EMAIL_PROVIDER_CLEAN = if_else(AUX | EMAIL_PROVIDER == "(missing)", EMAIL_PROVIDER, "others"))
head(clean_email_providers, 20)
## add clean EMAIL_PROVIDER
df_2_cli_account_clean <- df_2_cli_account_clean %>%
  mutate(EMAIL_PROVIDER = as.character(EMAIL_PROVIDER)) %>%
  left_join(clean_email_providers %>%
              select(EMAIL_PROVIDER, EMAIL_PROVIDER_CLEAN)
            , by = "EMAIL_PROVIDER") %>%
  select(-EMAIL_PROVIDER) %>%
  mutate(EMAIL_PROVIDER_CLEAN = as.factor(EMAIL_PROVIDER_CLEAN))

#### EXPLORE COLUMNS of df_2
## compute and plot distribution of NEW COLUMNS EMAIL_PROVIDER_CLEAN
df2_dist_emailproviderclean <- df_2_cli_account_clean %>%
  group_by(EMAIL_PROVIDER_CLEAN) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT))
df2_dist_emailproviderclean


## Plot 
ggplot(data = df2_dist_emailproviderclean
       , aes(x = EMAIL_PROVIDER_CLEAN, y = TOT_CLIs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = " Clients account",
       x     = "Email account",
       y     = "Total clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)

#*W_PHONE*
df_2_dist_W_PHONE <- df_2_cli_account_clean %>%
  group_by(W_PHONE) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_W_PHONE

tot_df_2_dist_W_PHONE <- n_distinct(df_2_dist_W_PHONE$W_PHONE)

tot_df_2_dist_W_PHONE 

#*TYP_JOB*
df_2_dist_TYP_JOB<- df_2_cli_account_clean %>%
  group_by(TYP_JOB) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_TYP_JOB

tot_df_2_dist_TYP_JOB <- n_distinct(df_2_dist_TYP_JOB$TYP_JOB)

tot_df_2_dist_TYP_JOB 

#TYP_CLI_ACCOUNT
df_2_dist_TYP_CLI_ACCOUNT<- df_2_cli_account_clean %>%
  group_by(TYP_CLI_ACCOUNT) %>%
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>%
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>%
  arrange(desc(PERCENT)) %>%
  as.data.frame()

df_2_dist_TYP_CLI_ACCOUNT

tot_df_2_dist_TYP_CLI_ACCOUNT <- n_distinct(df_2_dist_TYP_CLI_ACCOUNT$TYP_CLI_ACCOUNT)

tot_df_2_dist_TYP_CLI_ACCOUNT

#EXPLORE ID_ADDRESS distribution
df_2_dist_address <- df_2_cli_account_clean         %>%
  group_by(ID_ADDRESS)                %>% 
  summarise(TOT_CLIs = n_distinct(ID_CLI)) %>% 
  mutate(PERCENT = TOT_CLIs/sum(TOT_CLIs)) %>% 
  arrange(desc(PERCENT))                   %>% 
  as.data.frame()
df_2_dist_address 

#!!! NOTE: too many different values for EMAIL_PROVIDER to be an useful category !!!#

#### FINAL REVIEW df_2_clean ####

str(df_2_cli_account_clean)
summary(df_2_cli_account_clean)

#Information about this file:
# Customers have more gmail accounts (41%). As a type of job there are many missing value 
