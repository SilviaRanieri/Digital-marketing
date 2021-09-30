## Dataset 3

# df3: Information on the address corresponding to a customer account
  
# Variables are:
  
#- `ID_ADDRESS`: identify the address (**Key**);
#- `CAP`: identify the postal code;
#- `PRV`: identify the province;
#- `REGION`: identify the region.

#### FIRST LOOK of df_3 ####

str(df_3_cli_address)
summary(df_3_cli_address)

#### START CLEANING df_3 ####

df_3_cli_address_clean <- df_3_cli_address

#### CLEANING DUPLICATE VALUES in df_3 ####

## check for duplicates
df_3_cli_address_clean %>%
  summarise(TOT_ID_ADDRESSes = n_distinct(ID_ADDRESS)
            , TOT_ROWs = n())

#!!! NOTE:  there are duplicates !!!#

df_3_cli_address_clean <- df_3_cli_address_clean %>%
  distinct()

#### CLEANING DATA TYPES in df_3 ####

## format string as factors ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%
  mutate(CAP = as.character(CAP))

#### CLEANING MISSING VALUES in df_3 ####

df_3_cli_address_clean %>%
  group_by(w_CAP = !is.na(CAP)
           , w_PRV = !is.na(PRV)
           , w_REGION = !is.na(REGION)) %>%
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS))
df_3_cli_address_clean
## let examine in details some of these missing cases
df_3_cli_address_clean %>% filter(!is.na(PRV) & is.na(REGION))

## MISSING VALUES rows are removed ##
df_3_cli_address_clean <- df_3_cli_address_clean %>%  
  filter(!is.na(CAP) & !is.na(PRV) & !is.na(REGION))

#### CONSISTENCY CHECK ID_ADDRESS in df_2/df_3 ####

cons_idaddress_df2_df3 <- df_2_cli_account_clean %>%
  select(ID_ADDRESS) %>%
  mutate(is_in_df_2 = 1) %>%
  distinct() %>%
  full_join(df_3_cli_address_clean %>%
              select(ID_ADDRESS) %>%
              mutate(is_in_df_3 = 1) %>%
              distinct()
            , by = "ID_ADDRESS"
  ) %>%
  group_by(is_in_df_2, is_in_df_3) %>%
  summarise(NUM_ID_ADDRESSes = n_distinct(ID_ADDRESS)) %>%
  as.data.frame()

cons_idaddress_df2_df3

#!!! NOTE:  there are ID_ADDRESSes actually not mapped in df_3 !!!#
#!!!        this issue should be taken into account in joining these two tables !!!#

#### EXPLORE COLUMNS of df_3 ####
nrow(df_3_cli_address_clean %>%
       distinct(CAP))
nrow(df_3_cli_address_clean %>%
       distinct(PRV))
print(df_3_cli_address_clean %>%
        distinct(REGION))


df_3_cli_address_clean_prv <- df_3_cli_address_clean %>%  
  group_by(PRV) %>%
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS)) %>%
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs)) %>%
  arrange(desc(PERCENT))
df_3_cli_address_clean_prv

# Plot
ggplot(data = df_3_cli_address_clean_prv
       , aes(x = PRV, y = TOT_ADDs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = " Clients by province ",
       x     = "Province",
       y     = "Total address") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)
# *REGION*
df3_dist_region<- df_3_cli_address_clean                   %>%
  group_by(REGION)                                %>%  
  summarise(TOT_ADDs = n_distinct(ID_ADDRESS)) %>% 
  mutate(PERCENT = TOT_ADDs/sum(TOT_ADDs))  %>%  
  arrange(desc(PERCENT))
df3_dist_region

# Plot
ggplot(data = df3_dist_region
       , aes(x = REGION, y = TOT_ADDs)
) +                       
  geom_bar(stat = "identity"               
           , fill="steelblue") +  
  labs(title = " Clients by region",
       x     = "Region",
       y     = "Clients") +               
  theme_minimal() +                               
  theme(plot.title = element_text(hjust = 0.5)) + 
  guides(fill = FALSE)



#### ???? TO DO df_3 ???? ####
# EXPLORE the df_3_cli_address_clean relevant variables

#### FINAL REVIEW df_3_clean ####

str(df_3_cli_address_clean)
summary(df_3_cli_address_clean)

#information about this file:
#Customers mostly belong to regions lombardia (28%) and followed by Lazio and Campania (10%).
#For province Milan (11%) and Rome (8%)
