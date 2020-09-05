library(tidyverse)

# Data - multiple cause mortality files
## https://www.cdc.gov/nchs/data_access/vitalstatsonline.htm

data2018 <- read_table("mort2018us.zip", col_names = FALSE)

data2018 <-
  data2018 %>%
  select(20:31) %>%
  rename("CoD1" = "X20", 
         "CoD2" = "X21", 
         "CoD3" = "X22", 
         "CoD4" = "X23",
         "CoD5" = "X24",
         "CoD6" = "X25",
         "CoD7" = "X26", 
         "CoD8" = "X27",
         "CoD9" = "X28", 
         "CoD10" = "X29", 
         "CoD11" = "X30", 
         "CoD12" = "X31")
  

# define function to identify LIVER outcomes

CoDrecodeLiver <- function(variable) {
  case_when(
    str_starts(variable, "K70") ~ "99ArLD",
    str_starts(variable, "K71") ~ "99Toxic liver disease",
    str_starts(variable, "K72") ~ "99Liver failure",
    str_starts(variable, "K73") ~ "99Chronic hepatitis",
    str_starts(variable, "K740") ~ "99Hepatic fibrosis",
    str_starts(variable, "K741") ~ "99Hepatic sclerosis",
    str_starts(variable, "K742") ~ "99Hepatic fibrosis & sclerosis",
    str_starts(variable, "K743") ~ "99Primary biliary cirrhosis",
    str_starts(variable, "K744") ~ "99Secondry biliary cirrhosis",
    str_starts(variable, "K745") ~ "99Biliary cirrhosis other",
    str_starts(variable, "K746") ~ "99Other cirrhosis",
    str_starts(variable, "K750") ~ "99Liver abcess",
    str_starts(variable, "K751") ~ "99Other inflammatory",
    str_starts(variable, "K752") ~ "99Other inflammatory",
    str_starts(variable, "K753") ~ "99Other inflammatory",
    str_starts(variable, "K754") ~ "99Autoimmune hepatitis",
    str_starts(variable, "K758") ~ "99NASH",
    str_starts(variable, "K759") ~ "99Other inflammatory",
    str_starts(variable, "K760") ~ "99Fatty liver",
    str_starts(variable, "K761") ~ "99Other liver",
    str_starts(variable, "K762") ~ "99Other liver",
    str_starts(variable, "K763") ~ "99Other liver",
    str_starts(variable, "K764") ~ "99Other liver",
    str_starts(variable, "K765") ~ "99Other liver",
    str_starts(variable, "K766") ~ "99Portal hypertension",
    str_starts(variable, "K767") ~ "99Hepatorenal syndrome",
    str_starts(variable, "K768") ~ "99Other liver",
    str_starts(variable, "K769") ~ "99Other liver",
    str_starts(variable, "K770") ~ "99Other liver",
    str_starts(variable, "K778") ~ "99Other liver",
    str_starts(variable, "C22") ~ "98HCC",
    str_starts(variable, "B18") ~ "99Viral Hepatitis",
    str_starts(variable, "F10") ~ "97Alcohol xs",
    TRUE ~ variable
  )
}

# Replace Liver codes for outcomes

data2018LiverCoD <-
  data2018 %>%
  mutate(CoD1 = CoDrecodeLiver(CoD1),
         CoD2 = CoDrecodeLiver(CoD2),
         CoD3 = CoDrecodeLiver(CoD3),
         CoD4 = CoDrecodeLiver(CoD4),
         CoD5 = CoDrecodeLiver(CoD5),
         CoD6 = CoDrecodeLiver(CoD6),
         CoD7 = CoDrecodeLiver(CoD7),
         CoD8 = CoDrecodeLiver(CoD8),
         CoD9 = CoDrecodeLiver(CoD9),
         CoD10 = CoDrecodeLiver(CoD10),
         CoD11 = CoDrecodeLiver(CoD11),
         CoD12 = CoDrecodeLiver(CoD12))

# Add grouped codes for outcome

CoDrecodeGroup <- function(variable) {
  case_when(
    str_starts(variable, "99") ~ "Liver",
    str_starts(variable, "98") ~ "HCC",
    str_starts(variable, "A") ~ "Infections",
    str_starts(variable, "B") ~ "Infections",
    str_starts(variable, "C") ~ "Cancer",
    str_starts(variable, "D") ~ "Haematological",
    str_starts(variable, "E") ~ "Endocrine & metabolic",
    str_starts(variable, "F") ~ "Mental",
    str_starts(variable, "G") ~ "Neurological",
    str_starts(variable, "H") ~ "Eyes & Ears",
    str_starts(variable, "I") ~ "Circulatory",
    str_starts(variable, "J") ~ "Respiratory",
    str_starts(variable, "K0") ~ "GI",
    str_starts(variable, "K1") ~ "GI",
    str_starts(variable, "K2") ~ "GI",
    str_starts(variable, "K3") ~ "GI",
    str_starts(variable, "K4") ~ "GI",
    str_starts(variable, "K5") ~ "GI",
    str_starts(variable, "K6") ~ "GI",
    str_starts(variable, "K8") ~ "Biliary",
    str_starts(variable, "K9") ~ "GI",
    str_starts(variable, "L") ~ "Skin",
    str_starts(variable, "M") ~ "MSK",
    str_starts(variable, "N") ~ "GUM",
    str_starts(variable, "O") ~ "Maternal",
    str_starts(variable, "P") ~ "Perinatal",
    str_starts(variable, "Q") ~ "Congenital",
    str_starts(variable, "R") ~ "Other",
    str_starts(variable, "S") ~ "External",
    str_starts(variable, "T") ~ "External",
    str_starts(variable, "U") ~ "Other",
    str_starts(variable, "V") ~ "External",
    str_starts(variable, "W") ~ "External",
    str_starts(variable, "X") ~ "External",
    str_starts(variable, "Y") ~ "External",
  )
}


data2018CoDAll <- 
  data2018LiverCoD %>%
  mutate(GroupedMainCoD = CoDrecodeGroup(CoD1),
         Group1 = CoDrecodeGroup(CoD1),
         Group2 = CoDrecodeGroup(CoD2),
         Group3 = CoDrecodeGroup(CoD3),
         Group4 = CoDrecodeGroup(CoD4),
         Group5 = CoDrecodeGroup(CoD5),
         Group6 = CoDrecodeGroup(CoD6),
         Group7 = CoDrecodeGroup(CoD7),
         Group8 = CoDrecodeGroup(CoD8),
         Group9 = CoDrecodeGroup(CoD9),
         Group10 = CoDrecodeGroup(CoD10),
         Group11 = CoDrecodeGroup(CoD11),
         Group12 = CoDrecodeGroup(CoD12)) 


# Main causes of death

MainCauses <-
  data2018CoDAll %>%
  count(Group1) %>%
  replace_na(list(Group1 = "Unknown")) %>%
  mutate(Total = sum(n),
         Proportion = n / Total * 100) %>% 
  arrange(-Proportion)

MainCauses


# ArLD vs NAFLD (broadly defined, as per ZY inc. "Other cirrhosis")

data2018LiverDisease <-
  data2018CoDAll %>%
  mutate(ArLD = if_else(CoD1 == "99ArLD" | 
                          CoD2 == "99ArLD" |
                          CoD3 == "99ArlD" |
                          CoD4 == "99ArLD" |
                          CoD5 == "99ArLD" |
                          CoD6 == "99ArLD" |
                          CoD7 == "99ArLD" |
                          CoD8 == "99ArLD" |
                          CoD9 == "99ArLD" |
                          CoD10 == "99ArLD" |
                          CoD11 == "99ArLD" |
                          CoD12 == "99ArLD", 
                        "Yes", "No")) %>%
  mutate(NAFLDorCrypto = if_else(CoD1 == "99Fatty liver" | CoD1 == "99NASH" | CoD1 == "99Other cirrhosis" |
                           CoD2 == "99Fatty liver" | CoD2 == "99NASH" | CoD2 == "99Other cirrhosis" |
                           CoD3 == "99Fatty liver" | CoD3 == "99NASH" | CoD3 == "99Other cirrhosis" |
                           CoD4 == "99Fatty liver" | CoD4 == "99NASH" | CoD4 == "99Other cirrhosis" |
                           CoD5 == "99Fatty liver" | CoD5 == "99NASH" | CoD5 == "99Other cirrhosis" |
                           CoD6 == "99Fatty liver" | CoD6 == "99NASH" | CoD6 == "99Other cirrhosis" |
                           CoD7 == "99Fatty liver" | CoD7 == "99NASH" | CoD7 == "99Other cirrhosis" |
                           CoD8 == "99Fatty liver" | CoD8 == "99NASH" | CoD8 == "99Other cirrhosis" |
                           CoD9 == "99Fatty liver" | CoD9 == "99NASH" | CoD9 == "99Other cirrhosis" |
                           CoD10 == "99Fatty liver" | CoD10 == "99NASH" | CoD10 == "99Other cirrhosis" |
                           CoD11 == "99Fatty liver" | CoD11 == "99NASH" | CoD11 == "99Other cirrhosis" |
                           CoD12 == "99Fatty liver" | CoD12 == "99NASH" | CoD12 == "99Other cirrhosis", 
                         "Yes", "No")) %>%
  mutate(Viral = if_else(CoD1 == "99Viral Hepatitis" | 
                          CoD2 == "99Viral Hepatitis" |
                          CoD3 == "99Viral Hepatitis" |
                          CoD4 == "99Viral Hepatitis" |
                          CoD5 == "99Viral Hepatitis" |
                          CoD6 == "99Viral Hepatitis" |
                          CoD7 == "99Viral Hepatitis" |
                          CoD8 == "99Viral Hepatitis" |
                          CoD9 == "99Viral Hepatitis" |
                          CoD10 == "99Viral Hepatitis" |
                          CoD11 == "99Viral Hepatitis" |
                          CoD12 == "99Viral Hepatitis", 
                        "Yes", "No")) %>%
  mutate(AlcoholXs = if_else(CoD1 == "97Alcohol xs" | 
                               CoD2 == "97Alcohol xs" |
                               CoD3 == "97Alcohol xs" |
                               CoD4 == "97Alcohol xs" |
                               CoD5 == "97Alcohol xs" |
                               CoD6 == "97Alcohol xs" |
                               CoD7 == "97Alcohol xs" |
                               CoD8 == "97Alcohol xs" |
                               CoD9 == "97Alcohol xs" |
                               CoD10 == "97Alcohol xs" |
                               CoD11 == "97Alcohol xs" |
                               CoD12 == "97Alcohol xs", 
                             "Yes", "No"))


data2018CoDArLD <-
  data2018LiverDisease %>%
  filter(ArLD == "Yes" & Viral == "No") %>%
  count(Group1) %>%
  replace_na(list(Group1 = "Unknown")) %>%
  mutate(Total = sum(n),
         Proportion = n / Total * 100) %>% 
  arrange(-Proportion)


data2018CoDNAFLD <-
  data2018LiverDisease %>%
  filter(NAFLDorCrypto == "Yes" & ArLD == "No" & Viral == "No" & AlcoholXs == "No") %>%
  count(Group1) %>%
  replace_na(list(Group1 = "Unknown")) %>%
  mutate(Total = sum(n),
         Proportion = n / Total * 100) %>% 
  arrange(-Proportion)


## Outputs, cause specific
data2018CoDArLD

data2018CoDNAFLD


data2018CoDAll %>% 
  replace_na(list(Group1 = "Unknown")) %>%
  filter(Group1 == "Unknown")


