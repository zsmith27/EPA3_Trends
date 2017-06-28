test <- final.df %>% 
  group_by(SITE, DATE, ICPRB_NAME, ICPRB_VALUE, ActivityIdentifier) %>% 
  filter(n() > 1)

test2 <- test %>% 
  arrange(SITE, DATE, ICPRB_NAME) %>% 
  filter(!SITE %in% c("103011", "103031", "103041", "104011", "104021",
                      "104051", "105031", "106031", "106141"))

test2 <- final.df %>%  filter(SITE == "USGS-01567000")

test3 <- final.df %>% filter(ResultAnalyticalMethod.MethodIdentifierContext == "APHA_SM20ED")
test4 <- test %>% filter(ResultAnalyticalMethod.MethodIdentifierContext == "APHA_SM20ED")


test5 <- final.df %>% 
  filter(AGENCY == "21DELAWQ",
         ICPRB_NAME == "PH", 
         SITE == "103011") %>% 
  select(SITE, DATE, ResultSampleFractionText, REPORTED_VALUE)
table(test5$ResultSampleFractionText, test5$SITE)


test3 <- as.data.frame(test2) %>% 
  filter(AGENCY == "21DELAWQ") %>% 
  select(SITE, ICPRB_NAME) %>% 
  distinct()

final.df2 <- final.df %>% 
  select(-TIME, -END_TIME, -ActivityStartTime.TimeZoneCode,
         -ActivityEndTime.TimeZoneCode) %>% 
  distinct()

test3 <- final.df2 %>% 
  filter(AGENCY == "21DELAWQ") %>% 
  group_by(SITE, DATE, ICPRB_NAME, ICPRB_VALUE, ActivityIdentifier) %>% 
  filter(n() > 1) %>% 
  mutate(ResultSampleFractionText = gsub("\\s+"," ",unique(ResultSampleFractionText)),
         OMIT = case_when(
           ICPRB_NAME %in% c("DOC", "TOC") & 
             ResultAnalyticalMethod.MethodIdentifierContext == "APHA_SM20ED" ~ TRUE,
           ICPRB_NAME %in% "PH" & ResultSampleFractionText == "PH SU" ~ TRUE,
           ICPRB_NAME %in% "DO" & ResultSampleFractionText == "DO MG/L" ~ TRUE,
           ICPRB_NAME %in% c("CL_TOT", "NO2W") & RESULT_TYPE != "K" ~ TRUE,
           !ICPRB_NAME %in% c("DOC", "TOC", "PH", "DO", "CL_TOT", "NO2W") ~ NA)) %>% 
  filter(is.na(OMIT))

test4 <- test3 %>% 
  group_by(SITE, DATE, ICPRB_NAME, ICPRB_VALUE, ActivityIdentifier) %>% 
  filter(n() > 1)

gsub("\\s+"," ", unique(test3$ResultSampleFractionText))
