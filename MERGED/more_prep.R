

# Load dplyr to manipulate data.
library(dplyr)
#==============================================================================
# Connect to PostgeSQL database.
con = DBI::dbConnect("PostgreSQL", user = "Z_Smith", password = "Hexapoda27",
                dbname = "WQT_May", host = "localhost", port = 5432)
on.exit(dbDisconnect(con), add = TRUE)
#------------------------------------------------------------------------------
# Import all parameter data.
param.df <- DBI::dbReadTable(con, "param_data", stringsAsfactors = FALSE)
#------------------------------------------------------------------------------
sub.param <- param.df %>% 
  select(AGENCY, SITE, ICP_ID, ICPRB_NAME, DATE, CENSORED) %>% 
  distinct()
#------------------------------------------------------------------------------
# Remove Sites/Parameters with > 50% of the data represented by Censored values.
#------------------------------------------------------------------------------
# Identify data with > 50% Censored values.
censored.df <- sub.param %>% 
  group_by(AGENCY, SITE, ICP_ID, ICPRB_NAME, CENSORED) %>% 
  summarise(COUNT = length(CENSORED))%>% 
  group_by(AGENCY, SITE, ICP_ID, ICPRB_NAME) %>% 
  mutate(TOTAL = sum(COUNT)) %>% 
  filter(CENSORED == "Censored") %>% 
  mutate(PERCENT = COUNT / TOTAL * 100) %>% 
  filter(PERCENT > 50)
# Remove data with > 50% Censored values.
sub.param <- anti_join(sub.param, censored.df,
                      by = c("AGENCY", "SITE", "ICP_ID", "ICPRB_NAME"))
#------------------------------------------------------------------------------
cov.df <- sub.param %>% 
  select(AGENCY, SITE, ICP_ID, ICPRB_NAME) %>% 
  distinct()
#------------------------------------------------------------------------------
early <- (as.Date("12/31/1986", format = "%m/%d/%Y") -
         as.Date("01/01/1972", format = "%m/%d/%Y")) %>% as.numeric()
mid <- (as.Date("12/31/2001", format = "%m/%d/%Y") -
            as.Date("01/01/1987", format = "%m/%d/%Y")) %>% as.numeric()
late <- (as.Date("12/31/2015", format = "%m/%d/%Y") -
          as.Date("01/01/2002", format = "%m/%d/%Y")) %>% as.numeric()
#------------------------------------------------------------------------------
early.df <- sub.param %>% 
  filter(DATE <= "1986-12-31") %>% 
  group_by(AGENCY, SITE, ICP_ID, ICPRB_NAME) %>% 
  summarise(COUNT = length(DATE)) %>% 
  filter(COUNT > early * 0.2)
#------------------------------------------------------------------------------
mid.df <- sub.param %>% 
  filter(DATE > "1986-12-31" & DATE < "2002-01-01") %>% 
  group_by(AGENCY, SITE, ICP_ID, ICPRB_NAME) %>% 
  summarise(COUNT = length(DATE)) %>% 
  filter(COUNT > mid * 0.2)
#------------------------------------------------------------------------------
late.df <- sub.param %>% 
  filter(DATE >= "2002-01-01") %>% 
  group_by(AGENCY, SITE, ICP_ID, ICPRB_NAME) %>% 
  summarise(COUNT = length(DATE)) %>% 
  filter(COUNT > late * 0.2)
#------------------------------------------------------------------------------

test <- sub.param %>% 
  group_by(AGENCY, SITE, ICP_ID, ICPRB_NAME) %>% 
  mutate(DIFF = c(0, diff(DATE)))

test <- final.df  %>% 
  filter(stringr::str_detect(ResultAnalyticalMethod.MethodIdentifierContext,
                             c("APHA_SM19ED", "APHA_SM20ED"))) %>% 
  select(SITE, ICPRB_NAME) %>% 
  distinct()

test2 <- semi_join(final.df, test, by = c("SITE", "ICPRB_NAME")) %>% 
  filter(!is.na(ResultAnalyticalMethod.MethodIdentifierContext))
table(test2$ResultAnalyticalMethod.MethodIdentifierContext)

sub.test <- final.df %>% 
  group_by(SITE, DATE, ICPRB_NAME, ICPRB_VALUE) %>% 
  filter(n() > 1)


sub.test$NA_COUNT <- rowSums(is.na(sub.test))

test <- sub.test %>% 
  group_by(SITE, DATE, ICPRB_NAME, ICPRB_VALUE) %>% 
  filter(NA_COUNT == min(NA_COUNT))

test2 <- test %>% 
  group_by(SITE, DATE, ICPRB_NAME, ICPRB_VALUE, TIME) %>% 
  filter(n() > 1) 


row1 <- test2[1,]
row2 <- test2[2,]
test <- as.data.frame(t(test2[1:2,]))
tt <- dplyr::setdiff(test$V1, test$V2)

test$ROW_2 <- row2[1, ]

tt <- apply(test2[1:2,], 2, unique)

tt <- dplyr::setdiff(row1, row2)
names(final.df)[!names(final.df) %in% names(test)]


test <- replace(final.df$ActivityMediaSubdivisionName, 
                final.df$ActivityMediaSubdivisionName == "", NA)
table(final.df$ResultAnalyticalMethod.MethodIdentifierContext)
nrow(final.df[is.na(final.df$ActivityMediaSubdivisionName), ])
length(test[is.na(test)])

test <- final.df[is.na(final.df$ActivityMediaSubdivisionName), ]
