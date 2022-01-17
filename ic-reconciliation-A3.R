library(readr)
library(odbc)
library(dplyr)
library(dbplyr)
#library(ggplot2)
library(lubridate)
library(tidyr)

#IC Rate Filter#
# 2020-01-09
# IC rate filters, with an added section to separate spiff entries
# into separate rows for each date, for the spiff validation
##

# not used, kept as a reference for how week start is set
options(lubridate.week.start = 1)

# 
r.start = floor_date(x = Sys.Date(), unit = "months") - months(1)
r.end = floor_date(x = Sys.Date(), unit = "months") - days(1)
mo = r.start %--% r.end

version = "20200108 0917"

icsr = read_csv(file = sprintf("source/%s", version),
                # icsr = read_csv(file = "address to internal server location",
                col_names = F)

names(icsr) = c("ID", "AgentNo", "AgentName", "Location", "SiteID", "QueueNo",
                "Type", "Start", "End", "Day", "Night", "0")

# Create new tibble, filtered for rates active in the month in question
# Anything outside of the month range is excluded
rates = icsr %>%
  mutate_at(vars(Start, End), mdy) %>%
  filter(int_overlaps(Start %--% End,
                      mo)) %>%
  select(ID, AgentNo, AgentName, Location, SiteID,
         QueueNo, Type, Start, End, Day, Night)

# Separate Spiff entries into individual dates per row
spiffbyday = rates %>%
  filter(Type == "SPIFF") %>%
  group_by(ID, AgentNo) %>%
  mutate(`Spiff Date` = list(seq(Start,
                                 End,
                                 by = "1 day"))) %>%
  unnest() %>%
  ungroup() %>%
  transmute(AgentNo, AgentName, Location, SiteID,
            `AgentNo QueueNo` = paste(AgentNo, QueueNo, sep = " "),
            QueueNo, Type, Start, `Spiff Date`, End, Day, Night) 

# Save spread spiffs
spiffbyday %>%
  data.table::fwrite(file = sprintf("output/%s spiffs by day.csv", version))


# Saves a separate file for regular vs Spiff rates
rates %>%
  filter(Type == "OPI") %>%
  data.table::fwrite(file = sprintf("output/%s opi.csv", version))

rates %>%
  filter(Type == "SPIFF") %>%
  data.table::fwrite(file = sprintf("output/%s spiff.csv", version))


rm(r.start, r.end, mo, version, icsr, rates)
#end rate filtering section
###############################################################################



###############################################################################
#Call Data Pull#
#
#query works as intended
#goal is not only to use above-pulled rate info to calculate payment,
                # but also to automatically check for missing spiffs

# question: what is the best way to identify spiffs here?
  # 

con = dbConnect(odbc::odbc(), .connection_string="Driver={SQL Server};
                                                  Server=cyrsql00;
                                                  Database=warehouse;
                                                  Integrated Security=True;
                                                  Persist Security Info=False")

# establish previous date range
qstart = floor_date(x = now(), unit = "months") - months(1)
qend = floor_date(x = now(), unit = "months")
#mo = s %--% e

#establish mapping table for agents, which will be used as a filter
n = tbl(con, "navAgent") %>%
  filter(AgentType != "Employee",
         AgentNo != "123456") %>% #exclude IT test profile
  select(AgentNo)

#QueueMapping table, with Spanish/NonSpan categorization
qmap = tbl(con, in_schema("Mapping.dbo", "QueueMapping")) %>%
  mutate(Language = if_else(QueueMap == 'B Port', 'NonSpan', QueueMap))


#as of 2019-08-21 this query pulls the exact same calls and durations for the
  # month as the raw SQL
c = tbl(con, "AgentCall") %>%
  inner_join(n, by = ("AgentNo" = "AgentNo")) %>%
  inner_join(qmap, by = ("QueueNo" = "QueueNo")) %>%
  filter(CallStartTime >= qstart,
         CallStartTime < qend,
         CallsAnswAgentDur + CallsAdHocAgentDur + QDurAgents > 0) %>%
  transmute(AgentNo,
            QueueNo,
            QueueName,
            Language,
            CallId,
            CallStartTime, #call time comes in as UTC
            CallDur = CallsAnswAgentDur + CallsAdHocAgentDur + QDurAgents)

calls = collect(c)

caldat = calls %>%
  mutate(CallEndTime = (CallStartTime + seconds(Dur)),
         CallInterval = CallStartTime %--% CallEndTime,
         secs = as.numeric(CallInterval),
         CallDur
         ) %>%
  summarise(Dur = sum(CallDur))

