library(tidyverse)
library(readxl)
library(lubridate)


#uses 'Payroll Detail Report with Dept' from ADP, using a 2-wk non-event period

#specify budget version to pull rates from the Resource Planner
#Resource Planner file MUST NOT BE OPEN
budgetversion = "2020 BoD 2"


# assume 80 hours per pay period for FT (x/80 per hour gives x per pay period)
FThours = 80

payrollsrc = read_csv(file =
                "source/Payroll Detail Report with Dept 20191021-20191103.csv")
deptmap = read_csv(file = "Dept Mapping.csv")
emplist = read_csv(file = "source/Employee List 20191114.csv")


#first identify full and part time employees based on ADP Pay Class
data = payrollsrc %>%
  left_join(deptmap, by = "Department") %>%
  filter(Dept == 69 |
           Dept == 72,
         `Pay Code` == "REGULAR") %>%
  left_join(emplist, by = "Position ID") %>%
  mutate(FTPT = if_else(str_detect(string = `Pay Class`,
                                   pattern = "FT") == T,
                        "FT",
                        if_else(str_detect(string = `Pay Class`,
                                           pattern = "PT") == T,
                                "PT",
                                "")),
         # set as data source for later steps
         Tenure = interval(mdy(`Hire/Rehire Date`),
                           mdy('10-28-2019')) / years(1)
         )


#find ratio of part time and full time employees by location and language
ratio = data %>%
  select(`Position ID`,
         Location,
         Dept,
         FTPT) %>%
  distinct() %>%
  group_by(Location,
           Dept,
           FTPT) %>%
  summarize(ct = n()) %>%
  spread(key = FTPT,
         value = ct,
         fill = 0) %>%
  summarize(PTr = (sum(PT) / sum(PT + FT)),
            FTr = (sum(FT) / sum(PT + FT)))

hours = data %>%
  group_by(Location,
           Dept,
           FTPT) %>%
  summarise(Hours = mean(Hours)) %>%
  ungroup() %>%
  mutate(Hours = if_else(FTPT == "FT", FThours, Hours))


#load wage table from the Resource Planner
rpswb = readxl::read_excel(path = "internalfilepath/Tidy Resource Planner.xlsx",
                           sheet = "Staff, Wage & Ben")

# pulls wages from Resource Planner for budgetversion
wages = rpswb %>%
  select(Month,
         Version,
         Location,
         Department,
         `Base Wage`) %>%
  mutate(`Base Wage` = if_else(is.na(`Base Wage`),
                               0,
                               `Base Wage`)) %>%
  filter(Version == budgetversion)



# to calculate PTO accrual based on hours (as is the case for PT), set per-hour
  # rates for both groups
# assume 80 hours per pay period for FT to keep them in the same calculation
accrual = tribble(~TenGrp,       ~FTPT,     ~PTOperHr,
                  "less than 1", "FT",      3.08 / FThours,
                  "1-5 yrs",     "FT",      4.62 / FThours,
                  "5-10 yrs",    "FT",      6.15 / FThours,
                  "> 10 yrs",    "FT",      7.69 / FThours,
                  "less than 1", "PT",       .04,
                  "1-5 yrs",     "PT",       .06,
                  "5-10 yrs",    "PT",       .08,
                  "> 10 yrs",    "PT",       .1)

tenur = data %>%
  select(Dept, Location, Tenure) %>%
  mutate(TenGrp = if_else(Tenure < 1, "less than 1",
                          if_else(Tenure < 4, "1-4 years",
                                  if_else(Tenure < )))
  )

