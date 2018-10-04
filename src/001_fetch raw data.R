rm(list = ls())
gc()

# retrieve fig data based on gen st 199 sections -> sln(s) for associated classes + sections

# setup -------------------------------------------------------------------
library(tidyverse)
library(dbplyr)
library(odbc)

# helper function - create quoted vector, i.e. c(), from unquoted text
# not intended to smoothly handle punctuation but can be coerced a little, e.g. Cs(a, b, "?")
Cs <- function(...){as.character(sys.call())[-1]}

# To access local files/vars that are not part of repo, move up one level from project directory
setwd(rstudioapi::getActiveProject())

# server connection params
source("src/config.R")

con <- dbConnect(odbc::odbc(), dns, database = dabs[2], UID = uid, PWD = rstudioapi::askForPassword("pwd-"))


# source data -------------------------------------------------------------

# get FIG schedule line numbers (sln)
# children <- tbl(con, in_schema("sec", "time_sched_fig_child_sln")) %>%
#   filter(ts_year >= 2010) %>%
#   collect()

# After browsing the time sched table, parent_sln is not as useful as I hoped, it is virtually always the same as sln
# if it is "used with FIGS" then I can't imagine how
# instead I will use the GEN ST 199xx courses to create parents/keys for figs
parents <- tbl(con, in_schema("sec", "time_schedule")) %>%
  filter(ts_year >= 2010, ts_quarter == 4, dept_abbrev == "GEN ST", course_no == 199) %>%
  select(ts_year, ts_quarter, sln, parent_sln, course_branch, dept_abbrev, course_no, section_id) %>%
  collect()

# needs to also be keyed off the year and quarter since SLN's repeat frequently
# also, the GEN ST sections can share block registration children
genst <- tbl(con, in_schema("sec", "transcript_courses_taken")) %>%
  filter(tran_yr >= 2010, tran_qtr == 4, dept_abbrev == "GEN ST", course_number == 199) %>%
  select(system_key, tran_yr, tran_qtr, dept_abbrev, course_number, section_id, course_branch) %>%
  inner_join(parents,
             by = c("tran_yr" = "ts_year",
                    "tran_qtr" = "ts_quarter",
                    "dept_abbrev" = "dept_abbrev",
                    "course_number" = "course_no",
                    "section_id" = "section_id",
                    "course_branch" = "course_branch"),
             copy = T) %>%
  collect()

# now we can combine the system_keys with a fig key for shared gen st 199 registrations
genst <- genst %>%
  mutate_if(is.character, trimws) %>%
  mutate(fig.yrq = tran_yr*10 + tran_qtr,
  fig.key = paste(fig.yrq, section_id, sln, sep = "_")) %>%
  select(system_key, fig.yrq, fig.key, fig.yr = tran_yr, fig.qtr = tran_qtr) %>%
  distinct()

stus <- genst %>% select(system_key, fig.yrq, fig.yr, fig.qtr, fig.key) %>% arrange(fig.yrq) %>% distinct(system_key, .keep_all = T)


# fetch x_transcripts_x REGISTRATIONS for fig students
transcripts <- tbl(con, in_schema("sec", "registration_courses")) %>%
  filter(regis_yr >= 2013,
         `repeat` %in% c("0", ""),
         request_status %in% c('A', 'C', 'R'),
         dup_enroll == "") %>%
  select(system_key, regis_yr, regis_qtr, index1, sln, course_branch, grade_dt,            # grade_dt may be useful to filter
         dept_abbrev = crs_curric_abbr,
         course_number = crs_number,
         section_id = crs_section_id) %>%
  inner_join(stus,
             by = "system_key",
             copy = T) %>%
  collect() %>%
  mutate_if(is.character, trimws) %>%
  mutate(tran.yrq = regis_yr*10 + regis_qtr,
         ckey = paste(tran.yrq, sln, course_branch, dept_abbrev, course_number, sep = "_"),
         cskey = paste(ckey, section_id, sep = "_")) %>%
  filter(tran.yrq >= 20134, tran.yrq >= fig.yrq)

# Reduce to the 2 digit section # where possible
transcripts <- transcripts %>%
  mutate(l = str_length(section_id)) %>%
  group_by(system_key, tran.yrq, dept_abbrev, course_number) %>%
  filter(l == max(l)) %>%
  select(-l) %>%
  ungroup()

# child transcripts
chld <- tbl(con, in_schema("sec", "time_sched_fig_child_sln")) %>% collect()
chld <- chld %>%
  filter(ts_year >= 2010, ts_quarter == 4, dept_abbrev == "GEN ST")

chld.sln <- tbl(con, in_schema("sec", "time_schedule")) %>%
  select(ts_year, ts_quarter, course_no, dept_abbrev, section_id, sln) %>%
  inner_join(chld, copy = T) %>%
  collect() %>%
  mutate_if(is.character, trimws) %>%
  mutate(fig.yrq = ts_year*10 + ts_quarter,
         fig.key = paste(fig.yrq, section_id, sln, sep = "_")) %>%
  select(fig.key, ts_year, ts_quarter, fig.yrq, fig_child_sln, chld.index = index1)

chld.data <- tbl(con, in_schema("sec", "time_schedule")) %>%
  select(ts_year, ts_quarter, course_no, dept_abbrev, section_id, sln) %>%
  inner_join(chld.sln, by = c("sln" = "fig_child_sln",
                              "ts_year" = "ts_year",
                              "ts_quarter" = "ts_quarter"),
             copy = T) %>%
  collect() %>%
  mutate_if(is.character, trimws)

#####
# can't do _this_ part unfortunately; even with further filtering by system_keys it's still not accurate. Some section ID's just appear to be
# missing or wrong, I can't tell. I first noticed this with ART 120 in 2010 - there are far more people in that class than in the FIG and
# the transcripts don't appear to have all the sections that they should based on searching it via the SWS. So the child sln's can't be
# used to reliably get a set of FIG students - only the course info itself. I could still use this to tag the 'core' FIG classes for each FIG.

# chld <- chld %>% mutate(fig.key = paste(paste0(ts_year, ts_quarter), trimws(section_id), sep = "_")) %>%
#   select(ts_year, ts_quarter, fig.key, sln = fig_child_sln)
# chld.sln <- tbl(con, in_schema("sec", "time_schedule")) %>%
#   select(ts_year, ts_quarter, course_no, dept_abbrev, section_id, sln) %>%
#   inner_join(chld, copy = T) %>%
#   collect()
# fig.child <- tbl(con, in_schema("sec", "transcript_courses_taken")) %>%
#   filter(tran_yr >= 2010) %>%
#   select(system_key, tran_yr, tran_qtr, dept_abbrev, course_number, section_id, course_branch, grade_system, grade) %>%
#   inner_join(chld.sln,
#              by = c("tran_yr" = "ts_year",
#                     "tran_qtr" = "ts_quarter",
#                     "course_number" = "course_no",
#                     "dept_abbrev" = "dept_abbrev",
#                     "section_id" = "section_id"),
#              copy = T) %>%
#   collect() %>%
#   mutate_if(is.character, trimws) %>%
#   mutate(yrq = tran_yr*10 + tran_qtr) %>%
#   filter(system_key %in% transcripts$system_key)
#####


# Non-fig FTFY students ---------------------------------------------------

acon <- dbConnect(odbc::odbc(), dns, Database = dabs[1], UID = uid,
                  PWD = rstudioapi::askForPassword("pwd-"))

ftfy <- tbl(acon, in_schema("sec", "IV_StudentFactSheet")) %>%
  filter(AcademicQtrKeyId >= 20134, AcademicQtrKeyId <= 20174, AcademicQtr == 4,
         StudentLevelTaxonomyKey == '00010', AcademicCareerEntryType == "FTFY", AcademicCareerLevelEnrolledTerm == 1) %>%
  select(system_key = SDBSrcSystemKey, yrq1 = AcademicQtrKeyId, qtr1 = AcademicQtr) %>%
  collect() %>%
  mutate(yr1 = yrq1 %/% 10)

ftfy <- ftfy[!(ftfy$system_key %in% stus$system_key),]


# problems:
# 1) transcript_courses_taken doesn't show sections w/ fine detail, only top level
# 2) registration is _every_ registration, so need to really slim it down
not.fig <- tbl(con, in_schema("sec", "registration_courses")) %>%
  filter(regis_yr >= 2013,
         `repeat` %in% c("0", ""),
         request_status %in% c("A", "C", "R"),
         dup_enroll == "") %>%
  select(system_key, regis_yr, regis_qtr, index1, sln, course_branch, grade_dt,
         dept_abbrev = crs_curric_abbr,
         course_number = crs_number,
         section_id = crs_section_id) %>%
  inner_join(ftfy, by = "system_key", copy = T) %>%
  collect() %>%
  mutate_if(is.character, trimws) %>%
  mutate(tran.yrq = regis_yr*10 + regis_qtr,
         ckey = paste(tran.yrq, sln, course_branch, dept_abbrev, course_number, sep = "_"),
         cskey = paste(ckey, section_id, sep = "_"))

# sift to get only the sub-sections, not top-level courses
not.fig <- not.fig %>%
  mutate(l = str_length(section_id)) %>%
  group_by(system_key, tran.yrq, dept_abbrev, course_number) %>%
  filter(l == max(l)) %>%
  select(-l) %>%
  ungroup()                             # discard the large lecture components of classes with lecture/quiz sections

# not.fig <- not.fig %>%
#   group_by(system_key, tran.yrq, ckey) %>%
#   filter(index1 == max(index1))

# keep first 2 years, no summers
not.fig <- not.fig %>% group_by(system_key) %>%
  filter(tran.yrq >= yrq1, tran.yrq < yrq1+19, regis_qtr != 3) %>%
  distinct(system_key, cskey, .keep_all = T)

# create a 'fig key' for the non-fig students using the course sections from their first yrq
x <- not.fig %>% filter(tran.yrq == yrq1) %>%
  mutate(not.figkey = paste(yrq1, dept_abbrev, course_number, section_id, sep = "_")) %>%
  select(system_key, not.figkey) %>%
  distinct()

# going to end up with FAR more rows than the fig students b/c every course is a key here, not just the gen-studies
# but we can reduce the n a little bit by getting rid of first quarter classes with only 1 entry

# check fig sizes
t <- genst %>% group_by(fig.key) %>% summarize(n = n())
table(t$n)
# keep sizes from 9:25
nx <- x %>% group_by(not.figkey) %>% mutate(n = n()) %>% ungroup() %>% filter(n %in% 9:25)
x <- x[x$system_key %in% nx$system_key,]

not.fig <- x %>% inner_join(not.fig, by = "system_key")

# save --------------------------------------------------------------------

save(transcripts, not.fig, genst, chld.data, file = "data/fig-stu-transcripts.Rdata")







