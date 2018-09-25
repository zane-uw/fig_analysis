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


# fetch transcripts for fig students
transcripts <- tbl(con, in_schema("sec", "transcript_courses_taken")) %>%
  filter(tran_yr >= 2010, repeat_course == 0) %>%
  select(system_key, tran_yr, tran_qtr, dept_abbrev, course_number, section_id, course_branch,
         index1, grade_system, grade) %>%
  inner_join(stus,
             by = "system_key",
             copy = T) %>%
  collect() %>%
  mutate_if(is.character, trimws) %>%
  mutate(tran.yrq = tran_yr*10 + tran_qtr,
         ckey = paste(course_branch, dept_abbrev, course_number, sep = "_"),
         cskey = paste(ckey, section_id, sep = "_")) %>%
  filter(tran.yrq >= 20104, tran.yrq >= fig.yrq)

# oddity check
  # any(transcripts$tran.yrq < transcripts$fig.yrq)
  # x <- transcripts %>% filter(tran.yrq < fig.yrq)
  # transcripts <- transcripts %>% filter(tran.yrq >= fig.yrq)

# # get system_keys for students in the above child courses (the fig courses)
# child.trans <- tbl(con, in_schema("sec", "transcript_courses_taken")) %>%
#   filter(tran_yr >= 2010, tran_qtr == 4) %>%
#   select(system_key, tran_yr, tran_qtr, dept_abbrev, course_number, section_id, course_branch) %>%
#   inner_join(children,
#              by = c("tran_yr" = "ts_year",
#                     "tran_qtr" = "ts_quarter",
#                     "dept_abbrev" = "dept_abbrev",
#                     "course_number" = "course_no",
#                     "section_id" = "section_id",
#                     "course_branch" = "course_branch"),
#              copy = T) %>%
#   collect() %>%
#   mutate(fig.yrq = tran_yr*10 + tran_qtr)                     # just curious - .001 sec for n = 10e4
#   # mutate(fig.yrq = as.numeric(paste0(tran_yr, tran_qtr)))   # .15 sec for n = 10e4

# child transcripts (e.g. fig course transcripts only and to flag fig core classes)
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

# save --------------------------------------------------------------------

save(transcripts, genst, chld.data, file = "data/fig-stu-transcripts.Rdata")
