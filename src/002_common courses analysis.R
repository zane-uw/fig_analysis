rm(list = ls())
gc()

library(tidyverse)


# setup -------------------------------------------------------------------
setwd(rstudioapi::getActiveProject())
load("data/fig-stu-transcripts.Rdata")

# plotting opts
theme_set(theme_bw(11))

# create terms 1...n
transcripts <- transcripts %>%
  group_by(fig.key) %>%
  arrange(fig.key, tran.yrq) %>%
  ungroup()

transcripts$term <- unlist(tapply(transcripts$tran.yrq,
                                  factor(transcripts$fig.key),
                                  function(x){
                                    r <- rle(x)
                                    e <- length(unique(x))
                                    return(rep(1:e, times = r$lengths))
                                  }), use.names = F)

# n-students
transcripts %>%
  filter(term == 1) %>%
  group_by(fig.yrq) %>%
  summarize(nstu = n_distinct(system_key),
            ncourse = n_distinct(ckey),
            nsect = n_distinct(cskey)) %>%
  ungroup()

# n-figs
transcripts %>%
  filter(term == 1) %>%
  group_by(fig.yrq, fig.key) %>%
  summarize(nstu = n_distinct(system_key),
            ncourse = n_distinct(ckey),
            nsect = n_distinct(cskey)) %>%
  ungroup()


# Analyses ----------------------------------------------------------------

# — 1) Are FIG students taking courses together their first year? ---------
#   1st year = terms 2 and 3, the winter/spring after the FIG
#   took at least 1 other course section with a student from same FIG

# filter, remove repeat courses, give students enough time to take more courses together
dat <- transcripts %>% filter(term == 2 | term == 3, fig.yrq >= 20134, tran.yrq >= 20134) %>% distinct(fig.key, system_key, dept_abbrev, course_number, .keep_all = T)

# taking any sections together:
fig.sect <- dat %>%
  group_by(fig.key, term, dept_abbrev, course_number, section_id) %>%
  mutate(n.sect = n()) %>%              # I may want to know the singletons later
  ungroup() %>%
  group_by(fig.key, term, dept_abbrev, course_number) %>%  # also would like to know the course reg in case they take the course but not same section
  mutate(n.course = n())


fig.syskeys <- unique(dat$system_key)
n1.classes <- unique(fig.sect$system_key[fig.sect$n.sect == 1])
n2plus <- unique(fig.sect$system_key[fig.sect$n.sect >= 2])

# so there were __ students that didn't take any sections with people from their fig
singles <- n1.classes[!(n1.classes %in% n2plus)]
length(singles)
length(singles) / length(fig.syskeys)      # _not_ 95% after adjusting for registration sections

(same.course.tab <- table('same course' = fig.sect$n.course, 'same sect' = fig.sect$n.sect))
sum(diag(same.course.tab)) / sum(same.course.tab)


# verify with dplyr syntax:
x <- fig.sect %>% group_by(fig.key) %>% filter(n.sect == 1) %>% distinct(fig.key, system_key) %>% ungroup()
y <- fig.sect %>% group_by(fig.key) %>% filter(n.sect >= 2) %>% distinct(fig.key, system_key) %>% ungroup()
z <- anti_join(x, y)
nrow(z) == length(singles)    # should be true
rm(x, y, z)

# common sections for proportions
co.sect <- fig.sect %>% ungroup() %>%
  group_by(fig.key) %>%                   # noticed that add = F was possibly buggy before(?)
  mutate(nstu = n_distinct(system_key)) %>%
  filter(n.sect >= 2) %>%
  summarize(by.fig.nstu.any.joint.classes = n_distinct(system_key),
            fig.yrq = max(fig.yrq),
            fig.size = max(nstu)) %>%
  mutate(joint.prop = by.fig.nstu.any.joint.classes / fig.size) %>%
  ungroup()

co.sect$fig.prop.cut <- cut(co.sect$joint.prop, breaks = c(0, .25, .5, .75, .95, 1),
                            labels = c("0-25", "25-50", "50-75", "75-95", "100%"))
table(co.sect$fig.prop.cut)

co.sect %>% group_by(fig.yrq) %>% summarize(n = sum(fig.size),
                                            n_j = sum(by.fig.nstu.any.joint.classes),
                                            prop = n_j / n)

sum(co.sect$by.fig.nstu.any.joint.classes)
length(unique(dat$system_key))
sum(co.sect$by.fig.nstu.any.joint.classes) / length(unique(dat$system_key))

# figs w/ no co sections at all?
length(unique(dat$fig.key[!(dat$fig.key %in% co.sect$fig.key)]))    # that few? huh


# — 2) what courses (not sections) are FIG students taking togethe --------

# w/ term
pop.courses <- dat %>%
  group_by(fig.key, term) %>%
  distinct(system_key, dept_abbrev, course_number, .keep_all = T) %>%
  count(ckey, sort = T)

# 3) Average # of sections a FIG student takes with someone else from the FIG
# in first 2 years
stu.sub <- transcripts %>%
  filter(tran.yrq <= fig.yrq + 19, regis_qtr != 3, tran.yrq > fig.yrq) %>%
  distinct(fig.key, system_key, cskey, .keep_all = T) %>%
  group_by(fig.key, cskey) %>%
  mutate(n.sect = n()) %>%
  ungroup()

stu.tally <- stu.sub %>% distinct(cskey, n.sect)
table(stu.tally$n.sect)
# of how many sections?
(ns <- length(unique(stu.tally$cskey)))   # indeed, b/c people from different FIGs might appear in the same section(s)

# stu.n <- stu.sub %>%
#   group_by(fig.key, tran.yrq, system_key) %>%
#   summarize(stu.n.sect = n_distinct(cskey),
#             stu.n.course = n_distinct(ckey))







# want to know what the average number of co-classes is -------------------

# take sections from the fig.sect data
co <- fig.sect %>%





# non-FIG students --------------------------------------------------------

# terms:
not.fig <- not.fig %>% group_by(yrq1) %>% arrange(yrq1, tran.yrq) %>% ungroup()
not.fig$term <- unlist(tapply(not.fig$tran.yrq,
                                  factor(not.fig$yrq1),
                                  function(x){
                                    r <- rle(x)
                                    e <- length(unique(x))
                                    return(rep(1:e, times = r$lengths))
                                  }), use.names = F)


# # first year
# nf.cohort <- not.fig %>%
#   filter(term == 1) %>%
#   mutate(cohort = paste(yrq1, cskey, sep = "_")) %>%
#   select(system_key, yrq1, cohort) %>%
#   distinct()
#
# nf.yr1 <- not.fig %>%
#   filter(term == 2 | term == 3) %>%
#   left_join(nf.cohort)

# how many didn't come back in those terms?
length(unique(nf.cohort$system_key[(nf.cohort$system_key %in% nf.yr1$system_key) == F]))    # 618

# co-classes w/ someone from same 'cohort' (section):
nf.co.sect <- nf.yr1 %>%
  group_by(cohort, cskey, add = F) %>%
  summarize(by.sect.nstu.joint.classes = n_distinct(system_key))

co.sect <- fig.sect %>%
  group_by(fig.key, add = F) %>%
  filter(n.sect >= 2) %>%
  summarize(by.fig.nstu.any.joint.classes = n_distinct(system_key),
            fig.yrq = max(fig.yrq),
            fig.size = max(nstu)) %>%
  mutate(joint.prop = by.fig.nstu.any.joint.classes / fig.size) %>%
  ungroup()



# other questions ---------------------------------------------------------

# what are the most common/freq FIG courses?
freq.courses <- transcripts %>% filter(term == 1, ckey != "0_GEN ST_199")

freq.courses <- freq.courses %>%
  group_by(fig.yrq, ckey, cskey) %>%
  distinct(fig.yrq, ckey, cskey) %>%
  group_by(fig.yrq, ckey, add = F) %>%      # annual
  summarize(nc = n())

freq.courses %>% group_by(ckey, add = F) %>% summarize(n = sum(nc)) %>% arrange(desc(n))
