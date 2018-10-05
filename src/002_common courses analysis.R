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

# Decisions:
# Filter cohort sizes into comparable ranges between FIG/non-FIG, 10-30
#


# — 1) Are FIG students taking courses together their first year? ---------
#   1st year = terms 2 and 3, the winter/spring after the FIG
#   took at least 1 other course section with a student from same FIG

# initial subset = first wi/spr
dat <- transcripts %>%
  group_by(fig.key) %>%
  mutate(fig.size = n_distinct(system_key)) %>%
  ungroup() %>%
  filter(fig.size >= 10, fig.size <= 30, term == 2 | term == 3, fig.yrq >= 20134, tran.yrq >= 20134) %>%
  distinct(fig.key, system_key, dept_abbrev, course_number, section_id, .keep_all = T)
# remove large sections (> 30 students)
dat <- dat %>% group_by(term, cskey) %>% mutate(sect.size = n_distinct(system_key)) %>% filter(sect.size <= 30) %>% ungroup()

# taking sections together:
fig.sect <- dat %>%
  group_by(fig.key, term, dept_abbrev, course_number, section_id) %>%
  mutate(n.fig.sect = n_distinct(system_key)) %>%              # I may want to know the singletons later; n() would work but why not be more specific
  ungroup() %>%
  group_by(fig.key, term, dept_abbrev, course_number) %>%  # also would like to know the course reg in case they take the course but not same section
  mutate(n.fig.course = n_distinct(system_key)) %>%
  ungroup()


fig.syskeys <- unique(dat$system_key)
n1.classes <- fig.sect %>% filter(n.fig.sect == 1) %>% distinct(system_key)
n2plus <- fig.sect %>% filter(n.fig.sect >= 2) %>% distinct(system_key)

# so there were __ students that didn't take any sections with people from their fig
singles <- setdiff(n1.classes$system_key, n2plus$system_key)
length(singles)
length(singles) / length(fig.syskeys)      # _not_ 95% after adjusting for registration sections and getting rid of huge lectures

(same.course.tab <- table('same course' = fig.sect$n.fig.course, 'same sect' = fig.sect$n.fig.sect))

# verify with dplyr syntax:
x <- fig.sect %>% group_by(fig.key) %>% filter(n.fig.sect == 1) %>% distinct(fig.key, system_key) %>% ungroup()
y <- fig.sect %>% group_by(fig.key) %>% filter(n.fig.sect >= 2) %>% distinct(fig.key, system_key) %>% ungroup()
z <- anti_join(x, y)
nrow(z) == length(singles)    # should be true
rm(x, y, z)


# common sections for proportions
co.sect <- fig.sect %>% ungroup() %>%
  group_by(fig.key) %>%                   # noticed that add = F was possibly buggy before(?)
  mutate(nstu = n_distinct(system_key)) %>%
  filter(n.fig.sect >= 2) %>%
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
  distinct(system_key, dept_abbrev, course_number, .keep_all = T) %>% ungroup()

(p <- pop.courses %>% group_by(dept_abbrev, course_number) %>% summarize(pop = n_distinct(system_key)) %>% arrange(desc(pop)))


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



# want to know what the average number of co-classes is -------------------

# by student, n.co >= 2, n distinct sections, mean
x <- fig.sect %>% mutate(cut = if_else(n.fig.sect == 1, "1", "2+")) %>% group_by(system_key) %>% mutate(stu.n.courses = n_distinct())




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


# # how many didn't come back in those terms?
# length(unique(not.fig$system_key[(nf.cohort$system_key %in% nf.yr1$system_key) == F]))    # 618

nfdat <- not.fig %>%
  group_by(not.figkey) %>%
  mutate(nf.size = n_distinct(system_key)) %>%
  ungroup() %>%
  filter(term == 2 | term == 3, nf.size >= 10, nf.size <= 30, yrq1 >= 20134, tran.yrq >= 20134) %>%
  group_by(cskey) %>%
  mutate(sect.size = n_distinct(system_key)) %>%
  filter(sect.size <= 30) %>%
  ungroup() %>%
  group_by(not.figkey, cskey) %>%
  mutate(n.common.sect = n_distinct(system_key)) %>%
  ungroup() %>%
  group_by(not.figkey, ckey) %>%
  mutate(n.common.course = n_distinct(system_key)) %>%
  ungroup()


nfco <- nfdat %>%
  group_by(not.figkey, add = F) %>%
  filter(n.common.sect >= 2) %>%
  summarize(by.notfig.nstu.any.joint.classes = n_distinct(system_key)) %>%
  ungroup()


# so the proportion of non-fig students who have any joint classes is:
nf1 <- unique(nfdat$system_key[nfdat$n.common.sect == 1])
nf2plus <- unique(nfdat$system_key[nfdat$n.common.sect > 1])
nfsingles <- nf1[!(nf1 %in% nf2plus)]

length(nfsingles) / length(unique(nfdat$system_key))




# Section - to - section comparison ---------------------------------------

fcse <- transcripts %>% filter(term == 1, dept_abbrev == "CSE", course_number == 142) %>% mutate(id = paste0(fig.yrq, dept_abbrev, course_number, section_id)) %>%
  select(system_key, id) %>% distinct()

nfcse <- not.fig %>% filter(term == 1, dept_abbrev == "CSE", course_number == 142) %>% mutate(id = paste0(tran.yrq, dept_abbrev, course_number, section_id)) %>%
  select(system_key, id) %>% distinct()

fcse <- fcse %>% inner_join(transcripts, by = "system_key") %>% filter(term == 2 | term == 3) # %>% select(id, term, tran.yrq, dept_abbrev, course_number, section_id)
n <- not.fig %>% select(system_key, term, tran.yrq, dept_abbrev, course_number, section_id) %>% distinct()
nfcse <- nfcse %>% inner_join(n, by = "system_key") %>% filter(term == 2 | term == 3) # %>% select(id, term, tran.yrq, dept_abbrev, course_number, section_id)

x <- fcse %>% group_by(id, term, dept_abbrev, course_number, section_id) %>% summarize(n = n_distinct(system_key))
y <- nfcse %>% group_by(id, term, dept_abbrev, course_number, section_id) %>% summarize(n = n_distinct(system_key))

table(x$n)
table(y$n)

sum(x$n > 1) / nrow(x)
sum(y$n > 1) / nrow(y)

