rm(list = ls())
gc()

library(tidyverse)


# setup -------------------------------------------------------------------
load("data/fig-stu-transcripts.Rdata")

theme_set(theme_bw(11))

# fig.sizes
# sz <- genst %>% group_by(fig.key) %>% summarize(nstu = length(unique(system_key))) %>% ungroup() %>% arrange(desc(nstu))
# nsect <- chld.data %>% group_by(fig.key) %>% summarize(fig.nsect = max(chld.index)) %>% ungroup() %>% arrange(desc(fig.nsect))      # w sections
# ncourse <- chld.data %>% group_by(fig.key) %>% summarize(fig.ncourse = n_distinct(paste0(dept_abbrev, course_no))) %>% ungroup() %>% arrange(desc(fig.ncourse))      # w/o sections

# head(sz, 10)
# head(ncourse, 10)
# head(nsect, 10)

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

# annual summary
N <- transcripts %>%
  filter(term == 1) %>%
  group_by(fig.yrq) %>%
  summarize(nstu = n_distinct(system_key),
            ncourse = n_distinct(ckey),
            nsect = n_distinct(cskey)) %>%
  ungroup()

# FIG summary:
# annual summary
nfig <- transcripts %>%
  filter(term == 1) %>%
  group_by(fig.yrq, fig.key) %>%
  summarize(nstu = n_distinct(system_key),
            ncourse = n_distinct(ckey),
            nsect = n_distinct(cskey)) %>%
  ungroup()

transcripts <- transcripts %>% left_join(nfig, by = c("fig.key", "fig.yrq"))



# Analyses ----------------------------------------------------------------

# 1) Are FIG students taking courses together their first year?
#   1st year = terms 2 and 3, the winter/spring after the FIG
#   took at least 1 other course section with a student from same FIG

# filter, remove repeat courses, give students enough time to take more courses together
dat <- transcripts %>% filter(term == 2 | term == 3) %>% distinct(fig.key, system_key, dept_abbrev, course_number, .keep_all = T)


# taking any sections together:
fig.sect <- dat %>%
  group_by(fig.key, cskey) %>%
  mutate(n.sect = n())          # I may want to know the singletons later

sys.keys <- unique(dat$system_key)
n1.classes <- unique(fig.sect$system_key[fig.sect$n.sect == 1])
n2plus <- unique(fig.sect$system_key[fig.sect$n.sect >= 2])

# so there were __ students that didn't take any classes with people from their fig
singles <- n1.classes[!(n1.classes %in% n2plus)]
length(singles)
length(singles) / length(sys.keys)      # only 5% of FIG students don't take a class with someone else from their FIG

# verify with dplyr syntax:
x <- fig.sect %>% group_by(fig.key) %>% filter(n.sect == 1) %>% distinct(fig.key, system_key) %>% ungroup()
y <- fig.sect %>% group_by(fig.key) %>% filter(n.sect >= 2) %>% distinct(fig.key, system_key) %>% ungroup()
z <- anti_join(x, y)
# CHECK
rm(x, y, z)

# ex/test:
# x <- transcripts %>% filter(fig.key == "20154_A1_14948", tran.yrq == 20161) %>% group_by(cskey) %>% mutate(nsect = n()) %>% ungroup()
# x %>% filter(nsect >= 2) %>% summarize(n_distinct(system_key))

co.sect <- fig.sect %>%
  group_by(fig.key, add = F) %>%
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
length(unique(transcripts$system_key))
sum(co.sect$by.fig.nstu.any.joint.classes) / length(unique(transcripts$system_key))


# FIGS w/o any common sections =
no.co.sects <- nfig[!(nfig$fig.key %in% co.sect$fig.key),]
nrow(no.co.sects)
sum(no.co.sects$nstu)


# 2) what courses (not sections) are FIG students taking together?

co.courses <- dat %>%
  group_by(fig.key) %>%
  distinct(system_key, ckey, .keep_all = T) %>%
  count(ckey, sort = T)


# 3) Average # of courses a FIG student takes with someone else from the FIG
stu.sub <- transcripts %>%
  filter(tran.yrq <= fig.yrq + 19, tran_qtr != 3) %>%
  distinct(fig.key, system_key, cskey, .keep_all = T)
#
# stu.avg <- stu.avg %>%
#   group_by(fig.key, tran.yrq, cskey) %>%
#   mutate(n.co.stu = n()) %>%
#   group_by(fig.key, tran.yrq, system_key, add = F) %>%
#   mutate(n.sect.qtr = n_distinct(cskey))

stu.n <- stu.sub %>%
  group_by(fig.key, tran.yrq, system_key) %>%
  summarize(stu.n.sect = n_distinct(cskey),
            stu.n.course = n_distinct(ckey))

stu.co <- stu.sub %>%
  group_by(tran.yrq, fig.key, cskey) %>%
  mutate(stu.co.sect = n_distinct(system_key)) %>%
  group_by(system_key, tran.yrq, fig.key, cskey) %>%






# want to know what the average number of co-classes is -------------------

# take sections from the fig.sect data
co <-





# other questions ---------------------------------------------------------

# what are the most common/freq FIG courses?
freq.courses <- transcripts %>% filter(term == 1, ckey != "0_GEN ST_199")

freq.courses <- freq.courses %>%
  group_by(fig.yrq, ckey, cskey) %>%
  distinct(fig.yrq, ckey, cskey) %>%
  group_by(fig.yrq, ckey, add = F) %>%      # annual
  summarize(nc = n())

freq.courses %>% group_by(ckey, add = F) %>% summarize(n = sum(nc)) %>% arrange(desc(n))
