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

# 2) What courses are they taking together?

# filter, remove repeat courses
dat <- transcripts %>% filter(term == 2 | term == 3) %>% distinct(fig.key, system_key, dept_abbrev, course_number, .keep_all = T)


# taking any courses together:
co.courses <- dat %>%
  group_by(fig.key, cskey) %>%
  mutate(n.sect = n()) %>%
  filter(n.sect >= 2) %>%
  group_by(fig.key, add = F) %>%
  summarize(by.fig.nstu.any.joint.classes = n_distinct(system_key),
            fig.yrq = max(fig.yrq),
            fig.size = max(nstu)) %>%
  mutate(joint.prop = by.fig.nstu.any.joint.classes / fig.size) %>%
  ungroup()

co.courses$fig.prop.cut <- cut(co.courses$joint.prop, breaks = c(0, .25, .5, .75, .95, 1),
                            labels = c("0-25", "25-50", "50-75", "75-95", "100%"))
table(co.courses$fig.prop.cut)

co.courses %>% group_by(fig.yrq) %>% summarize(n = sum(fig.size),
                                            n_j = sum(by.fig.nstu.any.joint.classes),
                                            prop = n_j / n)



# other questions ---------------------------------------------------------

# what are the most common/freq FIG courses?
freq.courses <- transcripts %>% filter(term == 1, ckey != "0_GEN ST_19")

freq.courses <- freq.courses %>%
  group_by(fig.yrq, ckey, cskey) %>%
  distinct(fig.yrq, ckey, cskey) %>%
  group_by(fig.yrq, ckey, add = F) %>%      # annual
  summarize(nc = n())

freq.courses %>% group_by(ckey, add = F) %>% summarize(n = sum(nc)) %>% arrange(desc(n))
