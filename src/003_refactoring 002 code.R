rm(list = ls())
gc()

library(tidyverse)

load("data/fig-stu-transcripts.Rdata")

setwd(rstudioapi::getActiveProject())
load("data/fig-stu-transcripts.Rdata")

# plotting opts
theme_set(theme_bw(11))

# create terms 1...n; labels
transcripts <- transcripts %>%
  filter(regis_qtr != 3) %>%
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

table(transcripts$term)
(u <- unique(transcripts$term))
(l <- rep_along(u, c("Fall", "Winter", "Spring")))
(l <- paste(l, rep(1:length(u), each = 3, length.out = length(u)), sep = "-"))
transcripts$termlab <- factor(transcripts$term, levels = u, labels = l)
rm(u, l)

# # n-students
# (info.sum <- transcripts %>%
#     filter(term == 1) %>%
#     group_by(fig.yrq) %>%
#     summarize(yr.nstu = n_distinct(system_key),
#               # yr.ncourse = n_distinct(ckey),
#               # yr.nfigsect = n_distinct(cskey),
#               nfigs = n_distinct(fig.key)) %>%
#     ungroup())

# n-figs
(info.fig <- transcripts %>%
  filter(term == 1) %>%
  group_by(fig.yrq, fig.key) %>%
  summarize(fig.nstu = n_distinct(system_key)) %>% #,
            # ncourse = n_distinct(ckey),
            # nsect = n_distinct(cskey)) %>%
  ungroup())

(cohort.size <- transcripts %>%
    filter(term == 1) %>%
    group_by(fig.key) %>%
    mutate(s = n_distinct(system_key)) %>%
    filter(s >= 10, s <= 30) %>%
    ungroup() %>%
    group_by(fig.yrq) %>%
    summarize(yr.nstu = n_distinct(system_key),
              yr.avgfig = mean(s)))


k <- info.fig$fig.key[info.fig$fig.nstu > 1 & info.fig$fig.nstu <= 30]

# create subsets for yr1 and year 1+2
yr1 <- transcripts %>%
  filter(term == 2 | term == 3, fig.key %in% k) %>%
  distinct()

yr12 <- transcripts %>%
  filter(term > 1, term <= 6, fig.key %in% k, dept_abbrev != "NUTR" & course_number != 300) %>%
  distinct()



# Most popular courses ----------------------------------------------------

popular.courses <- yr12 %>%
  filter(fig.yrq <= 20164) %>%
  group_by(term, termlab, dept_abbrev, course_number) %>%
  summarize(size = n_distinct(system_key)) %>%
  ungroup() %>%
  arrange(termlab, desc(size)) %>%
  group_by(termlab) %>%
  mutate(r = row_number()) %>%
  filter(r <= 20)


# popular courses: how many FIGS do they appear in? -----------------------







# 2 year graphic ----------------------------------------------------------

# fig sizes = info.fig

# Most commonly taken sections together (aggregated up to course level)
grsect <- yr12 %>%
  filter(fig.yrq <= 20164) %>%
  group_by(fig.key, term, termlab, dept_abbrev, course_number, section_id, cskey) %>%
  summarize(co.section = n_distinct(system_key)) %>%
  filter(co.section > 1) %>%
  ungroup()

grsect <- grsect %>%
  group_by(term, termlab, dept_abbrev, course_number) %>%
  summarize(n.co.section.by.course = sum(co.section)) %>%
  ungroup() %>%
  arrange(termlab, desc(n.co.section.by.course))

# Most commonly taken courses together, with or without someone else in FIG
grcourse <- yr12 %>%
  filter(fig.yrq <= 20164) %>%
  group_by(fig.key, term, termlab, dept_abbrev, course_number) %>%
  summarize(co.course = n_distinct(system_key)) %>%
  ungroup() %>%
  filter(co.course > 1)

grcourse <- grcourse %>%
  group_by(term, termlab, dept_abbrev, course_number) %>%
  summarize(n.co.course = sum(co.course)) %>%
  ungroup() %>%
  arrange(termlab, desc(n.co.course))

# spreadsheet branches off here ^^
spreadsheet <- full_join(grsect, grcourse) %>%
  filter(n.co.section.by.course <= n.co.course) %>%
  mutate(perc = n.co.section.by.course / n.co.course,
         courselab = paste(dept_abbrev, course_number, sep = " ")) %>%
  filter(perc <= 1) %>%
  arrange(termlab, desc(n.co.section.by.course))

# now back to graphic
ft <- unique(paste(popular.courses$dept_abbrev, popular.courses$course_number, sep = " "))
ft <- ft[order(ft)]

gr12 <- spreadsheet %>%
  filter(courselab %in% ft) %>%
  arrange(termlab, desc(n.co.section.by.course)) %>%
  group_by(termlab) %>%
  filter(row_number() <= 14)


# Tile plot:
# tile:
p <- ggplot(data = gr12, aes(x = termlab, y = courselab, label = sprintf("%0.2f", round(perc, digits = 2)))) +
  geom_tile(aes(fill = 100*gr12$perc), alpha = .8) +
  scale_fill_viridis_c(begin = .1, end = .8, direction = -1, option = "plasma") +
  geom_text(fontface = "bold") +
  scale_y_discrete(limits = rev(unique(sort(gr12$courselab)))) +
  ylab("Course - common sections") +
  xlab("Quarter (in first two years)") +
  labs(fill = "%",
       subtitle = "Percent of students taking section with \nat least 1 other student from same FIG")
p


# back to the spreadsheet:
out <- spreadsheet %>% filter(term <= 3) %>%
  group_by(courselab) %>%
  summarize('common course+section' = sum(n.co.section.by.course),
            'common course' = sum(n.co.course)) %>%
  mutate(percent = `common course+section` / `common course`)

# supplement with number of sections per course
course.sect <- yr12 %>%
  filter(term <= 3) %>%
  group_by(termlab, dept_abbrev, course_number) %>%
  summarize(sections = n_distinct(section_id))
course.sect <- course.sect %>%
  mutate(courselab = paste(dept_abbrev, course_number, sep = " ")) %>%
  group_by(courselab, add = F) %>%
  summarize(sections = sum(sections)) %>%
  arrange(desc(sections))

out <- out %>% left_join(course.sect) %>% arrange(desc(`common course+section`))

write_csv(out, "../most popular common sections and courses.csv", col_names = T)
