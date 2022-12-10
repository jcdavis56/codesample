library(tidyverse)
library(rvest)
library(styler)
library(tidytext)
library(textdata)
library(udpipe)
library(SnowballC)
library(nametagger)
library(contactdata)
library(countrycode)
library(ggthemes)
library(ggraph)
library(igraph)

setwd("~/DATA II")

path_out <- "C:\\Users\\joshu\\OneDrive\\Documents\\DATA II"

links <- c(
  "https://www.govinfo.gov/content/pkg/CHRG-117hhrg47848/html/CHRG-117hhrg47848.htm",
  "https://www.govinfo.gov/content/pkg/CHRG-115hhrg30215/html/CHRG-115hhrg30215.htm",
  "https://www.govinfo.gov/content/pkg/CHRG-112hhrg77480/html/CHRG-112hhrg77480.htm"
)

retrieve_hearings <- function(urls) {
  for (each_link in urls) {

    # add a break each request so we don't produce a 429 error
    Sys.sleep(1)

    names <- substring(each_link, 42, 53)

    request <- read_html(each_link)

    element <- html_element(request, "pre")

    text_list <- html_text(element)

    text <- paste(text_list, collapse = "")

    writeLines(text, paste(names, ".txt", sep = ""))
  }
}

retrieve_hearings(links)

file_list <- list.files(path_out,
  pattern = ".*.txt"
)

hearings_list <- lapply(file_list, function(x) read_file(x))

hearings <- do.call("rbind", hearings_list)

hearings_all <- tibble(text = hearings)

hearings_all <- hearings_all %>% mutate(session = c("112", "115", "117"))

word_tokens <- unnest_tokens(hearings_all, word_tokens, text, token = "words")

hearings_no_sw <- anti_join(word_tokens, stop_words, by = c("word_tokens" = "word"))

sentiment_nrc <-
  get_sentiments("nrc") %>%
  rename(nrc = sentiment)

sentiment_bing <-
  get_sentiments("bing") %>%
  rename(bing = sentiment)

analysis_nrc <- hearings_no_sw %>%
  left_join(sentiment_nrc, by = c("word_tokens" = "word"))

analysis_bing <- hearings_no_sw %>%
  left_join(sentiment_bing, by = c("word_tokens" = "word"))

analysis_nrc %>%
  filter(!is.na(nrc)) %>%
  group_by(nrc, session) %>%
  count() %>%
  head()

analysis_bing %>%
  filter(!is.na(bing)) %>%
  group_by(bing, session) %>%
  count() %>%
  head()

# identify popular topics

hearings_no_sw %>%
  group_by(word_tokens, session) %>%
  count() %>%
  filter(n > 25) %>%
  arrange(desc(n)) %>%
  view()

nrc_plot <- ggplot(data = filter(analysis_nrc, !is.na(nrc))) +
  geom_histogram(aes(nrc), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_grid(cols = vars(session)) +
  labs(
    title = "Sentiment of the Congressional Energy & Commerce Subcommittee",
    subtitle = "Analysis of Sessions 112, 115, and 117",
    y = "Number of Observations",
    x = "Feeling Expressed"
  ) +
  theme_economist()

bing_plot <- ggplot(data = filter(analysis_bing, !is.na(bing))) +
  geom_histogram(aes(bing), stat = "count") +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  facet_grid(cols = vars(session)) +
  labs(
    title = "Sentiment of the Congressional Energy & Commerce Subcommittee",
    subtitle = "Analysis of Sessions 112, 115, and 117",
    y = "Number of Observations",
    x = "Feeling Expressed"
  ) +
  theme_economist()

nrc_plot

bing_plot

ggsave(filename = "bing_plot.png", plot = bing_plot)

ggsave(filename = "nrc_plot.png", plot = nrc_plot)

