library(dplyr)
library(tidyr)
library(ggplot2)
library(unvotes)
library(forcats)

un_votes_ru <- un_votes |>
  filter(between(rcid, 3500, 8900)) |>
  mutate(ru = country == "Russia") |>
  arrange(rcid, desc(ru)) |>
  group_by(rcid) |>
  mutate(ru_eq = if_else(vote == first(vote), TRUE, FALSE)) |>
  inner_join(un_roll_call_issues) |>
  group_by(country, country_code, issue) |>
  summarize(vote = mean(ru_eq)) |>
  arrange(desc(vote)) |>
  filter(country != "Russia") |>
  group_by(issue) |>
  mutate(mm = if_else(2 < abs((vote - mean(vote))/sd(vote)) | vote == min(vote), TRUE, FALSE),
         rank = if_else(percent_rank(vote) > 0.98 | percent_rank(vote) < 0.02, TRUE, FALSE))

un_votes_ru |>
  ggplot(aes(x = fct_reorder(issue, vote, mean), y = vote)) +
  geom_violin(alpha = 0.5) +
  ggrepel::geom_text_repel(data = filter(un_votes_ru, rank == TRUE), aes(label = country)) +
  theme_minimal() +
  labs(x = NULL, y = "% Vote similarity with Russia (UNGA)",
       caption = "Source: UN Votes R package (Voeten 2013)")