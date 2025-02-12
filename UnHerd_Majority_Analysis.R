---
title: "UnHerd Majority "
author: "Ethan Pinto"
date: "`r Sys.Date()`"
output: pdf_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r install packages}
##install.packages("tidyverse")
##install.packages("tidytext")
##install.packages("textdata")
##install.packages("rmarkdown")
##install.packages("ggplot2")
##install.packages("knitr")
```



```{r}
library(tidyverse)
library(tidytext)
library(textdata)
library(knitr)
library(rmarkdown)
library(ggplot2)
```

```{r}
# Load the data
data <- read_csv("Book1.csv")
```
Summary of Comments:

```{r}
sentiment_distribution <- data %>%
  count(Code) %>%
  mutate(percentage = n / sum(n) * 100)
print(sentiment_distribution)

```

```{r}
ggplot(sentiment_distribution, aes(x = Code, y = percentage, fill = Code)) +
  geom_bar(stat = "identity") +
  labs(title = "Sentiment Distribution", x = "Sentiment Category", y = "Percentage") +
  theme_minimal()

```

Content Eliciting Each Type of Comment

```{r}
ad_summary <- data %>%
  group_by(`Ad Name`, Code) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count)) %>%
  mutate(percentage = count / total * 100) %>%
  arrange(desc(percentage))

print(ad_summary)

```

```{r}
ggplot(ad_summary, aes(x = reorder(`Ad Name`, -percentage), y = percentage, fill = Code)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Content Eliciting Each Type of Comment", x = "Ad Name", y = "Percentage") +
  theme_minimal() +
  coord_flip()

```

Differences in Engagement and Comments Between Audiences

```{r}
audience_summary <- data %>%
  group_by(Audience, Code) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count)) %>%
  mutate(percentage = count / total * 100) %>%
  arrange(Audience, desc(percentage))

print(audience_summary)

```

```{r}
ggplot(audience_summary, aes(x = Audience, y = percentage, fill = Code)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Differences in Engagement and Comments Between Audiences", x = "Audience", y = "Percentage") +
  theme_minimal()
```

Policy Areas Resonance

```{r}
policy_keywords <- list(
  Economy = c("economy", "economic", "finance", "financial", "market"),
  Jobs = c("job", "jobs", "employment", "unemployment", "work"),
  Taxes = c("tax", "taxes", "taxation", "IRS", "income tax"),
  Healthcare = c("healthcare", "health care", "insurance", "medicare", "medicaid")
)

tag_policy_area <- function(comment, keywords) {
  policies <- unlist(lapply(names(keywords), function(policy) {
    if (any(sapply(keywords[[policy]], function(keyword) {
      grepl(keyword, comment, ignore.case = TRUE)
    }))) {
      return(policy)
    }
  }))
  if (length(policies) == 0) {
    return("Other")
  } else {
    return(paste(policies, collapse = ", "))
  }
}

data <- data %>%
  mutate(PolicyArea = sapply(`Comment Message`, tag_policy_area, keywords = policy_keywords))
head(data)
```

```{r}
data <- data %>%
  mutate(PolicyArea = strsplit(PolicyArea, ", ")) %>%
  unnest(PolicyArea)

head(data)
```

```{r}
sentiment_distribution <- data %>%
  count(Code) %>%
  mutate(percentage = n / sum(n) * 100)

ad_summary <- data %>%
  group_by(`Ad Name`, Code) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count)) %>%
  mutate(percentage = count / total * 100) %>%
  arrange(desc(percentage))

audience_summary <- data %>%
  group_by(Audience, Code) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count)) %>%
  mutate(percentage = count / total * 100) %>%
  arrange(Audience, desc(percentage))

```

```{r}
policy_resonance <- data %>%
  filter(PolicyArea != "Other") %>%
  group_by(Audience, PolicyArea) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count)) %>%
  mutate(percentage = count / total * 100) %>%
  arrange(Audience, desc(percentage))

top_5_policy_resonance <- policy_resonance %>%
  group_by(Audience) %>%
  top_n(5, wt = percentage) %>%
  ungroup() %>%
  arrange(Audience, desc(percentage))

print(top_5_policy_resonance)
```

```{r}
ggplot(top_5_policy_resonance, aes(x = reorder(PolicyArea, -percentage), y = percentage, fill = Audience)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Top 5 Policy Areas Resonance by Audience", x = "Policy Area", y = "Percentage") +
  theme_minimal() +
  coord_flip()
```





