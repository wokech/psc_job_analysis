## ggwordcloud: a word cloud geom for ggplot2
# https://cran.r-project.org/web/packages/ggwordcloud/vignettes/ggwordcloud.html

# 1) Package, library, and data installation

install.packages("ggwordcloud")
library(ggwordcloud)
data("love_words_small")
data("love_words")

# 2) Word cloud - geom_text_wordcloud()

# The geom_text_wordcloud geom constructs a word cloud from a 
# list of words given by the label aesthetic:

set.seed(42)
ggplot(love_words_small, aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal()

# Due to placement randomness - different seed, different result

set.seed(43)
ggplot(love_words_small, aes(label = word)) +
  geom_text_wordcloud() +
  theme_minimal()

# 3) Word cloud and text size

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud() +
  theme_minimal()

# The words are scaled according to the 
# value of the size aesthetic, the number of speakers here. 
# There are several classical choices for the scaling.

# In order to obtain a true proportionality (and a better font size control), 
# one can use the scale_size_area() scale.

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud() +
  scale_size_area(max_size = 20) + # font size 
  theme_minimal()


# It turns out that both wordcloud and wordcloud2 default to a linear scaling 
# between the value and the font size. This can be obtained 
# with the scale_radius() scale.

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud() +
  scale_radius(range = c(0, 20), limits = c(0, NA)) +
  theme_minimal()

# 4) Word cloud and text area

# In ggwordcloud2, there is an option, area_corr to scale the font of each 
# label so that the text area is a function of the raw size aesthetic 
# when used in combination with scale_size_area  

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud(area_corr = TRUE) +
  scale_size_area(max_size = 24) +
  theme_minimal()

# One can equivalently use the geom_text_wordcloud_area geom

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal()

# The area is proportional to the raw size aesthetic raised to the power 1/.7
# in order to match the human area perception. To obtain an area proportional 
# to the raw size aesthetic, it suffices to set the area_corr_power to 1

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(area_corr_power = 1) +
  scale_size_area(max_size = 24) +
  theme_minimal()

# 5) Word cloud with too many words

#Example
set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 40) +
  theme_minimal()

# It is up to the user to avoid this issue by either removing some words or 
# changing the size scale. One can also chose to remove those words 
# using the rm_outside option.

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +
  scale_size_area(max_size = 40) +
  theme_minimal()

# 6) Word cloud and rotation

# The words can be rotated by setting the angle aesthetic. 
# For instance, one can use a rotation of 90 degrees for a 
# random subset of 40 % of the words:

library(dplyr, quietly = TRUE)

love_words_small <- love_words_small %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))


set.seed(42)
ggplot(love_words_small, aes(
  label = word, size = speakers,
  angle = angle
)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal()


# ggwordcloud is not restricted to rotation of 90 degrees:
  
love_words_small <- love_words_small %>%
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))

set.seed(42)
ggplot(love_words_small, aes(
  label = word, size = speakers,
  angle = angle
)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal()


# 7) Word cloud and eccentricity

# The ggwordcloud algorithm moves the text around a spiral until it finds a 
# free space for it. This spiral has by default a vertical 
# eccentricity of .65, so that the spiral is 1/.65 wider than taller.

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal()

# This can be changed using the eccentricity parameter:

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(eccentricity = 1) +
  scale_size_area(max_size = 24) +
  theme_minimal()

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(eccentricity = .35) +
  scale_size_area(max_size = 24) +
  theme_minimal()

# 8) Word cloud and shape

for (shape in c(
  "circle", "cardioid", "diamond",
  "square", "triangle-forward", "triangle-upright",
  "pentagon", "star"
)) {
  set.seed(42)
  print(ggplot(love_words_small, aes(label = word, size = speakers)) +
          geom_text_wordcloud_area(shape = shape) +
          scale_size_area(max_size = 24) +
          theme_minimal() + ggtitle(shape))
}

#9 ) Word cloud and color

# A color can be assign to each word using 
# the color aesthetic. For instance, one can 
# assign a random factor to each word

set.seed(42)
ggplot(
  love_words_small,
  aes(
    label = word, size = speakers,
    color = factor(sample.int(10, nrow(love_words_small), replace = TRUE)),
    angle = angle
  )
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal()

# One can also map the color to a value, for instance the number 
# of speakers, and chose the colormap with a scale_color_* scale

set.seed(42)
ggplot(
  love_words_small,
  aes(
    label = word, size = speakers,
    color = speakers, angle = angle
  )
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 24) +
  theme_minimal() +
  scale_color_gradient(low = "darkred", high = "orange")

# 10) Word cloud and mask

set.seed(42)
ggplot(love_words_small, aes(label = word, size = speakers)) +
  geom_text_wordcloud_area(
    mask = png::readPNG(system.file("extdata/hearth.png",
                                    package = "ggwordcloud", mustWork = TRUE
    )),
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 18) +
  theme_minimal()


# 11) Word cloud with almost everything

# We are now ready to make a lovely word cloud

love_words <- love_words %>%
  mutate(angle = 45 * sample(-2:2, n(), replace = TRUE, prob = c(1, 1, 4, 1, 1)))

set.seed(42)
ggplot(
  love_words,
  aes(
    label = word, size = speakers,
    color = speakers, angle = angle
  )
) +
  geom_text_wordcloud_area(
    mask = png::readPNG(system.file("extdata/hearth.png",
                                    package = "ggwordcloud", mustWork = TRUE
    )),
    rm_outside = TRUE
  ) +
  scale_size_area(max_size = 16) +
  theme_minimal() +
  scale_color_gradient(low = "darkred", high = "red")

# 12) Advanced feature

# geom_text_wordcloud is compatible with the facet system of ggplot2

library(dplyr, quietly = TRUE, warn.conflicts = FALSE)
library(tidyr, quietly = TRUE)
love_words_small_l <- love_words_small %>%
  gather(key = "type", value = "speakers", -lang, -word, -angle) %>%
  arrange(desc(speakers))

set.seed(42)
ggplot(
  love_words_small_l,
  aes(label = word, size = speakers)
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  theme_minimal() +
  facet_wrap(~type)

# One can also specify an original position for each label that 
# what will be used as the starting point of the spiral algorithm 
# for this label.

set.seed(42)
ggplot(
  love_words_small_l,
  aes(
    label = word, size = speakers,
    x = type, color = type
  )
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 20) +
  scale_x_discrete(breaks = NULL) +
  theme_minimal()

# Finally, there is a angle_group option that can be used to restrict 
# the words to appear only in a angular sector depending on their angle_group.

love_words_small_l <- love_words_small_l %>%
  group_by(type) %>%
  mutate(prop = speakers / sum(speakers)) %>%
  group_by(lang, word) %>%
  mutate(propdelta = (prop - mean(prop)) / sqrt(mean(prop)))


set.seed(42)
ggplot(
  love_words_small_l,
  aes(
    label = word, size = abs(propdelta),
    color = propdelta < 0, angle_group = propdelta < 0
  )
) +
  geom_text_wordcloud_area() +
  scale_size_area(max_size = 8) +
  theme_minimal() +
  facet_wrap(~type)

# 13) ggwordcloud as an approximate replacement for wordcloud and wordcloud2

ggwordcloud(love_words$word, love_words$speakers)

ggwordcloud2(love_words[, c("word", "speakers")], size = 1.5)
