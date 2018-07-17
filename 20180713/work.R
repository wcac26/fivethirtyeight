####################################################################################################
# Riddler Work for 2018-07-13 puzzles:
# https://fivethirtyeight.com/features/can-you-slice-this-in-half/

####################################################################################################
# Load Useful Libraries
library(dplyr)
library(ggplot2)
library(ggthemes)

####################################################################################################
# Riddler Express
# This year’s World Cup has been chock full of exciting penalty shootouts. 
# Historically, about 75 percent of soccer penalty kicks are successful. 
# Given that number, what are the chances that a shootout goes past its fifth kick 
# for each team and into the even more exciting sudden-death portion of the penalty-kick period?

# Answer: Prob (both teams score the same number of kicks out of 5)
sum(dbinom(0:5, 5, 0.75)^2)

####################################################################################################
# Riddler

create_square <- function(input_df){
  
  bind_rows(list(x = c(0, 0), y = c(0, 1), line_seg = c(1, 1)) %>% as.data.frame,
            list(x = c(0, input_df$x), y = c(1, 1), line_seg = c(2, 2)) %>% as.data.frame,
            list(x = c(input_df$x, input_df$x), y = c(1, input_df$y), line_seg = c(3, 3)) %>% as.data.frame,
            list(x = c(input_df$x, 1), y = c(input_df$y, input_df$y), line_seg = c(4, 4)) %>% as.data.frame,
            list(x = c(1, 1), y = c(input_df$y, 0), line_seg = c(5, 5)) %>% as.data.frame,
            list(x = c(1, 0), y = c(0, 0), line_seg = c(6, 6)) %>% as.data.frame) %>%
    mutate(line_type = 'a') ->
    outline_points
  
  bind_rows(list(x = c(0, input_df$x), y = c(1, input_df$y), line_seg = c(7, 7)) %>% as.data.frame,
            list(x = c(0, input_df$x), y = c(input_df$y, 1), line_seg = c(8, 8)) %>% as.data.frame,
            list(x = c(0, 1), y = c(0, input_df$y), line_seg = c(9, 9)) %>% as.data.frame,
            list(x = c(0, 1), y = c(input_df$y, 0), line_seg = c(10, 10)) %>% as.data.frame) %>%
    mutate(line_type = 'b') ->
    diag_points
  
  slope <- (input_df$y/2 - (0.5 + input_df$y/2))/(0.5 - input_df$x/2)
  b <- input_df$y/2 - slope * 0.5
  
  list(x = c((1-b)/slope, -b/slope), y = c(1, 0), line_seg = c(11, 11)) %>% as.data.frame %>%
    mutate(line_type = 'a') ->
    bisect_points
  
  area_left <- (bisect_points$x[1] + bisect_points$x[2])/2
  area_right <- (1 - input_df$x) * input_df$y + (input_df$x - bisect_points$x[1] + input_df$x - bisect_points$x[2])/2
  
  cat(area_left, '|', area_right)
  
  bind_rows(outline_points, diag_points, bisect_points) %>%
  return
  
}

all_plots <- list()
for(i in 1:25){
  
  df <- data.frame(x = runif(1, 0, 1),
                   y = runif(1, 0, 1))
  
  all_plots[[i]] <- create_square(df) %>% mutate(facet = i)
  
}

z <- bind_rows(all_plots)

z %>%
  ggplot(aes(x = x, y = y, group = line_seg)) +
  geom_point() +
  geom_line(aes(linetype = line_type)) +
  theme_bw() +
  xlim(0, 1) +
  ylim(0, 1) + 
  guides(linetype = FALSE) +
  facet_wrap(~facet)

