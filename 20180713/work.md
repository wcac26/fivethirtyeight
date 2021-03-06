Riddler 2018-07-13 : Can you slice this in half?
================

Overview
--------

On my free time, I like to solve riddles. This is my attempt at [538's Riddler](https://fivethirtyeight.com/features/can-you-slice-this-in-half/)

Riddler Express
---------------

From 538:

> This year’s World Cup has been chock full of exciting penalty shootouts. Historically, about 75 percent of soccer penalty kicks are successful. Given that number, what are the chances that a shootout goes past its fifth kick for each team and into the even more exciting sudden-death portion of the penalty-kick period?

**Solution:** To find the answer, all we need to do is calculate the probability that both teams score the same number of goals out of 5. To do this, we use the binomial distribution. The chances that this will happen is **29.02%**.

The code used for this can fit in one line:

``` r
sum(dbinom(0:5, 5, 0.75)^2)
```

Riddler
-------

From 538:

> From Adam Wagner, a puzzling bisection. Adam writes that this novel challenge is “my all-time favorite puzzle, and I like the exclusivity of knowing that nobody I tell it to will ever have encountered it before. But as Chris McCandless said, ‘Happiness \[is\] only real when shared,’ so I’ve decided at long last to let it leave the nest and be enjoyed by all of Riddler Nation.”

> Say you have an “L” shape formed by two rectangles touching each other. These two rectangles could have any dimensions and they don’t have to be equal to each other in any way. (A few examples are shown below.)

> Using only a straightedge and a pencil (no rulers, protractors or compasses), how can you draw a single straight line that cuts the L into two halves of exactly equal area, no matter what the dimensions of the L are? You can draw as many lines as you want to get to the solution, but the bisector itself can only be one single straight line.

**Solution:** The first thing I noticed about this puzzle is that you can create the figure by removing one rectangle from the upper right corner of a square. Once you have the two rectangles, I focused on drawing diagonals, and pretty quickly found out that you can bisect the figure by connecting the midpoints of each of the two rectangles. The solution was mostly done through intuition and trial and error. I created a visualization to see what a bunch of these bisections look like.

``` r
# Function to create squares
create_squares <- function(n_squares){
  
  create_single_square <- function(input_df){
    
    # Create line segments that will form the outside border of the rectangles
    bind_rows(list(x = c(0, 0), y = c(0, 1)) %>% as.data.frame,
              list(x = c(0, input_df$x), y = c(1, 1)) %>% as.data.frame,
              list(x = c(input_df$x, input_df$x), y = c(1, input_df$y)) %>% as.data.frame,
              list(x = c(input_df$x, 1), y = c(input_df$y, input_df$y)) %>% as.data.frame,
              list(x = c(1, 1), y = c(input_df$y, 0)) %>% as.data.frame,
              list(x = c(1, 0), y = c(0, 0)) %>% as.data.frame) %>%
      mutate(line_type = 'a') ->
      outline_points
    
    # Create diagonals of the two rectangles
    bind_rows(list(x = c(0, input_df$x), y = c(1, input_df$y)) %>% as.data.frame,
              list(x = c(0, input_df$x), y = c(input_df$y, 1)) %>% as.data.frame,
              list(x = c(0, 1), y = c(0, input_df$y)) %>% as.data.frame,
              list(x = c(0, 1), y = c(input_df$y, 0)) %>% as.data.frame) %>%
      mutate(line_type = 'b') ->
      diag_points
    
    # Get equation of line to pass through the rectangles' centers
    slope <- (input_df$y/2 - (0.5 + input_df$y/2))/(0.5 - input_df$x/2)
    b <- input_df$y/2 - slope * 0.5
    
    list(x = c(max((1-b)/slope, 0), -b/slope), y = c(min(1, b) , 0)) %>% as.data.frame %>%
      mutate(line_type = 'a') ->
      bisect_points
    
    bind_rows(outline_points %>% mutate(color = 'black'), 
              diag_points %>% mutate(color = 'gray'), 
              bisect_points %>% mutate(color = 'green')) %>%
      mutate(line_seg = rep(1:(nrow(.)/2), each = 2)) %>%
      return
    
  }
  
  # Loop through and create n_squares figures
  all_plots <- list()
  for(i in 1:n_squares){
    df <- data.frame(x = runif(1, 0, 1),
                     y = runif(1, 0, 1))
    
    all_plots[[i]] <- 
      create_single_square(df) %>% 
      mutate(facet = i,
             order_score = 1 - ((1 - df$x) * (1 - df$y)))
  }
  
  all_plots %<>% bind_rows
  
  all_plots %>%
    mutate(facet = factor(facet, levels = all_plots %>% arrange(order_score) %>% pull(facet) %>% unique)) %>% return
}
```

``` r
# Generate figures and plot
squares <- create_squares(64)

squares %>%
  ggplot(aes(x = x, y = y, group = line_seg)) +
  geom_line(aes(linetype = line_type, color = color)) +
  scale_color_manual(values = c('black', 'grey', 'green')) + 
  theme_bw() + 
  theme(panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  xlim(0, 1) +
  ylim(0, 1) + 
  guides(linetype = FALSE, color = FALSE) +
  facet_wrap(~facet)+ 
  theme(strip.background = element_blank(), strip.text.x = element_blank())
```

![](work_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-2-1.png)
