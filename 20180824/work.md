Riddler 2018-08-24: How Many Hoops Will Kids Jump Through To Play Rock, Paper, Scissors?
================

**[Link to Riddler](https://fivethirtyeight.com/features/how-many-hoops-will-kids-jump-through-to-play-rock-paper-scissors/)**

------------------------------------------------------------------------

Riddler Express
---------------

From 538:

> Xavier and Yolanda are on a road trip when they notice a hiking trailhead on the side of the highway. They decide to check it out and pull into the parking lot. The sign at the trailhead describes the trail like this: “Flat, with excellent scenery on both sides. The trail dead-ends about 3 miles from this trailhead.”

> Xavier and Yolanda agree to go hiking on the trail. But Yolanda hikes at a faster pace than Xavier, and each prefers to hike at his or her own steady pace instead of adjusting it to stay with the other person. Neither Xavier nor Yolanda knows exactly how fast either of them walk; they only know that Yolanda is faster. To further complicate matters, both of their cellphones are dead, they have no other timepieces of any kind, and they have no idea, as they stand at the trailhead, whether there are any distance markers or memorable landmarks along the trail.

> How can Xavier and Yolanda plan their hike such that (1) they start hiking down the trail at the same time, (2) they each hike at their own steady pace the entire time, (3) they only hike up and down the trail (i.e., no hiking in the parking lot or on the highway), and (4) they arrive back at the trailhead at the same time?

**Solution:** Xavier (slower hiker) will travel the six miles up and down the trail normally. Yolanda will have to work her hike around Xavier's pace. Her plan should be to hike the entire trail (all 6 miles) first, then once she is done with that she would start the hike again at the trailhead, heading towards Xavier because he is still hiking. Once she bumps into Xavier on her hike, she would do a 180 degree turn and hike back to the trailhead. She would keep repeating this process, of going back and forth between the start and Xavier, until her and Xavier are both at the start at the same time.

**Note:** This solution is under the assumption that neither hiker can stop to take a break. Otherwise, the plan would be for both hikers to hike normally, and Yolanda would just wait for Xavier at the trailhead after her hike finishes.

------------------------------------------------------------------------

Riddler
-------

------------------------------------------------------------------------

From 538:

> Let’s call this game rock-paper-scissors-hop. Here is an idealized list of its rules:

-   Kids stand at either end of N hoops.
-   At the start of the game, one kid from each end starts hopping at a speed of one hoop per second until they run into each other, either in adjacent hoops or in the same hoop.
-   At that point, they play rock-paper-scissors at a rate of one game per second until one of the kids wins.
-   The loser goes back to their end of the hoops, a new kid immediately steps up at that end, and the winner and the new player hop until they run into each other.
-   This process continues until someone reaches the opposing end. That player’s team wins!

> You’ve just been hired as the gym teacher at Riddler Elementary. You’re having a bad day, and you want to make sure the kids stay occupied for the entire class. If you put down eight hoops, how long on average will the game last? How many hoops should you put down if you want the game to last for the entire 30-minute period, on average?

**Solution:** If we have 8 hoops, we would expect the game to last around **80 seconds.** If we wanted to create a game that would last 30 minutes on average, we would want to start with a game that has **42 hoops** (see relationship between starting hoops and game length in attached graph at bottom).

**Work:**

``` r
# Function to simulate this game and return seconds elapsed for each game
hoop_game <- function(n_hoops = 8) {
  
  # Initialize starting position, assuming A wins first game.
  # Whether A or B wins, it doesn't matter at this stage.
  hoops_to_jump <- floor((n_hoops - 1)/2)
  a_pos <- 1 + hoops_to_jump
  b_pos <- n_hoops
  
  seconds_elapsed <- hoops_to_jump
  
  # Stop game when one player is on the opposite side.
  # Stop when hoops_to_jump == 0 because that means that a player is about to 
  # reach the other side and end the game
  while(a_pos != n_hoops & b_pos != 1) {
    
    # Hoops/seconds needed to converge towards the middle
    hoops_to_jump <- max(floor((b_pos - a_pos)/2), 1)
      
    # Randomly select who will win RPS engagement
    game_winner <- sample(c('a', 'b'), size = 1)
    
    if(game_winner == 'a') {
      a_pos <- a_pos + hoops_to_jump
      b_pos <- n_hoops
    } else {
      a_pos <- 1
      b_pos <- b_pos - hoops_to_jump
    }
      
    # Expected time for each rock paper scissor engagement 
    # = 1 / P(kids throw different objects) 
    # = 1 / (2/3)
    # = 1.5 seconds
    seconds_elapsed <- seconds_elapsed + hoops_to_jump + 1.5
    
  }
  
  return(seconds_elapsed)

}
```

``` r
# Create 1k simulations for the hoop game for different starting n_hoop configurations:

set.seed(123)
result_list <- list()

for(hoops in 1:50) {
  result_list[[hoops]] <- replicate(1e3, hoop_game(hoops)) 
}
```

``` r
# Average game length for 8 starting hoops
summary(result_list[[8]])
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   11.50   25.50   55.75   80.18  111.50  588.50

``` r
# Plot average length of games as a function of starting hoops
hoops_df <- 
  data.frame(n_hoops = 1:50) %>%
  mutate(avg_game_length = sapply(result_list, mean))

hoops_df %>%
  ggplot(aes(x = n_hoops, y = avg_game_length/60)) +
  geom_line() +
  geom_smooth(size = .25, color = 'red') +
  geom_hline(yintercept = 30, linetype = 'dashed') +
  theme_fivethirtyeight() +
  theme(axis.title = element_text()) +
  labs(y = 'Average Game Length (Minutes)',
       x = 'Number of Starting Hoops',
       title = 'Average Game Length by Number of Starting Hoops')
```

![](work_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-4-1.png)
