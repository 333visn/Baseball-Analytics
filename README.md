Note: A few projects do not have the datasets attatched to it because Frontier League data is too big for Github, so it was not included

1. Logan Webb: Release Speed Tracking (Spring 2022)
- Tools: R, Shiny, baseballr, ggplot2
- This Shiny application provides a streamlined interface to monitor Logan Webb’s release velocity throughout the 2022 season.
- Tracks how velocity (mph) fluctuates across different innings, counts, and batter handedness.
- Allows users to filter by specific scenarios (e.g., "fastest pitches in the 7th inning with 2 outs") to visualize how Webb maintains his heat as the game progresses.

2. Casey Schmitt: Rookie Debut Analysis (Summer 2023)
- Tools: R, Shiny, baseballr, ggplot2
- Built during Casey Schmitt’s intial MLB call up in May 2023. This app uses the baseballr package to scrape and visualize his initial performance metrics.
- Includes situational performance bars (Runners On vs. Bases Empty) and a Swing & Miss rate analysis by pitch type to identify which offerings were most effective during his debut stretch.

3. Seattle Mariners: 2023 Starting Rotation Study (Spring 2024)
- Tools: R, Shiny, baseballr, tidyverse
- An interactive evaluation of the Mariners' 2023 starting rotation, inspired by the debate over whether they were the best unit in baseball.
- Utilizes the custom geom_strikezone for pitch-grid mapping and features comparative analysis for wOBA and WAR (Wins Above Replacement).
- A dedicated module evaluating the "Cal Raleigh effect," looking at catcher framing and defensive contributions across the league.
- Interactive scatter plots comparing Mariners pitchers to the rest of the MLB in terms of Runs Prevented Above Average and total WAR.

4. Fantasy Baseball Pitchers: Outlier Analysis (Summer 2024)
- Tools: R, rvest, tidyverse, R Markdown
- A data engineering project focused on identifying consistent pitching performance by scraping real-time game logs and filtering for statistical outliers.
- Built custom scrapers for CBS Sports and Fox Sports to aggregate season stats and individual game logs.
- Developed robust logic to handle player name mismatches and dynamic URL structures for automated data collection.
- Applied Z-score analysis to remove extreme variance games, recalculating "Adjusted" Fantasy Points (FPPG) and Strikeout-to-Walk ratios (KW.adj).

5. 2023 Postseason: Bayesian Batting Model (Spring 2025)
- Tools: R, brms (Stan), lme4, ggplot2
- An advanced statistical study using Bayesian Hierarchical Modeling to analyze 2023 postseason play-by-play data.
- A Bernoulli GLMM (isHit ~ 1 + (1|game_id) + (1|team_id: player_id)) designed to account for the high variance of small postseason sample sizes.
- Used brms to derive posterior probabilities for batting success, allowing for a more stable estimation of "True Talent" compared to raw batting averages.
- Features a Top and Bottom Player Batting Probabilities plot with 95% credible intervals, providing a clear visual of which players significantly over/underperformed during the October run.

6. Synthetic Pitcher-Batter Matchup Predictor (Summer 2025)
- Tools: Python, Scikit-Learn, Pandas, NumPy
- A predictive modeling project using Frontier League data from the 2025 season to simulate and predict the outcomes of hypothetical at-bats.
- Utilizes a RandomForestClassifier to determine the result of an at-bat (e.g., Strikeout, Single, Walk) based on a synthetic input of pitcher and batter historical profiles.
- Features a realistic game engine that accounts for strike zone boundaries, pitcher location tendencies, and batter swing-zone probabilities.
- Implements stadium-based hit classification (calculating fence distance vs. launch angle) and estimates xwOBA and xDamage for all simulated outcomes.

7. Frontier League xwOBA Model (Fall 2025)
- Tools: R, Caret, Tidyverse
- A K-Nearest Neighbors (KNN) model built to calculate Expected Weighted On-Base Average (xwOBA) specifically for the Frontier League environment.
- Uses launch angle and exit velocity to determine the probability of hit outcomes, mapping them to weighted values.
- Provides a more accurate evaluation of hitter performance by removing defense and park factors from the equation.

8. Frontier League Stuff+ Model (Fall 2025)
- Tools: Python, Jupyter Notebook, Pandas, Scikit-Learn
- Inspired by professional pitching metrics, this project develops a custom "Stuff+" model for the Frontier League.
- Quantifies the physical characteristics of a pitch (velocity, vertical/horizontal break, and release point) independent of the outcome or location.
- To identify high-upside arms by evaluating pitch quality relative to league-average movement profiles for each pitch type.

9. Swing Decision Model (Fall 2025)
- Tools: Python, PyTorch (Neural Networks)
- An exploration into plate discipline using deep learning.
- To build a neural network that evaluates "Swing vs. Take" decisions based on pitch location, count, and pitcher tendencies.
