# uberEats_shinyApp
shiny app for visualizing menu items and prices across major cities in CA + a menu pricing module
First we analysed which cities have the most amount of active listings
Then we look at the most popular submenus
Then we look to use the most heavily represented city as the backbone of our analyis to draw relative conclusions for menu prices in all other cities
a 2 sample t-tests of average prices of top submenus are done to ascertain whether prices on average are the same or statistically different between LA and the
other cities. If different a simple transformation is done to get from LA prices to the prices of that city for that submenu. if not different then no price
adjustment is made to the distribution of prices in LA for that submenu in modeling the distribution of prices for that city.

ratings also matter. do higher rated restaurants tend to have higher prices on average? data suggests this might be the case and hence the average rating should
be factored in pricing.

do special keywords in dish names or even dish description tend to be associated with higher prices...
This might be the case but inconclusive from my research and corpus used for the analysis. Further work using richer data set and NLP techniques might result
in more conclusive evidence.

given a city, a restaurant rating and sub-menu items can we subject a representative price range for each sub-menu item that will inform a restaurant operator as to where to price items under those submenus? answer is yes and we demonstrate this in the app.
