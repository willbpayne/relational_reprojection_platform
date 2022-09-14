This is just the first version of our Design Priorities document, where we can put feature ideas, bugs, design tweaks, etc. for prioritization and collaborative feedback.

Design Priorities
----------

Short-term:
* Fix custom value interpolation
* Find at least one new transformation we like
* Categorical colors option (come up with list of factor variables and allow this qualitative color scheme to override color scale)
* Hosting this bad boy (Shiny Server)
* Floating abstract to Luke's AAG session (Eve)
* Email Taylor about SFR data, show what we have (Will)
* Recenter existing test cases to do side by side analysis, small multiples
* Add ability for center to be derived from data for non-relational datasets, rather than known a priori
  * Upload workflow needs a step where user chooses a center point (get past "isCTR" column)
  * Checkbox for no center 
  * If there is a center, serve the user a lookup input box to select the center from a list of all rows in the dataframe (include mean lat-long as an option for non-relational data; WARNING comes up)
  * Add a column called "isCTR" to the dataframe with a value of 1 for the row we picked
  * If there's no center, perform weighted mean center analysis:
  ** for each point feature, multiplying the x and y coordinate by the weight for that feature and summing all for both x and y individually, and then dividing this by the sum of all the weights
  * Or other metric
* Documentation: more of it
* Come up with automated function for distance decay, 2-d clustering

Mid-term:
* Accepting JSON/GeoJSON/Shapefile instead of just CSV input
* Adding centroids to polygons in input data (use the NH case?)
* Add value trim step ("Below what value do you not care about observations?" Subset the data and plot)
* TIN/Delauney Triangles and/or raster surface
* Control contours at all (move out of GGplot into sp?) Do it all at the end? Smoothing functions with existing approach?
* flannery's exponent support
* custom value interpretation

Long-term:
* project polygons 
* proj4 integration (talk to hyperproj folks)
* Add support for doing things with two values/centers at the same time (meh, maybe not?)
* better: add support for changing center point 
* Port to JavaScript?
* Add Universal Location Finder 3000 function (first look for lat/long, then look for polygons you can centroid, then look for location-esque string fields that you can geocode, in the order Census Block/Tract, ZIP, Town/City/etc., County, State, Country?) 

DONE:
* Fix custom distance to be data driven instead of just the India remittance scale
* SVG export
* Come up with actual uses cases and test data (keep remittances but find 2-3 more uses cases and at least one that's real). Will to get bikeshare station data to use here. Geology data.
* Get clean datasets for Austin and Philly relational trips from weighted mean center of bikeshare network
* Scale of yellow circles
* Code refactoring: functionizing, making generic (data variable/value, get rid of center column and hardcode), etc.
* Add ability for center to have or not have data (checkbox)
* variable selection from csv
* Accept that the center might also carry value (null or not?) Add a question: should the center point have null value or a numeric value?

