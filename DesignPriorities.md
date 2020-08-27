This is just the first version of our Design Priorities document, where we can put feature ideas, bugs, design tweaks, etc. for prioritization and collaborative feedback.

Design Priorities
----------

Short-term:
* Come up with actual uses cases and test data (keep remittances but find 2-3 more uses cases and at least one that's real). Will to get bikeshare station data to use here. Geology data.
* Recenter existing test cases to do side by side analysis, small multiples
* Add ability for center to have or not have data (checkbox)
* Documentation: more of it
* Code refactoring: functionizing, making generic (data variable/value, get rid of center column and hardcode), etc.
* Come up with automated function for distance decay, 2-d clustering
* variable selection from csv

Mid-term:
* Accepting JSON/GeoJSON/Shapefile instead of just CSV input
* Adding centroids to polygons in input data (use the NH case?)
* Add value trim step ("Below what value do you not care about observations?" Subset the data and plot)
* Accept that the center might also carry value (null or not?)
  Add a question: should the center point have null value or a numeric value?
* TIN/Delauney Triangles and/or raster surface
* Control contours at all (move out of GGplot into sp?) Do it all at the end? Smoothing functions with existing approach?
* SVG export
* flannery's exponent support
* custom value interpretation

Long-term:
* project polygons 
* proj4 integration (talk to hyperproj folks)
* Add support for doing things with two values/centers at the same time (meh, maybe not?)
* better: add support for changing center point 
* Port to JavaScript?
* Add Universal Location Finder 3000 function (first look for lat/long, then look for polygons you can centroid, then look for location-esque string fields that you can geocode, in the order Census Block/Tract, ZIP, Town/City/etc., County, State, Country?) 
