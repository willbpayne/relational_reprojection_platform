This is just the first version of our Design Priorities document, where we can put feature ideas, bugs, design tweaks, etc. for prioritization and collaborative feedback.

Design Priorities
----------

Short-term:
* Recenter existing test cases to do side by side analysis, small multiples
* Add ability for center to have or not have data (checkbox)
* Documentation: more of it
* Code refactoring: functionizing, making generic (data variable/value, get rid of center column and hardcode), etc.
* Design a user interface and build it out with a JavaScript wrapper on our code
* Figure out Jenks for distance (more exploded than on a line due to circle)
  ? hours best one-dimensional clustering algorithm in R?

Mid-term:
* Accepting JSON/GeoJSON/Shapefile instead of just CSV input
* Adding centroids to polygons in input data (use the NH case?)
* Add value trim step ("Below what value do you not care about observations?" Subset the data and plot)
* Accept that the center might also carry value (null or not?)
  Add a question: should the center point have null value or a numeric value?
* TIN/Delauney Triangles
* Control contours at all (move out of GGplot into sp?) Do it all at the end? Smoothing functions with existing approach?
  Eve to look into more seriously

Long-term:
* Add support for doing things with two values/centers at the same time
* Port to JavaScript?
* Add Universal Location Finder 3000 function (first look for lat/long, then look for polygons you can centroid, then look for location-esque string fields that you can geocode, in the order Census Block/Tract, ZIP, Town/City/etc., County, State, Country?) 
