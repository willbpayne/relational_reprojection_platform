// 0. Initialize website with header text and explanation of the process (with image examples) 
// 1. Prompt user to choose a canvas size (default?)
// 2. Choose a home point (where distances and bearings are calculated from)
// 3. Choose a dataset (CSV? GeoJSON?), with an attribute that relates back to the home point somehow (i.e. is relational; refugee flows, investment, similarity, cost/time distance, etc. Raw attributes like population, wealth, etc. aren’t well-suited for this; just do a cartogram or choropleth map)
// 		a. Have user specify lat, long, and attribute columns in the data file, and home point row
// 		b. IF the home point has a 0 or NA value,
// 4. Zeroth cut:
// 		a. Plot X/Y coordinates of lat/long of each point on clunky-ass Web Mercator canvas (specified as bounding box of data plus a small margin)
// 		b. Have user click to advance to “relative spatialization”
// 5. First cut: Calculate first cut of redone spatialization (with no correction for bounds or canvas utilization) from home point
// 		a. Have user click to advance to “recentered spatialization”
// 6. Second cut: Check bounding box of data of first cut of spatialization within the canvas, and then rescale spatialization to fill canvas space
// 		b. Have user click to advance to “respaced spatialization”
// 7. Third cut: Check readability/clustering of points in the second cut of spatialization, and then rescale to use space more effectively
// 8. Plot points from the third cut, then display titles centered on each point; home point should be bolded
// 9. Move titles if they overlap (with a buffer to allow for isoline drawing)
// 10. Draw isoline(s) using existing D3 function?
// 11. Set position for legend/title (could be outside of “canvas” space to avoid issues with placement over data) and draw them
// 12. Display final image on screen
// 13. On click, export image as PNG for download