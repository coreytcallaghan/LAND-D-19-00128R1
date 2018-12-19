// Sample USGS NLCD data with US eBird checklists
// contact: mitchell.lyons@gmail.com

var landcover2011 = ee.Image("USGS/NLCD/NLCD2011").select("landcover").rename("lc")

var landcover2011_agg = landcover2011.remap([11, 21, 22, 23, 24, 31, 41, 42, 43, 52, 71, 81, 82, 90, 95],
                                            [1,  3,  4,  5,  5,  3,  1,  1,  1,  1,  2,  2,  2,  1,  1])
                                     .rename("lcagg")
                                            
/* // in case we want to write the key to an asset or output
var rename_dict = {
  1: 'Green Area',
  2: 'Agriculture',
  3: 'Urban Green',
  4: 'Low intensity',
  5: 'Med/High Intensity'
}*/

print(landcover2011_agg)

var add_lc_properties = function(f) {
  
  var geom = f.geometry()
  
  // original class
  var lcagg = landcover2011_agg.reduceRegion({
    reducer: ee.Reducer.first(),
    geometry: geom,
    scale: 30,
    maxPixels: 1
  }).get("lcagg")
  
  // heterogeneity in aggregated class
  var lcagg_var = landcover2011_agg.reduceRegion({
    reducer: ee.Reducer.countDistinct(),
    geometry: geom.buffer(500),
    scale: 30,
    maxPixels: 5000
  }).get("lcagg")
  
  // heterogeneity in original classes
  var lc_var = landcover2011.reduceRegion({
    reducer: ee.Reducer.countDistinct(),
    geometry: geom.buffer(5000),
    scale: 30,
    maxPixels: 300000,
    bestEffort: true
  }).get("lc")
  
  var dict = ee.Dictionary({
    lc_agg: lcagg,
    lcagg_var: lcagg_var,
    lc_var: lc_var
  })
  
  return(f.set(dict))
}

var checklists_info = checklists.map(add_lc_properties)
print(checklists_info.first())

Export.table.toDrive({
  collection: checklists_info,
  description: "us_ebird_lc",
  folder: "ebird",
  fileNamePrefix: "us_ebird_lc",
  fileFormat: "CSV"
})

Map.addLayer(landcover2011_agg, {}, "LC agg", false)
Map.addLayer(checklists, {}, "checklists", false)