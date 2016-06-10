export PATH=/Library/Frameworks/GDAL.framework/Programs:$PATH
rm -r temp
ogr2ogr -clipsrc -20026376 -20048966 20026376 20048966 temp simplified_water_polygons.shp -s_srs EPSG:3857 -t_srs EPSG:4326 -nln shapes
