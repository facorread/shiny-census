// Code by Edwin de Jonge (2018) https://github.com/rstudio/leaflet/pull/598
// See related article and code by Stefan Haring (2020)
// https://towardsdatascience.com/eye-catching-animated-maps-in-r-a-simple-introduction-3559d8c33be1
// Update styles of map elements.
window.LeafletWidget.methods.setStyle = function(category, layerId, style) {
  var map = this;
  if (!layerId) {
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)) {
    layerId = [layerId];
  }

  style = HTMLWidgets.dataframeToD3(style);

  layerId.forEach(function(d,i) {
    var layer = map.layerManager.getLayer(category, d);
    if (layer) {
      layer.setStyle(style[i]);
    }
  });
};


window.LeafletWidget.methods.setLabel = function(category, layerId, label) {
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)) {
    layerId = [layerId];
  }

  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer) {
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};
