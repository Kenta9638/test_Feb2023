function map = colormaps_blu_red();
%

  cmap = cptcmap('GMT_wysiwyg'); cmap(1:4,:) = []; cmap(end-2:end,:) = [];
  tate_o = 1:length(cmap); yoko_o = 1:3; tate_m = rot90(1:0.05:length(cmap),3);
  cmap_rain  = interp2(yoko_o,tate_o,cmap,yoko_o,tate_m);
  map = cmap_rain ;

return;
