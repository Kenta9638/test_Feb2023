function map = colormaps_blu_red();
%

  cmap = colormap(colormaps_hotres); close
  cmap(:,2) = cmap(:,2)+0.2;
  cmap = cmap*1.1;
  cmap(cmap>1) = 1;
  test_cmap = cmap(end,:);
  for i = 1:60; cmap = [cmap;test_cmap]; end
  map = cmap;

return;
