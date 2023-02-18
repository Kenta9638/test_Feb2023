function map = kk_blue_red();
%

  Cmap = cptcmap('GMT_jet');
  tmp_cmap1 = Cmap(140:end,:);  % Red to White
  tmp_cmap3 = Cmap(1:96,:);    % White to Blue
  for i = 1:20
    tmp_cmap4(i,:) = Cmap(139,:);
  end

  A = Cmap(139,:); B = Cmap(96,:);
  tate_o = 1:2; yoko_o = 1:3;
  tate_m = rot90(1:2/(length(tmp_cmap1)-length(tmp_cmap3)+50):2,3);
  cmap = [A;B];
  tmp_cmap2 = interp2(yoko_o,tate_o,cmap,yoko_o,tate_m); % White to Blue

  A = Cmap(139,:); B = tmp_cmap2(end,:);
  tate_o = 1:2; yoko_o = 1:3;
  tate_m = rot90(1:0.03:2,3);
  cmap = [A;B];
  tmp_cmap5 = interp2(yoko_o,tate_o,cmap,yoko_o,tate_m); % White to Blue

  Cmap = [tmp_cmap3;flipud(tmp_cmap5);tmp_cmap4;tmp_cmap1];
  Cmap = [tmp_cmap3;flipud(tmp_cmap2)];
  for i = 1:15:132
    Cmap(i,:) = [];
  end
  Cmap2 = [Cmap;tmp_cmap4;tmp_cmap1];
  colormap_red  = tmp_cmap1;
  colormap_blue = [Cmap;tmp_cmap4;tmp_cmap4(1:7,:)];
%  A = figure; colormap(A,colormap_red); colorbar
%  B = figure; colormap(B,flipud(colormap_blue)); colorbar

  tate_o = 1:length(colormap_red); yoko_o = 1:3;
  tate_m = rot90(1:length(colormap_red)/150:length(colormap_red),3);
  colormap_red_mod = interp2(yoko_o,tate_o,colormap_red,yoko_o,tate_m);
  tate_o = 1:length(colormap_blue); yoko_o = 1:3;
  tate_m = rot90(1:length(colormap_blue)/150:length(colormap_blue),3);
  colormap_blue_mod = interp2(yoko_o,tate_o,colormap_blue,yoko_o,tate_m);

  Cmap = [colormap_blue_mod;colormap_red_mod];
  map = Cmap;

return;
