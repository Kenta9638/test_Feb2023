function  kk_coastline(fontsize,kk_color)


coastline_file = '/homes/metogra/kkurosaw/WORK/TOOLS/kk_tools/kk_tools/CoastlineWorldForRomsPac.mat';
coast = load(coastline_file);
m_plot(coast.lon,coast.lat,'linestyle','-','color',kk_color,'linewidth',fontsize);hold on;



end

