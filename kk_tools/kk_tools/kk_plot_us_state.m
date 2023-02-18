function  kk_plot_us_state(linecolor,linestyle,linewid)
states = shaperead("usastatelo","UseGeoCoords",true);
state_num = 51;
for i = 1:state_num
  tmp_lon = states(i,:).Lon;
  tmp_lat = states(i,:).Lat;
  m_plot(tmp_lon,tmp_lat,'linestyle',linestyle,'color',linecolor,'linewidth',linewid); hold on
end
end
