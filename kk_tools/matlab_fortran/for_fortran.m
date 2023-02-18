clear all;
close all;

lat = ncread('/data15/kurosawa/CLW/test/2014/06/20140605/0000-0555/0000-0025/GW1AM2_201406050022_177A_L2SGCLWLD2210210.nc','Latitude of Observation Point');
out_file_name = ['./test.dat'];

size(lat)


csvwrite(out_file_name,lat)

return
