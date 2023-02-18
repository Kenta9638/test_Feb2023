%
% 
    close all ; clear all

    paletts = {'colormaps_jaisnb' 'colormaps_jaisn2' 'colormaps_jaisnc' 'colormaps_jaisnd' 'colormaps_jaison'} ;

    nc = netcdf ('coads_climatology.cdf', 'read') ;
      sst  = squeeze(nc{'SST'}(4,:,:)) ;
      wspd = squeeze(nc{'WSPD'}(6,:,:)) ;
      xlon = nc{'COADSX'}(:) ;
      ylat = nc{'COADSY'}(:) ;
      bad  = nc{'SST'}.missing_value(:) ;
    nc = close (nc) ; 

    sst(sst==bad)   = NaN ;
    wspd(wspd==bad) = NaN ;

    npals = length (paletts) ;

    for ip = 1:npals

       figure (ip) ; colormap(paletts{ip}) ;
         set (gcf,'Position', [10 700 600 300]) ; 
         [ch,ch]=contourf(xlon, ylat, sst,[-5:1:30]);
         set(ch,'edgecolor','none'); colorbar ; 
         axis([20 380 -65 65])
         title(strrep(paletts{ip},'_','\_'),'Color','k','FontSize', 20)
    end 
