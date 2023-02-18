%=====================================================================
% Convert ncview colormap files (*.h) into matlab executable 
% (*.m) files.  You can simply call new colormaps such that 
% "colormap(colormaps_rainbow)".
%                                Yusuke Uchiyama, UCLA, 06-23-2010
%=====================================================================
%
clear all;
close all;


cmaps={'3gauss','bright','helix2','jaisnc','rainbow',...
       '3saw','bw','helix','jaisnd','roullet','banded',...
       'default','hotres','jaison','ssec','blue_red',...
       'detail','jaisn2','jet','wheel','blu_red','extrema',...
       'jaisnb','manga'};


for ifile=1:length(cmaps);

  cmap  =['colormaps_' char(cmaps(ifile))];
  input =[cmap '.h'];
  output=[cmap '.m'];
  disp([' processing ' input ' --> ' output]);

% read *.h file
  fid=fopen(input,'r');       		% open input file
  F=fread(fid);               		% read out the file
  fclose(fid);                		% close input file
  s=setstr(F');               		% convert to strings
  ind=find(s==10);            		% search for 'LF'(return) (id=10)
  ind=[0 ind];                		% adjust first row

% arrange input data string
  detect1=0; count=0;
  for jj=2:length(ind);       		% process row by row
    line=s(ind(jj-1)+1:ind(jj));
    is1=findstr(line,'{');			% detect first data line
    is2=findstr(line,'};');			% detect last data line
    if ~isempty(is1);				% remove unnecessary portion
      detect1=jj;
      line=squeeze(line(is1+1:end));
    end;
  if ~isempty(is2);					% remove unnecessary portion
    detect2=jj;
    line=squeeze(line(1:is2-1));
    line=[line ','];				% add a comma at the end
  end;

  if (detect1~=0);
    line=[', ' line];				% add a comma at the beginning
    cloc=find(line==',');			% split by commas
    for ii=1:length(cloc)-1;		% put data in array
      count=count+1;
      dat(count)=str2num(line(cloc(ii)+1:cloc(ii+1)-1));
    end;
  end;
end;

% re-arrange data array
  N=length(dat); L=round(N/3);		% reshape and normalize
  if (L~=N/3); disp('Data size error in conv_color.m'); break; end;
  col=reshape(dat,3,L)./255;
  col(col<0)=0; col(col>1)=1;

% output
  fid = fopen(output,'w');
    fprintf(fid,'%s\n',['function map = ' cmap '();']);
    fprintf(fid,'%s\n', '%');
    fprintf(fid,'%s\n', '%================================================');
    fprintf(fid,'%s\n', '%');
    fprintf(fid,'%s\n',['%   function map = ' cmap '();']);
    fprintf(fid,'%s\n', '%');
    fprintf(fid,'%s\n',['%         Yusuke Uchiyama, UCLA, ' date]);
    fprintf(fid,'%s\n', '%');
    fprintf(fid,'%s\n', '%================================================');
    fprintf(fid,'\n%s\n',['map = [']);
    for jj=1:L;
      fprintf(fid,'       %12.6f    %12.6f   %12.6f;\n',col(1,jj),col(2,jj),col(3,jj));
    end;
    fprintf(fid,'%s\n\n', '];');
    fprintf(fid,'%s\n', 'return;');
  fclose(fid);

end;	% end loop on ifile

return

