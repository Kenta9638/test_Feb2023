function bigpattern = applyhatch(h,patterns,colorlist,step) % <-- KK !!


%APPLYHATCH Apply hatched patterns to a figure
%  APPLYHATCH(H,PATTERNS) creates a new figure from the figure H by
%  replacing distinct colors in H with the black and white
%  patterns in PATTERNS. The format for PATTERNS can be
%    a string of the characters '/', '\', '|', '-', '+', 'x', '.'
%    a cell array of matrices of zeros (white) and ones (black)
%
%  APPLYHATCH(H,PATTERNS,COLORS) maps the colors in the n by 3
%  matrix COLORS to PATTERNS. Each row of COLORS specifies an RGB
%  color value.
%
%  Note this function makes a bitmap image of H and so is limited
%  to low-resolution, bitmap output.
%
%  Example 1:
%    bar(rand(3,4));
%    applyhatch(gcf,'\-x.');
%
%  Example 2:
%    colormap(cool(6));
%    pie(rand(6,1));
%    legend('Jan','Feb','Mar','Apr','May','Jun');
%    applyhatch(gcf,'|-+.\/',cool(6));
%
%  See also: MAKEHATCH
%  Copyright 2002-2009 The MathWorks, Inc.
  

%
%  modified by KK !!!!!!!
%  
%  2019/02/25
%
%  SAMPLE: figure9, 10, 11, 12 in MERGE paper
%
%  /data_kotsuki01/kurosawa/MERGE/Journal_rev_20181105/code/figure/
%
%  fig4_3figs_LTS_diff_050.m 
%  fig5_3x3figs_LTS_050.m
%                       etc ...










oldppmode = get(h,'paperpositionmode');
oldunits = get(h,'units');
set(h,'paperpositionmode','auto');
set(h,'units','pixels');
figsize = get(h,'position');
if nargin == 2
  colorlist = [];
end
if verLessThan('matlab','8.4.0')
  bits = hardcopy(h,'-dzbuffer','-r0');
else
  bits = print(h,'-RGBImage','-r0');
end
set(h,'paperpositionmode',oldppmode);

bwidth = size(bits,2);
bheight = size(bits,1);
bsize = bwidth * bheight;
if ~isempty(colorlist)
  colorlist = uint8(255*colorlist);
  [colors,colori] = nextnonbw(0,colorlist,bits);
else
  colors = (bits(:,:,1) ~= bits(:,:,2)) | ...
	   (bits(:,:,1) ~= bits(:,:,3));
end
pati = 1;
colorind = find(colors);
while ~isempty(colorind)
  colorval(1) = bits(colorind(1));
  colorval(2) = bits(colorind(1)+bsize);
  colorval(3) = bits(colorind(1)+2*bsize);
  if iscell(patterns)
    pattern = patterns{pati};
  elseif isa(patterns,'char')
    pattern = makehatch(patterns(pati),step); % <-- KK !!
  else
    pattern = patterns;
  end
  pattern = uint8(255*(1-pattern));
  pheight = size(pattern,2);
  pwidth = size(pattern,1);
  ratioh = ceil(bheight/pheight);
  ratiow = ceil(bwidth/pwidth);
  bigpattern = repmat(pattern,[ratioh ratiow]);
  if ratioh*pheight > bheight
    bigpattern(bheight+1:end,:) = [];
  end
  if ratiow*pwidth > bwidth
    bigpattern(:,bwidth+1:end) = [];
  end
  bigpattern = repmat(bigpattern,[1 1 3]);
  color = (bits(:,:,1) == colorval(1)) & ...
	  (bits(:,:,2) == colorval(2)) & ...
	  (bits(:,:,3) == colorval(3));
  color = repmat(color,[1 1 3]);
  bits(color) = bigpattern(color);
  if ~isempty(colorlist)
    [colors,colori] = nextnonbw(colori,colorlist,bits);
  else
    colors = (bits(:,:,1) ~= bits(:,:,2)) | ...
	     (bits(:,:,1) ~= bits(:,:,3));
  end

  colorind = find(colors);
  pati = (pati + 1);
  if pati > length(patterns)
    pati = 1;
  end
end

bigpattern = squeeze(bigpattern(:,:,1)); % <-- KK !!

return

%%%newfig = figure('units','pixels','visible','off');
%%%imaxes = axes('parent',newfig,'units','pixels');
%%%im = image(bits,'parent',imaxes);
%%%fpos = get(newfig,'position');
%%%set(newfig,'position',[fpos(1:2) figsize(3) figsize(4)+1]);
%%%set(imaxes,'position',[0 0 figsize(3) figsize(4)+1],'visible','off');
%%%set(newfig,'visible','on');

function [colors,out] = nextnonbw(ind,colorlist,bits)
out = ind+1;
colors = [];
while out <= size(colorlist,1)
  if isequal(colorlist(out,:),[255 255 255]) | ...
	isequal(colorlist(out,:),[0 0 0])
    out = out+1;
  else
    colors = (colorlist(out,1) == bits(:,:,1)) & ...
	     (colorlist(out,2) == bits(:,:,2)) & ...
	     (colorlist(out,3) == bits(:,:,3));
    return
  end
end


function A = makehatch(hatch,step)  % <-- KK !!
%MAKEHATCH Predefined hatch patterns
%  MAKEHATCH(HATCH) returns a matrix with the hatch pattern for HATCH
%   according to the following table:
%      HATCH        pattern
%     -------      ---------
%        /          right-slanted lines
%        \          left-slanted lines
%        |          vertical lines
%        -          horizontal lines
%        +          crossing vertical and horizontal lines
%        x          criss-crossing lines
%        .          single dots
%
%  See also: APPLYHATCH
%  Copyright 2002-2009 The MathWorks, Inc.
n = step;   % <-- KK !! 
A=zeros(n);
switch (hatch)
 case '/'
  A = fliplr(eye(n));
 case '\'
  A = eye(n);
 case '|'
  A(:,1) = 1;
 case '-'
  A(1,:) = 1;
 case '+'
  A(:,1) = 1;
  A(1,:) = 1;
 case 'x'
  A = eye(n) | fliplr(diag(ones(n-1,1),-1));
 case '.'
  A(1:2,1:2)=1;
 otherwise
  error(['Undefined hatch pattern "' hatch '".']);
end
