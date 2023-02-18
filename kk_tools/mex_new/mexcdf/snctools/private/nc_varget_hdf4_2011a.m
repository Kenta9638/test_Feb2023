function data = nc_varget_hdf4_2011a(hfile,varname,varargin)
% HDF4 package backend for NC_VARGET

import matlab.io.hdf4.*

preserve_fvd = nc_getpref('PRESERVE_FVD');
use_std_hdf4_scaling = getpref('SNCTOOLS','USE_STD_HDF4_SCALING',false);


fid = fopen(hfile,'r');
fullfile = fopen(fid);
fclose(fid);

v = nc_getvarinfo(fullfile,varname);
switch(numel(varargin))
    
    case 0
        % retrieve everything.
        start = zeros(1,numel(v.Size));
        edge = v.Size;
        stride = ones(1,numel(v.Size));
        
    case 1
        % if only start was provided, then the count is implied to be one.
        start = varargin{1};
        edge = ones(1,numel(v.Size));
        stride = ones(1,numel(v.Size));
        
    case 2        
        % just a contiguous hyperslab.
        start = varargin{1};
        edge = varargin{2};
        stride = ones(1,numel(v.Size));
        
    case 3
        start = varargin{1};
        edge = varargin{2};
        stride = varargin{3};
end

% Force to be rows.
start = start(:)';
edge = edge(:)';
stride = stride(:)';


negs = find((edge<0) | isinf(edge));
if isempty(stride)
    edge(negs) =        v.Size(negs) - start(negs);
else
    edge(negs) = floor((v.Size(negs) - start(negs))./stride(negs));
end

if ~preserve_fvd
    start = fliplr(start);
    edge = fliplr(edge);
    stride = fliplr(stride);
end
    
sd_id = sd.start(fullfile,'read');

try
    idx = sd.nameToIndex(sd_id,varname);
    sds_id = sd.select(sd_id,idx);
    data = sd.readData(sds_id,start,edge,stride);
    
    % fill value, scale factor, add_offset, missing value, etc
	try
        [cal,~,offset] = sd.getCal(sds_id); 
    catch me %#ok<NASGU>
		cal = 1; offset = 0;
	end

    if use_std_hdf4_scaling
        data = cal*(double(data) - offset);  
    else
        % Use standard CF convention scaling.
        data = cal * double(data) + offset;
    end
    
    try
        fv = hdfsd('getfillvalue',sds_id);
        fv = double(fv);
        data = double(data);
        data(data==fv) = NaN;
    catch me %#ok<NASGU>
    end
    
    % Missing value is to be handled the same as fill value
    try
        attr_index = sd.findAttr(sds_id,'missing_value');
        missing_value = sd.readAttr(sds_id,attr_index);
        missing_value = double(missing_value);
        data = double(data);
        data(data==missing_value) = NaN;
    catch me %#ok<NASGU>
        %
    end


catch me

    if exist('sds_id','var')
        sd.endAccess(sds_id);
    end
    sd.close(sd_id);
	rethrow(me);
end

if exist('sds_id','var')
    sd.endAccess(sds_id);
end
sd.close(sd_id);

if ~preserve_fvd
    data = permute(data,ndims(data):-1:1);
end

% If 1D vector, force to be a column.
if numel(start) == 1
    data = data(:);
end

return
