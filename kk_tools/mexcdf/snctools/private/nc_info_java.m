function fileinfo = nc_info_java ( ncfile )
% NC_INFO_JAVA:  java backend for nc_info m-file
%
% This function returns the same metadata structure using the java 
% API.  

import ucar.nc2.dods.*     % import opendap reader classes
import ucar.nc2.*          % have to import this (NetcdfFile) as well for local reads


close_it = true;

% Try it as a local file.  If not a local file, try as
% via HTTP, then as dods
if isa(ncfile,'ucar.nc2.NetcdfFile')
	jncid = ncfile;
	ncfile = char(jncid.getLocation());
	close_it = false;
elseif isa(ncfile,'ucar.nc2.dods.DODSNetcdfFile')
	jncid = ncfile;
	ncfile = char(jncid.getLocation());
	close_it = false;
elseif exist(ncfile,'file')
	jncid = NetcdfFile.open(ncfile);
else
	try 
		jncid = NetcdfFile.open ( ncfile );
    catch  %#ok<CTCH>
		try
			jncid = DODSNetcdfFile(ncfile);
        catch  %#ok<CTCH>
			error ( 'SNCTOOLS:nc_varget_java:fileOpenFailure', ...
                'Could not open ''%s'' as either a local file, a regular URL, or as a DODS URL.', ...
                ncfile );
		end
	end
end


fileinfo.Filename = ncfile;


root_group = jncid.getRootGroup();
fileinfo.Dimension = get_dimensions_j ( root_group );
fileinfo.Dataset = get_variables_j ( root_group );

% Get the global attributes and variable attributes
j_att_list = root_group.getAttributes();
fileinfo.Attribute = snc_java_bundle_atts ( j_att_list );


if close_it
	close ( jncid );
end

return





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Dimension = get_dimensions_j ( root_group )
% GET_DIMENSIONS_J:  Get the dimensions using the java backend.
%

dim_count = 0;
dims = root_group.getDimensions();

%
% Set up an empty list first, in order to pre-allocate.
Dimension.Name = '';
Dimension.Length = 0;
Dimension.Unlimited = 0;

Dimension = repmat ( Dimension, dims.size(), 1 );

dims_iterator = dims.listIterator();
while 1

    try

        %
        % This throws an exception when we've reached the end of the list.
        jDim = dims_iterator.next();

    catch %#ok<CTCH>

        %
        % Break out of the while loop, there are no more dimensions to 
        % process.
        break;
    end

    dim_count = dim_count + 1;

    mdim.Name = char ( jDim.getName() );
    mdim.Length = jDim.getLength();
    mdim.Unlimited = jDim.isUnlimited();

    Dimension(dim_count,1) = mdim;

end

if dim_count == 0

    % 
    % Singleton variable case.
    Dimension = [];

end













%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function Dataset = get_variables_j ( root_group )

%
% Get information on the variables themselves.
var_count = 0;
j_var_list = root_group.getVariables();
j_var_iterator = j_var_list.listIterator();

Dataset.Name = '';
Dataset.Nctype = 0;
Dataset.Unlimited = 0;
Dataset.Dimension = {};
Dataset.Size = 0;
Dataset.Attribute = struct([]);

Dataset = repmat ( Dataset, j_var_list.size(), 1 );

while 1

    try

        %
        % This throws an exception when we've reached the end of the list.
        jvarid = j_var_iterator.next();

    catch  %#ok<CTCH>
        
        %
        % No more variables left to process.
        break;

    end

    var_count = var_count + 1;


    %mDataset = nc_getvarinfo ( root_group, jvarid );
	mDataset = snc_java_varid_info ( jvarid );

    Dataset(var_count,1) = mDataset;

end

if var_count == 0
    Dataset = [];
end


