function data = func_readconvgrd(tate,yoko,text_length,file_name)

D = dlmread(file_name);

counter = 1;
for i = 1:tate
  C = 0;
  tmp_data = 0;
  while C ~= yoko 
    A = D(counter,:);
    B = text_length; % length(A)
    tmp_data = [tmp_data A]; 
    C = C + B;
    counter = counter + 1;
  end 
  tmp_data(1) = []; 
  kk_dat(i,:) = tmp_data;
  clear tmp_data
end

data = kk_dat;

end
