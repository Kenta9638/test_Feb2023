function [lon] = kk_rehape(lon,A,B)

[a b] = size(lon); c = a*b;
%A = c; B = 1; 
C = A*B;
lon = rot90(lon,3); lon = fliplr(lon); lon = reshape(lon,c,1);
while 1
 [a b] = size(lon);
  if a*b == C; break; end;
    lon(a*b,:) = [];
end
lon = reshape(lon,A,B);

end

