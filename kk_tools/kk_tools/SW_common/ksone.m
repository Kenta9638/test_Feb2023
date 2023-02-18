function [p, D] = ksone(x)
% Kolmogorov-Smirnov Normality test - compare distribution with normal
% distribution, and get p value. Taken from Press et al.
% Note that the p value is different than that given by sigma stat, for
% reasons that I cannot understand. probks returns the same value as a verified
% version (xprobks)
% The D value is exactly the same. 
% P. Manis 27 April 1999.
%
a=size(x);
if(a(1) > 1) % transpose to match the way we solve.
   x=x';
end

xs=sort(x); % Order data
xs = (xs-mean(xs))/std(xs); % transform data to 0 mean and unit sd
n = length(x); % length of data
m = 1/n; % step size for cdf
cp=m:m:1; % generate simple staircase of right unit sizes
xcdf = normcdf(xs, 0, 1); % simple norm distribution to compare
D = max(abs(xcdf-cp)); % max difference between distributions
p = probks(D, n); % compute p from modified probks
%disp(sprintf('ksone -- D: %12.5f   p: %12.5f', D, p))
%plot(xs, cp, '-xk', xs, xcdf, '-ro');
%en=sqrt(n);
%px=xprobks((en+0.12+(0.11/en))*D) % verified p value against xprobks...


function [sum] = probks(D, n)
% probks from Press et al. Modified to have n passed so that
% alam is computed inside routine instead of outside. Otherwise,
% identical...
%
EPS1=0.001;
EPS2=1.0E-8;
fac=2; sum=0; termbf=0;

en=sqrt(n);
alam = (en+0.12+(0.11/en))*D;
a2 = -2.0*alam*alam;

for j=1:100
   term = fac*exp(a2*j*j);
   sum = sum + term;
   if(abs(term) <= EPS1*termbf | abs(term) <= EPS2*sum)
      return
   end
   fac = - fac;
   termbf = abs(term);
end
sum=1;
return;
