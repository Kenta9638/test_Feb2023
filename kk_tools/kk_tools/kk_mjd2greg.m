function [time]=kk_mjd2greg(mjd) 

% This method will not give dates accurately on the Gregorian Proleptic Calendar,
% i.e., the calendar you get by extending the Gregorian calendar backwards to
% years earlier than 1582. using the Gregorian leap year rules. In particular,
% the method fails if Y<400.


MONTH=['Jan'; 'Feb'; 'Mar'; 'Apr'; 'May'; 'Jun'; ... 
       'Jul'; 'Aug'; 'Sep'; 'Oct'; 'Nov'; 'Dec'];


  Z = mjd+2400001;
  W = floor((Z - 1867216.25)/36524.25);
  X = floor(W/4);
  A = Z+1+W-X;
  B = A+1524;
  C = floor((B-122.1)/365.25);
  D = floor(365.25*C);
  E = floor((B-D)/30.6001);
  F = floor(30.6001*E);

  Day   = floor(B-D-F);
  Month = E-1; 
  if Month>12, Month=Month-12; end;
  if Month<3,
    Year = C-4715;
  else
    Year = C-4716;
  end;
  if Day==0, Day=31; Month=Month-1; end;
  %%%if Month==0, Month=12; end;
  if Month==0; Month=12; Year=Year-1; end;
  
  %time=[num2str(Day),'-',MONTH(Month,1:3),'-',num2str(Year)];

  hour = (mjd-floor(mjd))*24;
  hour = num2str(hour);
  C = find(hour=='.');
  Hour = hour(1:C(1)-1);
  Hour = str2num(Hour);
  Hour = sprintf('%02d',Hour);
%  time=[num2str(Hour),'-', num2str(Day),'-',MONTH(Month,1:3),'-',num2str(Year)];

  Month = sprintf('%02d',Month);
  Day = sprintf('%02d',Day);

  time=[num2str(Year),num2str(Month),num2str(Day),num2str(Hour)];

return
