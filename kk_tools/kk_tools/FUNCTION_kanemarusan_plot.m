function [out_Ku out_DFR] = FUNCTION_kanemarusan_plot

%%clear all;
%%close all;

  DIR  = '/data_kotsuki01/kurosawa/ICE_FLAG/figure/Kanemarusan/20180829_DFR';
  rho  = ["005" "010" "020" "030" "050"];
  var1 = ["Ka" "Ku"];
  var2 = ["snow" "rain"];

  %--- read ---
  for tmp_rho = rho
    for tmp_var1 = var1
      for tmp_var2 = var2
        if      tmp_var2 == 'snow'; 
          tmp_var3 = 'GP';
          pred_file = [DIR '/' char(tmp_var2) '_' char(tmp_var1) '_' char(tmp_var3) 'rho' char(tmp_rho) '.txt'];
          tmp_data = dlmread(pred_file);
          eval([char(tmp_var2) '_' char(tmp_var1) '_' char(tmp_rho) '= tmp_data(:,2);']);
        elseif  tmp_var2 == 'rain'; 
          tmp_var3 = 'MP';
          pred_file = [DIR '/' char(tmp_var2) '_' char(tmp_var1) '_' char(tmp_var3) '.txt'];
          tmp_data = dlmread(pred_file);
          eval([char(tmp_var2) '_' char(tmp_var1) '= tmp_data(:,2);']);
        end
      end
    end
  end

  %--- DFR ---
  for tmp_rho = rho
    for tmp_var2 = var2
      if      tmp_var2 == 'snow'; 
        eval([char(tmp_var2) '_DFR_' char(tmp_rho) '=' char(tmp_var2) '_Ku_' char(tmp_rho) '-' char(tmp_var2) '_Ka_' char(tmp_rho) ';']);
      elseif  tmp_var2 == 'rain'; 
        eval([char(tmp_var2) '_DFR =' char(tmp_var2) '_Ku -' char(tmp_var2) '_Ka;']);
      end
    end
  end

  out_Ku{1} = rain_Ku;      out_DFR{1} = rain_DFR;
  out_Ku{2} = snow_Ku_005;  out_DFR{2} = snow_DFR_005;
  out_Ku{3} = snow_Ku_010;  out_DFR{3} = snow_DFR_010;
  out_Ku{4} = snow_Ku_020;  out_DFR{4} = snow_DFR_020;
  out_Ku{5} = snow_Ku_030;  out_DFR{5} = snow_DFR_030;
  out_Ku{6} = snow_Ku_050;  out_DFR{6} = snow_DFR_050;

%%  figure
%%  plot(rain_Ku,rain_DFR);         hold on
%%  plot(snow_Ku_005,snow_DFR_005); hold on
%%  plot(snow_Ku_010,snow_DFR_010); hold on
%%  plot(snow_Ku_020,snow_DFR_020); hold on
%%  plot(snow_Ku_030,snow_DFR_030); hold on
%%  plot(snow_Ku_050,snow_DFR_050); hold on
%%  axis([0 45 -5 15])
%%  grid on

  return
    
