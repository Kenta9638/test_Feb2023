function  posi = kk_subplot_posi(wid_num,hei_num,wid,hei,dx,dy,zero,figs,row)

  %--- position ---
  column = figs/row;
  counter  = 0;
  tmp_num1 = (wid-1)+wid_num*(hei-1);
  for i = 1:row
    tmp_num2 = (zero(1)+wid_num*(zero(2)-1))+(hei+dy)*wid_num*(i-1);
    for j = 1:column
      tmp_num3 = tmp_num2+(wid+dx)*(j-1);
      counter = counter + 1;
      posi{counter} = [tmp_num3 tmp_num3+tmp_num1];
    end
  end




end

