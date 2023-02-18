function tomac_png3
%SCREEN2JPEG Generate a JPEG file of the current figure with
%   dimensions consistent with the figure's screen dimensions.
%
%   SCREEN2JPEG('filename') saves the current figure to the
%   JPEG file "filename".
%
%    Sean P. McCarthy
%    Copyright (c) 1984-98 by MathWorks, Inc. All Rights Reserved

global k_savename

a = clock;
b = strcat(num2str(a(1), '%04d'), num2str(a(2), '%02d'), num2str(a(3), '%02d'), '-' ,...
    num2str(a(4), '%02d'), 'h', num2str(a(5), '%02d'), 'm', num2str(round(a(6)), '%02d'), 's');

%if nargin < 1
%     error('Not enough input arguments!')
%end
%oldscreenunits = get(gcf,'Units');
%oldpaperunits = get(gcf,'PaperUnits');
%oldpaperpos = get(gcf,'PaperPosition');
%set(gcf,'Units','pixels');
%scrpos = get(gcf,'Position');
%newpos = scrpos/100;
%set(gcf,'PaperUnits','inches','PaperPosition',newpos)
%print('-dpng','-r500',['/home/kurosawa/figs/' b '.png']); % 他の画像フォーマットしたい場合はここを編集
%drawnow
%set(gcf,'Units',oldscreenunits,'PaperUnits',oldpaperunits,...
%     'PaperPosition',oldpaperpos)
%set(gcf,'PaperPositionMode','auto')
fig = gcf;
fig.InvertHardcopy = 'off';
%print(['/home/kurosawa/_OLD_BLANTON_/figs/' b ],'-dpng','-r0')
%print(['/data9/kurosawa/figs/' b ],'-dpng','-r0')
['/glade/u/home/kkurosaw/FIGS/' b '_' k_savename ]
%print(['/data15/kurosawa/figs/' b '_' k_savename ],'-dpng','-r0')
print(['/glade/u/home/kkurosaw/FIGS/' b '_' k_savename '.png'],'-dpng','-r300')

end
