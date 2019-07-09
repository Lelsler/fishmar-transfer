% plot solutions to harvest rate evolution for Pella-Tomlnson growth curve

syms u(F) E F L V a r b xi R p
% E=ParamEsts(1);
% xi=ParamEsts(2);
% R=ParamEsts(3);
% b=ParamEsts(4);
C=.2; %portion of cost
xi=1;
R=1.4116;
b=1;
V=1;
r=2;
a=1;
p=1;
v=0.2;
Es=1;
L2=0; %effort-based cost

Lrange=[0 2]; %volume-based cost
L2range=[0 .2]; %effort-based cost
Vrange=[0.01:(3-.01)/100:3]; %reference price
writematrix(Vrange,'solutions_x_rq3.csv'); %LGE new

scrsz = get(0,'ScreenSize');
%plot harvest rates
figs(1)=figure('Color', [1 1 1],'Position',[1 scrsz(2) scrsz(3)/1.5 scrsz(4)/1.5]);
spl=1;
for L_val=1:length(Lrange)
    for L2_val=1:length(L2range)
        subplot(2,2,spl)
        spl=spl+1;
        hold on
        L=Lrange(L_val);
        L2=L2range(L2_val);
        v=0;
        %case 1: slow institution, exclusive access
        Fsols=ones(3,length(Vrange))*(-100);
        for i=1:length(Vrange)
            V=Vrange(i);
            du(F)=(L2 - L*((F - r)/a + (F)/a) + v/a - (V*a*((F - r)/a + (F)/a))/(F*(F - r))); %linear harvest cost + effort cost, slow institution exclusive access
            ddu(F)=diff(du,F);
            Fsol=vpasolve(du);
            ddu_sol=subs(ddu,Fsol);
            
            Fsol(eval(imag(Fsol)~=0))=-100; %take out imaginary solutions
            Fsol(Fsol<0)=-100; %take out negative solutions
            Fsol(Fsol>r)=-100; %take out F>r solutions
            Fsols(1:length(Fsol),i)=sort(Fsol,'descend');
            Fsols(Fsols==-100)=NaN;
            Fsols=sort(Fsols,'descend');
        end
        Fsols=log2(Fsols/(r/2));
        
        plot0=plot(Vrange,Fsols','k','LineWidth',4);
        plot0(2).LineStyle='--';
        %    plot0=scatter(Lrange,Fsols(1,:)','.k','LineWidth',2);
        
        %case 2: fast institution, open access
        Fsols=ones(3,length(Vrange))*(-100);
        for i=1:length(Vrange)
            V=Vrange(i);
            du(F)=(L2 - L*((F - r)/a) - (V*a*((F - r)/a ))/(F*(F - r))); %linear harvest cost + effort cost with fast institution, open access
            ddu(F)=diff(du,F);
            Fsol=vpasolve(du);
 %           ddu_sol=subs(ddu,Fsol);
            
            Fsol(eval(imag(Fsol)~=0))=-100; %take out imaginary solutions
            Fsol(Fsol<0)=-100; %take out negative solutions
            Fsol(Fsol>r)=-100; %take out F>r solutions
            Fsols(1:length(Fsol),i)=sort(Fsol,'descend');
            Fsols(Fsols==-100)=NaN;
            Fsols=sort(Fsols,'descend');
        end
        Fsols=log2(Fsols/(r/2));
        plot0=plot(Vrange,Fsols','r','LineWidth',2);
        plot0(2).LineStyle='--';
        
        ylim([-5 1])
        xlabel 'reference price (V)'
        ylabel 'harvest rate log_2(F/F_{MSY})'
        title(['marginal costs (I=' num2str(L) ', I_e=' num2str(L2) ')'])
    end
end


%plot stock levels
figs(2)=figure('Color', [1 1 1],'Position',[1 scrsz(2) scrsz(3)/1.5 scrsz(4)/1.5]);
spl=1;
for L_val=1:length(Lrange)
    for L2_val=1:length(L2range)
        subplot(2,2,spl)
        spl=spl+1;
        hold on
        L=Lrange(L_val);
        L2=L2range(L2_val);
        v=0;
        %case 1: slow institution, exclusive access
        Fsols=ones(3,length(Vrange))*(-100);
        for i=1:length(Vrange)
            V=Vrange(i);
            du(F)=(L2 - L*((F - r)/a + (F)/a) + v/a - (V*a*((F - r)/a + (F)/a))/(F*(F - r))); %linear harvest cost + effort cost, slow institution exclusive access
            ddu(F)=diff(du,F);
            Fsol=vpasolve(du);
            ddu_sol=subs(ddu,Fsol);
            
            Fsol(eval(imag(Fsol)~=0))=-100; %take out imaginary solutions
            Fsol(Fsol<0)=-100; %take out negative solutions
            Fsol(Fsol>r)=-100; %take out F>r solutions
            Fsols(1:length(Fsol),i)=sort(Fsol,'descend');
            Fsols(Fsols==-100)=NaN;
            Fsols=sort(Fsols,'descend');
        end
        Ssols=log2(2-Fsols/(r/2));
        writematrix(Ssols,'solutions_onerun_rq3.csv'); %LGE new
        plot0=plot(Vrange,Ssols','k','LineWidth',4);
        plot0(2).LineStyle='--';
        %    plot0=scatter(Lrange,Fsols(1,:)','.k','LineWidth',2);
        
        %case 2: fast institution, open access
        Fsols=ones(3,length(Vrange))*(-100);
        for i=1:length(Vrange)
            V=Vrange(i);
            du(F)=(L2 - L*((F - r)/a) - (V*a*((F - r)/a ))/(F*(F - r))); %linear harvest cost + effort cost with fast institution, open access
            ddu(F)=diff(du,F);
            Fsol=vpasolve(du);
 %           ddu_sol=subs(ddu,Fsol);
            
            Fsol(eval(imag(Fsol)~=0))=-100; %take out imaginary solutions
            Fsol(Fsol<0)=-100; %take out negative solutions
            Fsol(Fsol>r)=-100; %take out F>r solutions
            Fsols(1:length(Fsol),i)=sort(Fsol,'descend');
            Fsols(Fsols==-100)=NaN;
            Fsols=sort(Fsols,'descend');
        end
        Ssols=log2(2-Fsols/(r/2));
        writematrix(Ssols,'solutions_fast_rq3.csv'); %LGE new
        plot0=plot(Vrange,Ssols','r','LineWidth',2);
        plot0(2).LineStyle='--';
        
        ylim([-5 1])
        xlabel 'reference price (V)'
        ylabel 'biomass log_2(S/S_{MSY})'
        title(['marginal costs (I=' num2str(L) ', I_e=' num2str(L2) ')'])
    end
end

