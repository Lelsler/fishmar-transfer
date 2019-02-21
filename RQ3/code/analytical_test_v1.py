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

Lrange=[0:2.5/50:2.5];
L2range=[0 .25 .5];

scrsz = get(0,'ScreenSize');
figs(1)=figure('Color', [1 1 1],'Position',[1 scrsz(2) scrsz(3) scrsz(4)/2.5]);
for spl=1:3
    subplot(1,3,spl)
    hold on
    L2=L2range(spl);
    %case 1.1: no stock valuation, perfect stock elasticity
    v=0;
    Es=1;
    Fsols=ones(3,length(Lrange))*(-100);
    for i=1:length(Lrange)
        L=Lrange(i);
        du(F)=(L2 - L*((F - r)/a + (Es*F)/a) + v*Es/a - (V*a*((F - r)/a + (Es*F)/a))/(F*(F - r))); %linear harvest cost + effort cost with elasticity Es=[0,1]
        Fsol=vpasolve(du);
        %F=Fsol;
        %du_sol=eval(du);
        %Fstab=diff(du_sol,F) %check stability;
        Fsol(eval(imag(Fsol)~=0))=-100; %take out imaginary solutions
        Fsol(Fsol<0)=-100; %take out negative solutions
        Fsol(Fsol>r)=-100; %take out F>r solutions
        Fsols(1:length(Fsol),i)=sort(Fsol,'descend');
        Fsols(Fsols==-100)=NaN;
        Fsols=sort(Fsols,'descend');
    end
    Fsols=log2(Fsols/(r/(p+1)));
    plot0=plot(Lrange,Fsols','k','LineWidth',2);
    plot0(2).LineStyle='--';

    %case 1.2: stock valuation
    v=0.2;
    Es=1;
    Fsols=ones(3,length(Lrange))*(-100);
    for i=1:length(Lrange)
        L=Lrange(i);
        du(F)=(L2 - L*((F - r)/a + (Es*F)/a) + v*Es/a - (V*a*((F - r)/a + (Es*F)/a))/(F*(F - r))); %linear harvest cost + effort cost with elasticity Es=[0,1]
        Fsol=vpasolve(du);
        Fsol(eval(imag(Fsol)~=0))=-100; %take out imaginary solutions
        Fsol(Fsol<0)=-100; %take out negative solutions
        Fsol(Fsol>r)=-100; %take out F>r solutions
        Fsols(1:length(Fsol),i)=sort(Fsol,'descend');
        Fsols(Fsols==-100)=NaN;
        Fsols=sort(Fsols,'descend');
    end
    Fsols=log2(Fsols/(r/(p+1)));
    plot0=plot(Lrange,Fsols','b','LineWidth',2);
    plot0(2).LineStyle='--';

    %case 1.3: half stock elasticity
    v=0;
    Es=0.5;
    Fsols=ones(3,length(Lrange))*(-100);
    for i=1:length(Lrange)
        L=Lrange(i);
        du(F)=(L2 - L*((F - r)/a + (Es*F)/a) + v*Es/a - (V*a*((F - r)/a + (Es*F)/a))/(F*(F - r))); %linear harvest cost + effort cost with elasticity Es=[0,1]
        Fsol=vpasolve(du);
        Fsol(eval(imag(Fsol)~=0))=-100; %take out imaginary solutions
        Fsol(Fsol<0)=-100; %take out negative solutions
        Fsol(Fsol>r)=-100; %take out F>r solutions
        Fsols(1:length(Fsol),i)=sort(Fsol,'descend');
        Fsols(Fsols==-100)=NaN;
        if spl>1
            Fsols=sort(Fsols,'descend');
        end
    end
    Fsols=log2(Fsols/(r/(p+1)));
    plot0=plot(Lrange,Fsols','r','LineWidth',2);
    plot0(2).LineStyle='--';
    xlabel 'marginal harvest-dependent cost'
    ylabel 'harvest rate log_2(F/F_{MSY})'
    title (['marginal effort-dependent cost=' num2str(L2)])
end
