timestart = tic;
% Print out the basic information on the model
simulationstart=clock; % the time that the loop starts
disp('==================================================================================');
disp(['Running  ' mfilename '.m                      ' datestr(simulationstart)]);
disp('==================================================================================');
data=xlsread('GSS2014.xlsx');
%% data reading
x=data(:,2);
v=data(:,22);
v0=data(:,28);
v1=data(:,29);
z=data(:,4);
y=data(:,24);
datan=[y x z v0 v1 v];
[row,col]=find(isnan(y)==1|isnan(x)==1|isnan(z)==1|isnan(v0)==1|isnan(v1)==1);
datan(row,:)=[];
yn=datan(:,1); xn=datan(:,2); zn=datan(:,3); v0n=datan(:,4); v1n=datan(:,5); vn=datan(:,6);
N=size(datan,1)

datanmsc=datan;
[rowmsc,colmsc]=find(isnan(yn)==1|isnan(xn)==1|isnan(zn)==1|isnan(vn)==1);
datanmsc(rowmsc,:)=[];
ym=datanmsc(:,1); xm=datanmsc(:,2); zm=datanmsc(:,3); vm=datanmsc(:,6);
Nmsc=size(datanmsc,1)

%% Estimation
b=-0.1:0.01:0.50;%age
c=0:0.02:1;%schooling years
d=0.5:0.02:6;%income
a=5:0.5:50;
bb=repmat(b',length(c),1);
cc=kron(c',ones(length(b),1));
bc=[bb,cc];
dd=repmat(d',length(a),1);
aa=kron(a',ones(length(d),1));
da=[dd,aa];
bcda=repmat(bc,length(da),1);
dabc=kron(da,ones(length(bc),1));
coeff=[bcda,dabc];
Lc=length(coeff);
coeff1=coeff(:,1);
coeff2=coeff(:,2);
coeff3=coeff(:,3);
coeff4=coeff(:,4);

w1=(yn>1);
w2=(yn==3);
xni=repmat(xn,1,N);
zni=repmat(zn,1,N);
v0ni=repmat(v0n,1,N);
v1ni=repmat(v1n,1,N);
w1i=repmat(w1,1,N);
w2i=repmat(w2,1,N);
hnv0=1.06*std(v0n)*N^(-1/5);
hnv1=1.06*std(v1n)*N^(-1/5);
hnz=1.06*std(zn)*N^(-1/5);
hnx=1.06*std(xn)*N^(-1/5);
ker=normpdf((v0ni-v0ni')./hnv0).*normpdf((v1ni-v1ni')./hnv1).*normpdf((zni-zni')./hnz).*normpdf((xni-xni')./hnx);
pnw1=sum(w1i.*ker)./sum(ker);
pnw1=pnw1';
pnw2=sum(w2i.*ker)./sum(ker);
pnw2=pnw2';
lam1=(pnw1>0.5);
lam2=(pnw2>0.5);
snb=ones(Lc,1);
snbmsc=ones(Lc,1);
for lc=1:Lc
    xzv1   = xn*coeff1(lc)+zn*coeff2(lc)+v1n*coeff3(lc);%%n*1
    xzv0   = xn*coeff1(lc)+zn*coeff2(lc)+v0n*coeff3(lc);
    snb(lc)= (((w1-0.5).*lam1)'*((xzv1+0.2>0)-(xzv1+0.2<=0))+((w1-0.5).*(1-lam1))'*((xzv0+0.2>0)-(xzv0+0.2<=0))...
        +((w2-0.5).*lam2)'*((xzv1+0.2-coeff4(lc)>0)-(xzv1+0.2-coeff4(lc)<=0))...
        +((w2-0.5).*(1-lam2))'*((xzv0+0.2-coeff4(lc)>0)-(xzv0+0.2-coeff4(lc)<=0)))/N;
    xzvm   = xm*coeff1(lc)+zm*coeff2(lc)+vm*coeff3(lc);
    snbmscaux = mean(abs(ym-1-(xzvm+0.2>=0)-(xzvm+0.2>=coeff4(lc))));
    snbmsc(lc)= snbmscaux';
end 
ind  = (snb>=max(snb)-0.1*N^(-2/3)*sqrt(log(log(N))));
coefset= coeff(ind,:);
bu   = max(coeff1(ind));
bl   = min(coeff1(ind));
cu   = max(coeff2(ind));
cl   = min(coeff2(ind));
du   = max(coeff3(ind));
dl   = min(coeff3(ind));
au   = max(coeff4(ind));
al   = min(coeff4(ind));

[s,indmsc]= min(snbmsc);
mscest= coeff(indmsc,:)'
mmsest= [bl,bu;cl,cu;dl,du;al,au]

%---------------------bootstrap----------------%

nsind=randi(N,N,100);
yr=yn(nsind);xr=xn(nsind); zr=zn(nsind);v0r=v0n(nsind);v1r=v1n(nsind);vr=vn(nsind);
% ymr=[ym,ym(nsind)];xmr=[xm,xm(nsind)];zmr=[zm,zm(nsind)];vmr=[vm,vm(nsind)];
REP=100;
snbr=ones(Lc,1);
snbmscr=ones(Lc,1);
bru=ones(REP,1);    brl=ones(REP,1);        cru=ones(REP,1);          crl=ones(REP,1);           dru=ones(REP,1);        drl=ones(REP,1);       aru=ones(REP,1);       arl=ones(REP,1);
bstarr=ones(REP,1); cstarr=coeff2(REP,1);   dstarr=coeff3(REP,1);     astarr=coeff4(REP,1);
wr1=(yr>1);
wr2=(yr==3);
save ('jobsatresampleing_20200606.mat','-v7.3')
for r=1:REP   
    xnr=repmat(xr(:,r),1,N);
    znr=repmat(zr(:,r),1,N);
    v0nr=repmat(v0r(:,r),1,N);
    v1nr=repmat(v1r(:,r),1,N);
    w1nr=repmat(wr1(:,r),1,N);
    w2nr=repmat(wr2(:,r),1,N);
    hnv0r=1.06*std(v0r(:,r))*N^(-1/5);
    hnv1r=1.06*std(v1r(:,r))*N^(-1/5);
    hnzr=1.06*std(zr(:,r))*N^(-1/5);
    hnxr=1.06*std(xr(:,r))*N^(-1/5);
    kerr=normpdf((v0nr-v0nr')./hnv0r).*normpdf((v1nr-v1nr')./ hnv1r).*normpdf((znr-znr')./ hnzr).*normpdf((xnr-xnr')./ hnxr);
    pnw1r=sum(w1nr.*kerr)./sum(kerr);
    pnw1r=pnw1r';
    pnw2r=sum(w2nr.*kerr)./sum(kerr);
    pnw2r=pnw2r';
    lam1r=(pnw1r>0.5);
    lam2r=(pnw2r>0.5);

    datamscrs=[yr(:,r) xr(:,r) zr(:,r) vr(:,r)];
    [rowmscrs,colmscrs]=find(isnan(yr(:,r))==1|isnan(xr(:,r))==1|isnan(zr(:,r))==1|isnan(vr(:,r))==1);
    datamscrs(rowmscrs,:)=[];
    yrs=datamscrs(:,1); xrs=datamscrs(:,2); zrs=datamscrs(:,3); vrs=datamscrs(:,4); 

for lc=1:Lc
    xzv1r   = xr(:,r)*coeff1(lc)+zr(:,r)*coeff2(lc)+v1r(:,r)*coeff3(lc);%%n*1
    xzv0r   = xr(:,r)*coeff1(lc)+zr(:,r)*coeff2(lc)+v0r(:,r)*coeff3(lc);

    snbr(lc)= (((wr1(:,r)-0.5).*lam1r)'*((xzv1r+0.2>0)-(xzv1r+0.2<=0))+((wr1(:,r)-0.5).*(1-lam1r))'*((xzv0r+0.2>0)-(xzv0r+0.2<=0))...
        +((wr2(:,r)-0.5).*lam2r)'*((xzv1r+0.2-coeff4(lc)>0)-(xzv1r+0.2-coeff4(lc)<=0))...
        +((wr2(:,r)-0.5).*(1-lam2r))'*((xzv0r+0.2-coeff4(lc)>0)-(xzv0r+0.2-coeff4(lc)<=0)))/N;
    xzvrs     = xrs*coeff1(lc)+zrs*coeff2(lc)+vrs*coeff3(lc);
    snbmscraux = mean(abs(yrs-1-(xzvrs+0.2>=0)-(xzvrs+0.2>=coeff4(lc))));
    snbmscr(lc)= snbmscraux';
end 
    indr=(snbr>=max(snbr)-0.1*log(N)/N);
coefrset= coeff(indr,:);
bru(r) = max(coeff1(indr));
brl(r) = min(coeff1(indr));
cru(r) = max(coeff2(indr));
crl(r) = min(coeff2(indr));
dru(r) = max(coeff3(indr));
drl(r) = min(coeff3(indr));
aru(r) = max(coeff4(indr));
arl(r) = min(coeff4(indr));
    
[minsbbr,indmscr]=min(snbmscr); 
bstarr(r)=coeff1(indmscr);
cstarr(r)=coeff2(indmscr);
dstarr(r)=coeff3(indmscr);
astarr(r)=coeff4(indmscr);
end
save ('jobsatresampleing_20200606.mat','-v7.3')
b05=quantile(brl,0.05);
b95=quantile(bru,0.95);
c05=quantile(crl,0.05);
c95=quantile(cru,0.95);
d05=quantile(drl,0.05);
d95=quantile(dru,0.95);
a05=quantile(arl,0.05);
a95=quantile(aru,0.95);

bmsc05=quantile(bstarr,0.05);
bmsc95=quantile(bstarr,0.95);
cmsc05=quantile(cstarr,0.05);
cmsc95=quantile(cstarr,0.95);
dmsc05=quantile(dstarr,0.05);
dmsc95=quantile(dstarr,0.95);
amsc05=quantile(astarr,0.05);
amsc95=quantile(astarr,0.95);

solutionall=[bl,bu,b05,b95;cl,cu,c05,c95;dl,du,d05,d95;al,au,a05,a95]
solution95=[bmsc05,bmsc95;cmsc05,cmsc95;dmsc05,dmsc95;amsc05,amsc95]

% calculate the elapsed time up to now
time_spent = toc(timestart); % one tic can give many toc with this

fprintf(1,'\n Simulation Time since Start: %14.4g Mins\n',time_spent/60);

clock_append=clock; % the time that the loop starts
disp('===============================================================================');
