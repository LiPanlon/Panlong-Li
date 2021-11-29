%% individual PET netork
%% individual PET network method
f1='G:\hc.csv';
f2='G:\mci.csv';
f3='G:\ad.csv';
HC1=csvread(f1);
MCI1=csvread(f2);
AD1=csvread(f3);
HCmatrix=HC1;
MCImatrix=MCI1;
ADmatrix=AD1;
Xnc=mean(HCmatrix);
si=std(HCmatrix);
Mnc=corrcoef(HCmatrix);
for i=1:30
    for j=1:30
        sp(i,j)=sqrt((si(i).*si(i)+si(j).*si(j))./2);
        HCESD(:,i,j)=abs((HCmatrix(:,i)-Xnc(i))-(HCmatrix(:,j)-Xnc(j)))./sp(i,j);
        MCIESD(:,i,j)=abs((MCImatrix(:,i)-Xnc(i))-(MCImatrix(:,j)-Xnc(j)))./sp(i,j);
        ADESD(:,i,j)=abs((ADmatrix(:,i)-Xnc(i))-(ADmatrix(:,j)-Xnc(j)))./sp(i,j);
        HCR(:,i,j)=(exp(2*HCESD(:,i,j))-1)./(exp(2*HCESD(:,i,j))+1);
        MCIR(:,i,j)=(exp(2*MCIESD(:,i,j))-1)./(exp(2*MCIESD(:,i,j))+1);
        ADR(:,i,j)=(exp(2*ADESD(:,i,j))-1)./(exp(2*ADESD(:,i,j))+1);
         HCM(:,i,j)=(1-HCR(:,i,j)).*Mnc(i,j);
         MCIM(:,i,j)=(1-MCIR(:,i,j)).*Mnc(i,j);
        ADM(:,i,j)=(1-ADR(:,i,j)).*Mnc(i,j);
    end
end
n1=size(HCM);
n2=size(MCIM);
n3=size(ADM);
for i=1:n1(1)
   num=num2str(i);
   file1=strcat('F:\Data\ZhouYun\results\individual network\quchuchongfu1\HC\HCFC',num,'.txt');
   matrix(1:30,1:30)=HCM(i,:,:);
  for j=1:30
      matrix(j,j)=0;
  end
   save(file1,'matrix','-ascii');
%    clear matrix;
end
for i=1:n2(1)
   num=num2str(i);
   file1=strcat('F:\Data\ZhouYun\results\individual network\quchuchongfu1\MCI\MCIFC',num,'.txt');
   matrix(1:30,1:30)=MCIM(i,:,:);
     for j=1:30
      matrix(j,j)=0;
  end
   save(file1,'matrix','-ascii');
end
 for i=1:n3(1)
   num=num2str(i);
   file1=strcat('F:\Data\ZhouYun\results\individual network\quchuchongfu1\AD\ADFC',num,'.txt');
   matrix(1:30,1:30)=ADM(i,:,:);
     for j=1:30
      matrix(j,j)=0;
  end
   save(file1,'matrix','-ascii');
 end