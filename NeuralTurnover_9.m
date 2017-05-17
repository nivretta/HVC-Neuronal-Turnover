dt = 1;
tFinal = 75;
t = 0:dt:tFinal; %an array of t values

b = zeros(1,length(t)-1); %an array of "breeding season" values
progenitors = zeros(1,length(t)-1); %new stem cells & neural progenitor cells in VZ (express nestin)
migrators = zeros(1,length(t)-1); %new daughters of neural progenitor cells that have begun to migrate to HVC (express doublecortin)
newNeuronHVC = zeros(1,length(t)-1); %proportion of migrators that successfully get to HVC (express NeuN)
neuronNumHVC = (200000:dt:(tFinal+200000)); %total number of neurons in HVC
apoptosisHVC = zeros(1,length(t)-1); %death in HVC
proliferationVZ = zeros(1,length(t)-1); %proliferation in VZ
migrationSuccess = zeros(1,length(t)-1);
apoptosis = zeros(1,length(t)-1);
preserveHVC = zeros(1,length(t)-1);

minVZ = 7800; %minimum number of newly proliferated cells in VZ is during LD, taken from Reg I data
migrationDuration = 10; %days
changeDuration = 2; %days it takes to change from stem cell to migrator that expresses doublecortin
incorporationSuccess = 0.5; %success of migrated neurons' incorporation


for n = 1:(length(t)-1);

    if t(n) < 15;
        b(n) = 1;
    elseif t(n) >=15 && t(n) <= 45;
        b(n) = 0;
    else
        b(n) = 1;
    end

    if b(n) == 0 && t(n) >=16 && t(n) <=25
        proliferationVZ(n) = 1.5; %rate of VZ prolif during LD --> SD
    elseif b(n) == 1 && t(n) >=46 && t(n) <=56
        proliferationVZ(n) = 1.5; %rate of VZ prolif during SD --> LD
    else
        proliferationVZ(n) = 1; %rate of VZ prolif during stable state
    end
    
    progenitors(n) = minVZ * proliferationVZ(n);

    if b(n) == 1;
        migrators(n) = progenitors(n) * 0.5; %neural progenitor daughters that migrate out during breeding condition
    elseif b(n) == 0;
        migrators(n) = progenitors(n) * 0.5;
    end
    
    if b(n) == 1;
        migrationSuccess(n) = 0.5; 
    elseif b(n) == 0;
        migrationSuccess(n) = 0.47;
    end
    
    newNeuronHVC(n) = migrators(n) * migrationSuccess(n); 

    if b(n) == 0 && t(n) >=16 && t(n) <=25
        apoptosisHVC(n) = .0125; %apoptosis rate during LD-->SD   
    elseif b(n) == 1 && t(n) >=46 && t(n) <=56
        %apoptosis rate during SD-->LD, less due to effect of testosterone
        apoptosisHVC(n) = .0025; 
    else
        apoptosisHVC(n) = .0049; %apoptosis rate during stable state
    end
    
    %if b(n) == 0 && t(n) >=16 && t(n) <=25
    %    preserveHVC(n) = 0; %T does not save neurons from death during LD-->SD   
    %elseif b(n) == 1 && t(n) >=46 && t(n) <=56
    %    preserveHVC(n) = .01; %T "saves" mature neurons in HVC from death during SD --> LD 
    %else
   %     preserveHVC(n) = 0; %during stable state
   % end
    
    neuronNumHVC(n+1) = neuronNumHVC(n) + (newNeuronHVC(n) * incorporationSuccess) - (apoptosisHVC(n) * neuronNumHVC(n));
    
    
    apoptosis(n) = apoptosisHVC(n) * neuronNumHVC(n);
end

%subplot(5,1,1); plot(b); hold on
%subplot(5,1,2); 
figure;
plot (progenitors)
ylabel('Number of Progenitor Cells in VZ')
xlabel('0-15:LD                     16-46:SD                      46-75: LD')
%subplot(5,1,3); 

%subplot(5,1,4); 
figure(2);
plot (newNeuronHVC) %;hold on
ylabel('HVC New Neuron Number')
xlabel('0-15:LD                     16-46:SD                      46-75: LD')

%subplot(5,1,5); 
figure(3);
plot (neuronNumHVC) %;hold on
ylabel('HVC Total Neuron Number')
xlabel('0-15:LD                     16-46:SD                      46-75: LD') 

figure(4);
plot (apoptosis)
ylabel('Density of Cell Death in HVC')
xlabel('0-15:LD                     16-46:SD                      46-75: LD')