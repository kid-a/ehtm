%% -*- mode: octave -*-
disp("Starting tests...\n");
tic

%%
%% density over coincidences 
%%
TOLERANCE = 0.000000001;

disp("Testing dens_over_coinc with level=entry...\n");
C = [[2 3 0]; [255 8 9]];
level = "entry";
I = [4 5 6];

Y = dens_over_coinc(C, I, level);
abs(Y(1) - exp(-44)) < TOLERANCE
abs(Y(2) - exp(-63019)) < TOLERANCE

disp("Testing dens_over_coinc with level=intermediate...\n");
C = [[2 3 1]; [2 1 1]];
level = "intermediate";
I = {[0.10 0.20] [0.30 0.40 0.50] [0.60]};
Y = dens_over_coinc(C, I, level);

abs(Y(1) - 0.06) < TOLERANCE
abs(Y(2) - 0.036) < TOLERANCE


%%
%% density over temporal groups
%%
disp("Testing dens_over_groups... \n");
Y = [1.0 2.0 3.0 4.0];
PCG = [[1.0 2.0 3.0 4.0]', [5.0 6.0 7.0 8.0]'];

Z = dens_over_groups (Y, PCG);
abs(Z(1) - 30) < TOLERANCE
abs(Z(2) - 70) < TOLERANCE


%%
%%  density over classes
%%
disp("Testing dens_over_classes... \n");
Y = [1.0 2.0 3.0 4.0];
PCW = [[1.0 2.0 3.0 4.0]', [5.0 6.0 7.0 8.0]'];

Z = dens_over_classes (Y, PCG);
abs(Z(1) - 30) < TOLERANCE
abs(Z(2) - 70) < TOLERANCE


%%
%%  class posterior probability
%%
disp("Testing class_post_prob... \n");
Z = [0.4 0.5];
CLASS_PRIOR_PROB = [0.2 0.8];

P = class_post_prob (Z, CLASS_PRIOR_PROB)
abs(P(1) - 0.08 / 0.48) < TOLERANCE
abs(P(2) - 0.40 / 0.48) < TOLERANCE


entry_layer = make_layer("entry", [10 10]);
intermediate_layer = make_layer("intermediate", [5 5]);
output_layer = make_layer("output");
sample_input = ones(50,50);


%%
%%  expose
%%  !FIXME makes no sense with a matrix of ones
# disp("Testing expose on entry node... \n");
# entry_layer = do_expose(entry_layer, sample_input);
# for i = 1 : 10
#   for j = 1 : 10
#     prod(entry_layer{i,j}.in_msg == ones(1, 25))
#   endfor
# endfor


%%
%%  temporal clustering
%% 
%% make a Temporal Activation Matrix
TAM = normalize_over_rows ( rand ( 10 ) )

%% make a Temporal Connection Vector
TC = normalize_over_rows ( rand ( 1, 10 ) )

%% do temporal clustering
Groups = do_temporal_clustering (TC, TAM)

toc