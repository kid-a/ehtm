%%
%% test.erl
%%

-module (test).

-export ([go/0]).


data () ->
    {<<1,2,3,4,
       5,6,7,8,
       9,10,11,12,
       13,14,15,16>>,
     InputSize = 16,
     BitDepth = 8}.
     

go () ->
    Network = network:make_process_name ("MyNetwork"),
    network:feed (Network, data ()).
