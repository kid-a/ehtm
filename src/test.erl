%%
%% test.erl
%%

-module (test).

-export ([go/0,
	  erl_img_test/0
	 ]).

-include ("erl_img.hrl").


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

erl_img_test () ->
    {ok, Image} = erl_img:load ("/home/loris/101_ObjectCategories/accordion/image_0001.jpg"),
    PixMap = Image#erl_image.pixmaps,
    io:format ("Image is ~p~n", [Image]),
    io:format ("Pixmap is ~p~n", [PixMap]),
    

    {ok, Image1} = erl_img:load ("/home/loris/101_ObjectCategories/accordion/image_0002.jpg"),
    PixMap1 = Image1#erl_image.pixmaps,
    io:format ("Image is ~p~n", [Image1]),
    io:format ("Pixmap is ~p~n", [PixMap1]),
    
    {ok, Image2} = erl_img:load ("/home/loris/Pictures/2012/01/26/DSC_6583.JPG"),
    PixMap2 = Image2#erl_image.pixmaps,
    io:format ("Image is ~p~n", [Image2]),
    io:format ("Pixmap is ~p~n", [PixMap2]).
    
    
    
    
    
