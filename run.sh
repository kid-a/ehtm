ERL_PATHS="-pa ebin/ -pa ../erl_img/ebin"

erlc ${ERL_PATHS} ehtm.rel
erl ${ERL_PATHS} -boot ehtm


