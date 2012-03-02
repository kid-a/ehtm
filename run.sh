ERL_PATHS="-pa ebin/"

erlc ${ERL_PATHS} ehtm.rel
erl ${ERL_PATHS} -boot ehtm


