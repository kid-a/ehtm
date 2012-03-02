#
# Makefile
#

FIND=find
INCLUDE=../include
EUNIT_INCLUDE=$(shell echo /usr/lib/erlang/lib/eunit-*/include)

EBIN=../ebin/
IMG_EBIN=../../erl_img/ebin

BEAM=erlc -b beam \
	-pa $(EBIN) \
	-I $(INCLUDE) \
	-o $(EBIN)

TEST=erl -pa $(EBIN) -I $(INCLUDE) -I $(EUNIT_INCLUDE) \
-noshell \
-run entry_node test \
-run intermediate_node test \
-run output_node test \
-run init stop



#ERLC=erlc

OBJS=\
ehtm_app \
network_sup \
layer_sup \
node \
entry_node \
intermediate_node \
output_node

ERL_OBJS=$(foreach file, $(OBJS), $(EBIN)/$(file).beam)

all: $(ERL_OBJS)

$(EBIN)/%.beam: %.erl
	$(BEAM) $<

test: 
	$(TEST) $<

clean:
	@(cd $(EBIN); $(RM) -f *.beam)

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;
