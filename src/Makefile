#
# Makefile
#

FIND=find
INCLUDE=../include
IMG_INCLUDE=../../erl_img/include
EUNIT_INCLUDE=$(shell echo /usr/lib/erlang/lib/eunit-*/include)

EBIN=../ebin/

BEAM=erlc -b beam \
	-pa $(EBIN) \
	-I $(INCLUDE) -I $(IMG_INCLUDE) \
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
network \
layer_sup \
node \
entry_node \
intermediate_node \
output_node \
learning \
utils \
test
##examples

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

