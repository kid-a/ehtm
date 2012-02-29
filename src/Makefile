#
# Makefile
#

FIND=find
INCLUDE=../include

EBIN=../ebin/
BEAM=erlc -b beam \
	-pa $(EBIN) \
	-I $(INCLUDE) \
	-o $(EBIN)

#ERLC=erlc

OBJS=\
ehtm_app \
network_sup \
layer_sup \
entry_node \
utils

ERL_OBJS=$(foreach file, $(OBJS), $(EBIN)/$(file).beam)

all: $(ERL_OBJS)

$(EBIN)/%.beam: %.erl
	$(BEAM) $<

clean:
	@(cd $(EBIN); $(RM) -f *.beam)

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;
