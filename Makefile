#
# Makefile
#
FIND=find

SRC=src
EBIN=ebin


all:
	(cd $(SRC); make)

clean:
	(cd $(SRC); make clean)

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

# run:
# 	erl -pa $(EBIN) -run application start ehtm

