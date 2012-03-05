#
# Makefile
#
FIND=find

SRC=src
EBIN=ebin


all:
	(cd $(SRC); make)

test:
	(cd $(SRC); make test)

clean:
	(cd $(SRC); make clean)

clearbak:
	@$(FIND) . -type f -name \*~ -exec rm {} \;

run:
	./run.sh

