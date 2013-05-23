PATHS = ebin \
		apps/querl/ebin \
		apps/frontend/ebin

RELDIR = _rel
RELNAME = querl
DEPS = ebin \
	   deps/*/ebin

all:
	make doit

run:
	cd $(RELDIR)/bin/; ./$(RELNAME) start

stop:
	cd $(RELDIR)/bin/; ./$(RELNAME) stop

attach:
	cd $(RELDIR/bin/; ./$(RELNAME) ping
	cd $(RELDIR)/bin/; ./$(RELNAME) attach

deleter:
	find $(PATHS) -name *.beam -delete

doit:
	make deleter
	rm ebin/*.beam; rebar get-deps compile; relx
