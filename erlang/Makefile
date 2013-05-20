PATHS = ebin \
	apps/querl/ebin \
	apps/frontend/ebin

DEPS = ebin \
	deps/*/ebin

all:
	make doit

run:
	erl \
	-pa $(DEPS) \
	-pa $(PATHS) \
	-boot start_sasl \
	-s querl \
	-s frontend

deleter:
	find $(PATHS) -name *.beam -delete

doit:
	make deleter
	rm ebin/*.beam; rebar get-deps compile
