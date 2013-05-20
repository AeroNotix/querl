PATHS = ebin \
	apps/querl/ebin

all:
	make doit

run:
	erl -pa $(PATHS) \
	-boot start_sasl \
	-s querl \

deleter:
	find $(PATHS) -name *.beam -delete

doit:
	make deleter
	rm ebin/*.beam; rm ebin/*.app; rebar get-deps compile
