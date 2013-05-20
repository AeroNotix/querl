all:
	rm ebin/*.beam; rebar get-deps compile

run:
	erl -pa ebin \
	-boot start_sasl \
	-s querl
