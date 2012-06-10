default: fast

all:
	@rebar get-deps
	@rebar compile

fast:
	@rebar compile skip_deps=true

clean:
	@rebar clean

clean_database:
	@rm -rf database