PACKAGE=mod_http_channel
DEPS=rabbitmq-server mod_http erlang-rfc4627
TEST_APPS=rfc4627_jsonrpc mochiweb mod_http mod_http_channel
START_RABBIT_IN_TESTS=true

include ../include.mk
