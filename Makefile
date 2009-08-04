PACKAGE=rabbitmq-jsonrpc-channel
DEPS=rabbitmq-server erlang-rfc4627
TEST_APPS=rfc4627_jsonrpc rabbit_jsonrpc_channel
START_RABBIT_IN_TESTS=true

include ../include.mk
