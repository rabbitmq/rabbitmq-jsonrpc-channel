PACKAGE=rabbitmq-jsonrpc-channel
DEPS=rabbitmq-server rabbitmq-erlang-client erlang-rfc4627 rabbitmq-mochiweb rabbitmq-jsonrpc
RUNTIME_DEPS=rabbitmq-mochiweb rabbitmq-jsonrpc
EXTRA_PACKAGE_DIRS=priv
TEST_APPS=mochiweb rabbit_mochiweb rfc4627_jsonrpc rabbit_jsonrpc rabbit_jsonrpc_channel rabbit_jsonrpc_channel_test
START_RABBIT_IN_TESTS=true

include ../include.mk
