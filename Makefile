PACKAGE=rabbitmq-jsonrpc-channel
APPNAME=rabbit_jsonrpc_channel
DEPS=rabbitmq-server rabbitmq-erlang-client erlang-rfc4627 rabbitmq-mochiweb rabbitmq-jsonrpc
RUNTIME_DEPS=rabbitmq-mochiweb rabbitmq-jsonrpc
EXTRA_PACKAGE_DIRS=priv

include ../include.mk
