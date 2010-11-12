PACKAGE=rabbitmq-jsonrpc-channel
APPNAME=rabbit_jsonrpc_channel
DEPS=rabbitmq-server rabbitmq-erlang-client erlang-rfc4627 rabbitmq-mochiweb rabbitmq-jsonrpc
RUNTIME_DEPS=rabbitmq-mochiweb rabbitmq-jsonrpc
EXTRA_PACKAGE_DIRS=priv
TEST_APPS=crypto inets mochiweb rabbit_mochiweb rfc4627_jsonrpc amqp_client rabbit_jsonrpc rabbit_jsonrpc_channel rabbit_jsonrpc_channel_test
START_RABBIT_IN_TESTS=true

EXAMPLES=rabbitmq-jsonrpc-channel-examples

EXTRA_PACKAGE_ARTIFACTS=$(EXAMPLES).ez

include ../include.mk

$(DIST_DIR)/$(EXAMPLES).ez: $(DIST_DIR)/$(PACKAGE).ez $(TEST_TARGETS)
	mkdir -p $(DIST_DIR)/$(EXAMPLES)
	cp -r $(TEST_EBIN_DIR) $(DIST_DIR)/$(EXAMPLES)/ebin
	mkdir $(DIST_DIR)/$(EXAMPLES)/priv
	cp -r priv/www-examples $(DIST_DIR)/$(EXAMPLES)/priv
	(cd $(DIST_DIR); zip -r $(EXAMPLES).ez $(EXAMPLES))
