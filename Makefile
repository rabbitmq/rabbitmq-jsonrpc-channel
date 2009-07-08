RABBIT_SOURCE_ROOT=..
RABBIT_SERVER_SOURCE_ROOT=$(RABBIT_SOURCE_ROOT)/rabbitmq-server
RABBIT_SERVER_INCLUDE_DIR=$(RABBIT_SERVER_SOURCE_ROOT)/include

PLUGIN_NAME=rabbitmq-http2
PLUGINS_DIR=$(RABBIT_SERVER_SOURCE_ROOT)/plugins

SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES)) ebin/httpd.conf.tmp
ERLC_OPTS=-I $(RABBIT_SERVER_INCLUDE_DIR) -I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall +debug_info # +native -v

SERVER_ROOT=$(CURDIR)/server_root

all: $(EBIN_DIR) $(TARGETS)

$(EBIN_DIR):
	mkdir -p $@

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

ebin/httpd.conf.tmp: server_root/conf/httpd.conf.in
	sed -e "s:@SERVER_ROOT@:$(SERVER_ROOT):g" < $< > $@

clean:
	rm -f ebin/*.beam $(TARGETS)
	rm -f $(SERVER_ROOT)/logs/*_log

package:
	mkdir -p $(SERVER_ROOT)/logs

install: package
	rm -f $(PLUGINS_DIR)/$(PLUGIN_NAME)
	ln -s $(CURDIR) $(PLUGINS_DIR)/$(PLUGIN_NAME)



run: all start_server

start_server:
	mkdir -p $(SERVER_ROOT)/logs
	$(MAKE) -C $(RABBIT_SERVER_SOURCE_ROOT) run \
		RABBITMQ_SERVER_START_ARGS='-pa '"$$(pwd)/$(EBIN_DIR)"' -rabbit \
			rabbit_http_conf \"'"$$(pwd)"'/ebin/httpd.conf.tmp\" \
			extra_startup_steps [{\"HTTP-JSON-listeners\",rabbit_http,kickstart,[]}]'
