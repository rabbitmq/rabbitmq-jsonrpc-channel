RABBIT_SOURCE_ROOT=..
RABBIT_SERVER_SOURCE_ROOT=$(RABBIT_SOURCE_ROOT)/rabbitmq-server
RABBIT_SERVER_INCLUDE_DIR=$(RABBIT_SERVER_SOURCE_ROOT)/include

PLUGIN_NAME=rabbit_http
PLUGINS_DIR=$(RABBIT_SERVER_SOURCE_ROOT)/plugins
PLUGINS_LIB_DIR=$(PLUGINS_DIR)/lib

ZIP_NAME=$(PLUGIN_NAME)
EZ_NAME=$(ZIP_NAME).ez

HG_OPENSOURCE=http://hg.opensource.lshift.net
JSON_LIB=erlang-rfc4627

SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
DIST_DIR=dist
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

deps:
	hg clone $(HG_OPENSOURCE)/$(JSON_LIB) $(PLUGINS_LIB_DIR)/$(JSON_LIB)
	$(MAKE) -C $(PLUGINS_LIB_DIR)/$(JSON_LIB)

package:
	mkdir -p $(SERVER_ROOT)/logs

distclean:
	rm -rf $(DIST_DIR)

dist: package
	mkdir -p $(DIST_DIR)
	mkdir -p $(DIST_DIR)/$(ZIP_NAME)
	cp -r $(EBIN_DIR) *.cfg *.plugin $(SERVER_ROOT) $(DIST_DIR)/$(ZIP_NAME)
	(cd dist; zip -r $(EZ_NAME) *)

install: dist
	cp $(DIST_DIR)/$(EZ_NAME) $(PLUGINS_DIR)


run: all start_server

start_server:
	mkdir -p $(SERVER_ROOT)/logs
	$(MAKE) -C $(RABBIT_SERVER_SOURCE_ROOT) run \
		RABBITMQ_SERVER_START_ARGS='-pa '"$$(pwd)/$(EBIN_DIR)"' -rabbit \
			rabbit_http_conf \"'"$$(pwd)"'/ebin/httpd.conf.tmp\" \
			extra_startup_steps [{\"HTTP-JSON-listeners\",rabbit_http,kickstart,[]}]'
