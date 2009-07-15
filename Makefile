RABBIT_SOURCE_ROOT=..
RABBIT_SERVER_SOURCE_ROOT=$(RABBIT_SOURCE_ROOT)/rabbitmq-server
RABBIT_SERVER_INCLUDE_DIR=$(RABBIT_SERVER_SOURCE_ROOT)/include
RABBIT_SERVER_PLUGINS_DIR=$(RABBIT_SERVER_SOURCE_ROOT)/plugins

SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
PRIV_DIR=priv
DIST_DIR=dist
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES)) ebin/httpd.conf.tmp
ERLC_OPTS=-I $(RABBIT_SERVER_INCLUDE_DIR) -I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall +debug_info # +native -v

SERVER_ROOT=$(CURDIR)/server_root
DOC_ROOT=$(SERVER_ROOT)/htdocs
PACKAGE=mod_http_channel
PACKAGE_NAME=$(PACKAGE).ez

all: $(EBIN_DIR) $(TARGETS)

$(EBIN_DIR):
	mkdir -p $@

$(EBIN_DIR)/%.beam: $(SOURCE_DIR)/%.erl $(INCLUDES)
	erlc $(ERLC_OPTS) $<

ebin/httpd.conf.tmp: server_root/conf/httpd.conf.in
	sed -e "s:@SERVER_ROOT@:$(SERVER_ROOT):g" < $< > $@

clean:
	rm -f ebin/*.beam $(TARGETS)
	rm -rf $(DIST_DIR)
	rm -f $(SERVER_ROOT)/logs/*_log

run: all start_server

start_server:
	mkdir -p $(SERVER_ROOT)/logs
	$(MAKE) -C $(RABBIT_SERVER_SOURCE_ROOT) run \
		RABBITMQ_SERVER_START_ARGS='-pa '"$$(pwd)/$(EBIN_DIR)"' -rabbit \
			rabbit_http_conf \"'"$$(pwd)"'/ebin/httpd.conf.tmp\" \
			extra_startup_steps [{\"HTTP-JSON-listeners\",rabbit_http,kickstart,[]}]'

$(DIST_DIR):
	mkdir -p $@

$(DIST_DIR)/$(PACKAGE): $(DIST_DIR)
	mkdir -p $@

$(DIST_DIR)/$(PACKAGE)/$(PRIV_DIR): $(DIST_DIR)/$(PACKAGE)
	mkdir -p $@

$(DIST_DIR)/$(PACKAGE)/$(PRIV_DIR)/www: $(DIST_DIR)/$(PACKAGE)/$(PRIV_DIR)
	mkdir -p $@

package: clean $(DIST_DIR)/$(PACKAGE)/$(PRIV_DIR)/www $(TARGETS)
	cp -r $(EBIN_DIR) $(DIST_DIR)/$(PACKAGE)
	cp -r $(DOC_ROOT)/* $(DIST_DIR)/$(PACKAGE)/$(PRIV_DIR)/www
	(cd $(DIST_DIR); zip -r $(PACKAGE_NAME) $(PACKAGE))

install: package
	cp $(DIST_DIR)/$(PACKAGE_NAME) $(RABBIT_SERVER_PLUGINS_DIR)

