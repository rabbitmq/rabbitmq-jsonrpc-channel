SOURCE_DIR=src
EBIN_DIR=ebin
INCLUDE_DIR=include
INCLUDES=$(wildcard $(INCLUDE_DIR)/*.hrl)
SOURCES=$(wildcard $(SOURCE_DIR)/*.erl)
TARGETS=$(patsubst $(SOURCE_DIR)/%.erl, $(EBIN_DIR)/%.beam,$(SOURCES)) ebin/httpd.conf.tmp
ERLC_OPTS=-I $(INCLUDE_DIR) -o $(EBIN_DIR) -Wall +debug_info # +native -v

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

run: all start_server

RABBIT_SOURCE_ROOT=../AMQ
start_server:
	mkdir -p $(SERVER_ROOT)/logs
	make -C $(RABBIT_SOURCE_ROOT)/erlang/rabbit run \
		RABBIT_ARGS='-pa '"$$(pwd)/$(EBIN_DIR)"' -rabbit \
			rabbit_http_conf \"'"$$(pwd)"'/ebin/httpd.conf.tmp\" \
			extra_startup_steps [{\"HTTP-JSON-listeners\",rabbit_http,kickstart,[]}]'
