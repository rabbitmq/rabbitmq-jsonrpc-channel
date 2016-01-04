# RabbitMQ JSON RPC Plugin

This plugin is **deprecated** and **is not under active development**. Team RabbitMQ **recommends
against** its use.

## Binary Builds for RabbitMQ 3.6.0

None are provided by the RabbitMQ team (see above).

## Binary Builds for RabbitMQ 3.5.x

Available from the [Community Plugins archive](http://www.rabbitmq.com/community-plugins/),
as well as its dependencies, [rabbitmq_jsonrpc](http://www.rabbitmq.com/community-plugins/v3.5.x/rabbitmq_jsonrpc-3.5.x-c34cb475.ez) and [erlang-rfc4627](http://www.rabbitmq.com/community-plugins/v3.5.x/rfc4627_jsonrpc-3.5.x-git5e67120.ez).

## Building From Source

You can also compile and install it like any other plugin (see
http://www.rabbitmq.com/plugin-development.html).

To install and activate the RabbitMQ JSON-RPC channel plugin, copy all
the .ez packages *except rabbit_common* from the plugin's 'dist'
directory into the RabbitMQ server's plugin directory, use the command
at the top to enable rabbitmq_jsonrpc_channel, and restart rabbit.

Once the server starts successfully, you should be able to point your
web browser at [http://localhost:15670/rpc/rabbitmq](http://localhost:15670/rpc/rabbitmq)

and get an error along the lines of

```
{"version":"1.1","id":null,"error":{"name":"JSONRPCError","code":404,"message":"Procedure not found","error":["http://localhost:15670/rpc/rabbitmq",""]}}
```

which confirms that the RabbitMQ JSON-RPC channel is ready to accept
requests.


The plugin ships with some examples. To try these, ensure no existing
RabbitMQ broker is running and then type 'make run' in the
rabbitmq-jsonrpc-channel directory. This will start a RabbitMQ broker
with the examples configured. You should be able to point your web
browser at [http://localhost:15670/](http://localhost:15670/)
and get a webpage. Clicking on "Simple JSONRPC test" will run a small
test application. Successful output of http://localhost:15670/test/index.html
at the time of writing is:

```
  test_main
  {"installGenericProxy":{"name":"open","idempotent":false,"params":[{"name":"username","type":"str"},{"name":"password","type":"str"},{"name":"sessionTimeout","type":"num"},{"name":"virtualHost","type":"str"}]}}
  open
  {"installGenericProxy":{"name":"poll","idempotent":false,"params":[]}}
  {"installGenericProxy":{"name":"close","idempotent":false,"params":[]}}
  {"installGenericProxy":{"name":"call","idempotent":false,"params":[{"name":"method","type":"str"},{"name":"args","type":"arr"}]}}
  {"installGenericProxy":{"name":"cast","idempotent":false,"params":[{"name":"method","type":"str"},{"name":"args","type":"arr"},{"name":"content","type":"str"},{"name":"props","type":"arr"}]}}
  handle_channel_ready
  {"q1":"test-queue-1a"}
  {"q2":"test-queue-1b"}
  {"consumeOk":"aa-cons-tag1"}
  {"delivery":{"content":"hello, world","delivery_tag":1,"redelivered":false,"exchange":"","routing_key":"test-queue-1a","props":{"content_type":null,"content_encoding":null,"headers":null,"delivery_mode":null,"priority":null,"correlation_id":null,"reply_to":null,"expiration":null,"message_id":null,"timestamp":null,"type":null,"user_id":null,"app_id":null,"cluster_id":null}}}
  {"cancelOk":"aa-cons-tag1"}
  {"delivery2":{"content":"hello, world, again! pub 2","delivery_tag":2,"redelivered":false,"exchange":"","routing_key":"test-queue-1b","props":{"content_type":null,"content_encoding":null,"headers":null,"delivery_mode":null,"priority":null,"correlation_id":null,"reply_to":"something22","expiration":null,"message_id":null,"timestamp":null,"type":null,"user_id":null,"app_id":null,"cluster_id":null}}}
  {"installGenericProxy":{"name":"poll","idempotent":false,"params":[]}}
  {"installGenericProxy":{"name":"close","idempotent":false,"params":[]}}
  {"installGenericProxy":{"name":"call","idempotent":false,"params":[{"name":"method","type":"str"},{"name":"args","type":"arr"}]}}
  {"installGenericProxy":{"name":"cast","idempotent":false,"params":[{"name":"method","type":"str"},{"name":"args","type":"arr"},{"name":"content","type":"str"},{"name":"props","type":"arr"}]}}
  test basic.cancel compliance
  queue declare OK
  {"delivery4":{"content":"One","delivery_tag":1,"redelivered":false,"exchange":"","routing_key":"test-queue-4","props":{"content_type":null,"content_encoding":null,"headers":null,"delivery_mode":null,"priority":null,"correlation_id":null,"reply_to":null,"expiration":null,"message_id":null,"timestamp":null,"type":null,"user_id":null,"app_id":null,"cluster_id":null}}}
  {"never existed":"this-never-existed"}
  {"cancelled":"my-consumer"}
```

The source to the test program is in

 * priv/www-examples/test/test.js and
 * priv/www-examples/test/index.html
