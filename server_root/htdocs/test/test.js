function testMain() {
    log("test_main");

    var channelFactory = new JsonRpcService("/rpc/rabbitmq", handle_service_ready,
					    {debug: true,
					     debugLogger: log,
					     timeout: 30000});
    var channel;

    // service gateway ready
    function handle_service_ready() {
        log("open");
	channel = new RabbitChannel(channelFactory, handle_channel_ready,
				    { debug: true,
				      debugLogger: log,
				      timeout: 5 });
     }

    function handle_channel_ready() {
        log("handle_channel_ready");
        channel.accessRequest("/data")
	.addCallback(function (ticket)
	{
	    queue1 = "test-queue-1a";
	    queue2 = "test-queue-1b";

	    tag1 = "aa-cons-tag1";

	    msg1 = "hello, world";
	    msg2 = "hello, world, again! pub 2";

	    channel.queueDeclare(ticket, queue1)
	    .addCallback(function(reply)
	    {
		log({q1: reply});
		channel.basicConsume(ticket, queue1,
				     {
					 consumeOk: function(tag) {
					     log({consumeOk: tag});
					     this.tag = tag;
					 },
					 deliver: function(delivery) {
					     log({delivery: delivery});
					     channel.basicAck(delivery.delivery_tag);
					     channel.basicCancel(this.tag);
					 },
					 cancelOk: function(tag) {
					     log({cancelOk: tag});
					 }
				     },
				     { consumer_tag: tag1 })
		.addCallback(function () {
				 channel.basicPublish(ticket, "", queue1, msg1);
			     });
	    });

            channel.queueDeclare(ticket, queue2)
            .addCallback(function(reply)
	    {
                log({q2: reply});
		channel.basicConsume(ticket, queue2,
				     {
					 consumeOk: function(tag) {
					     this.tag = tag;
					 },
					 deliver: function(delivery) {
					     log({delivery2: delivery});
					     channel.basicAck(delivery.delivery_tag);
					     channel.basicCancel(this.tag)
					     .addCallback(reopen);
					 }
				     })
		.addCallback(function () {
				 channel.basicPublish(ticket, "", queue2, msg2,
						      {reply_to: "something22"});
			     });
            });
        });
    }

    function reopen() {
        channel.close();
        channel = new RabbitChannel(channelFactory, test_cancel,
				    { debug: true,
				      debugLogger: log,
				      timeout: 6 });
    }

    function test_cancel(channel) {
        log("test basic.cancel compliance");
        channel.accessRequest("/data")
	.addCallback(function (ticket)
	{
	    log({access_request: ticket});
            queue = "test-queue-4";
            ctag = "my-consumer";
	    channel.queueDeclare(ticket, queue, false, false, true)
	    .addCallback(function ()
	    {
		log("queue declare OK");
		channel.basicConsume(ticket, queue,
				     {
					 deliver: function(delivery) {
					     log({delivery4: delivery});
					     channel.basicCancel("this-never-existed")
					     .addCallback(function (x) {
							      log({"never existed": x});
							  });
					     channel.basicCancel(ctag)
					     .addCallback(function (x) {
							      log({cancelled: x});
							      channel.basicPublish(ticket,
										   "", queue,
										   "Two");
							  });
					 }
				     },
				     { consumer_tag: ctag,
				       no_ack: true })
		.addCallback(function () {
				 channel.basicPublish(ticket, "", queue, "One");
			     });
	    });
	});
    }
}

function log() {
    $A(arguments).each(function (arg) {
			   if (typeof(arg) == 'string') {
			       $("testOutput").appendChild(document.createTextNode(arg + "\n"));
			   } else {
			       $("testOutput").appendChild(document
							   .createTextNode(JSON.stringify(arg) +
									   "\n"));
			   }
		       });
}
