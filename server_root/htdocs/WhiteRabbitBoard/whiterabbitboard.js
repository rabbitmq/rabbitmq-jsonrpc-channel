function genguid() {
    return "g" + Math.floor(Math.random() * 1e10);
}

var sendCanvasPainterEvent;
var listenToCanvasPainterEvents;
var rabbitReadyCallback = null;

function setupWhiteRabbitBoard() {
    log("Starting.");

    var rabbitService = new JsonRpcService("/rpc/rabbitmq", onRabbitServiceReady,
					   {debug: true,
					    debugLogger: log,
					    timeout: 30000});
    var channel;
    var ticket;
    var queueName;
    var exchangeName = "canvasPainter";

    function onRabbitServiceReady() {
	log("onRabbitServiceReady");
	channel = new RabbitChannel(rabbitService, on_open,
				    { debug: true,
				      debugLogger: log });
    }

    function on_open() {
	log("on_open");
	channel.accessRequest("/data").addCallback(on_ticket);
    }

    function on_ticket(newTicket) {
	log("on_ticket");
	ticket = newTicket;
	channel.exchangeDeclare(ticket, exchangeName, "fanout")
	.addCallback(on_exchange_declared);
    }

    function on_exchange_declared() {
	log("on_exchange_declared");
	channel.queueDeclare(ticket).addCallback(on_queue_declared);
    }

    function on_queue_declared(newQueueName) {
	log("on_queue_declared");
	queueName = newQueueName;
	channel.queueBind(ticket, queueName, exchangeName).addCallback(on_queue_bound);
    }

    function on_queue_bound() {
	log("on_queue_bound");
	sendCanvasPainterEvent = function (event) {
	    channel.basicPublish(ticket, exchangeName, "", JSON.stringify(event));
	};
	listenToCanvasPainterEvents = function (callback) {
	    channel.basicConsume(ticket, queueName,
				 {
				     consumeOk: function(tag) {
					 log({consumeOk: tag});
				     },
				     deliver: function(delivery) {
					 //log({delivery: delivery});
					 var parsedMessage = JSON.parse(delivery.content);
					 //log({parsedMessage: parsedMessage});
					 callback(parsedMessage);
				     }
				 },
				 { no_ack: true });
	};
	rabbitReadyCallback();
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
