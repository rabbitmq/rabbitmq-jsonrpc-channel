var channel;
var queueName = null;

function mkElement(name, cls) {
    var e = document.createElement(name);
    e.className = cls;
    return e;
}

function mkSpan(cls, content) {
    var node = mkElement("span", cls);
    node.appendChild(document.createTextNode(content));
    return node;
}

var mimeTypeHandlers = {
    "text/plain": function(delivery) {
        var utterance = mkElement("div", "utterance");
        utterance.appendChild(mkSpan("nick", delivery.routing_key));
        utterance.appendChild(mkSpan("message", delivery.content));
        $("#chatOutput")[0].appendChild(utterance);
    },

    "application/json": function(delivery) {
	    var parsedMessage = JSON.parse(delivery.content);
    }
};

function initUsername() {
    var username = document.location.search.match(/username=([^&]+)/);
    if (username) {
	username = username[1].replace(/\+/g, " ");
	username = unescape(username);
    }

    if (username) {
	return username;
    } else {
	return "user" + Math.floor(Math.random() * 1e10);
    }
}

function chatMain() {
    log("Starting.");

    $("#userName")[0].value = initUsername();
    $("#chatMessage").focus();

    RabbitMQ.openChannel(function (c) {
			     channel = c;
			     change_channel();
			 },
			 { debug: true,
			   debugLogger: log });
}

function change_channel() {
    log("change_channel: " + $("#channelName")[0].value);
    channel.exchangeDeclare($("#channelName")[0].value, "fanout")
    .addCallback(on_exchange_declared);

    function on_exchange_declared() {
	log("on_exchange_declared");
	if (queueName != null) {
	    channel.queueDelete(queueName)
	    .addCallback(function (messageCount) {
			     log("queue deleted");
			     declare_fresh_queue();
			 });

	    queueName = null;
	} else {
	    declare_fresh_queue();
	}
    }

    function declare_fresh_queue() {
	log("declare_fresh_queue");
	channel.queueDeclare().addCallback(on_queue_declared);
    }

    function on_queue_declared(newQueueName) {
	log("on_queue_declared");
	queueName = newQueueName;
	channel.queueBind(queueName, $("#channelName")[0].value).addCallback(on_queue_bound);
    }

    function on_queue_bound() {
	log("on_queue_bound");
	$("#chatOutput").innerHTML = "";
	channel.basicConsume(queueName,
			     {
				 deliver: function(delivery) {
				     var mimeType = delivery.props.content_type;
				     if (mimeType == null) {
					 mimeType = "text/plain";
				     }
				     var handler = mimeTypeHandlers[mimeType];
				     if (handler != null) {
					 handler(delivery);
				     } else {
					 log({props: delivery.props, unhandled: delivery.content});
				     }
				 }
			     },
			     { no_ack: true });
    }
}

function send_chat() {
    channel.basicPublish($("#channelName")[0].value, $("#userName")[0].value,
			 $("#chatMessage")[0].value,
			 { content_type: "text/plain" });
    $("#chatMessage")[0].value = "";
}

function log() {
    for (var i = 0; i < arguments.length; ++i) {
        var arg = arguments[i];
        if (typeof(arg) == 'string') {
            $("#testOutput").append(arg + "\n");
        } else {
            $("#testOutput").append(JSON.stringify(arg) + "\n");
        }
    }
}
